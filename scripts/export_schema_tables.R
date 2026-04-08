#!/usr/bin/env Rscript

library(tidyverse)
library(flextable)
library(officer)
library(jsonlite)

table_schema_files <- c(
  "tags-table-schema.json",
  "observations-table-schema.json",
  "measurements-table-schema.json",
  "staps-table-schema.json",
  "twilights-table-schema.json",
  "paths-table-schema.json",
  "pressurepaths-table-schema.json",
  "edges-table-schema.json"
)
profile_schema_file <- "geolocator-dp-profile.json"

repo_url <- "https://raw.githubusercontent.com/rafnuss/geolocator-dp/v1.0"
out_dir <- file.path("data", "schema")
docx_path <- file.path(out_dir, "geolocator_schema_tables.docx")
write_csv <- TRUE
write_docx <- TRUE

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

all_files <- c(table_schema_files, profile_schema_file)
entries <- vector("list", length(all_files))

collapse_schema_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  if (is.list(value)) {
    return(paste(unlist(value), collapse = ", "))
  }
  if (length(value) > 1) {
    return(paste(value, collapse = ", "))
  }
  as.character(value)
}

flatten_profile_schema <- function(schema) {
  root <- schema
  if (is.null(root$properties) && !is.null(root$allOf)) {
    for (entry in root$allOf) {
      if (!is.null(entry$properties)) {
        root <- entry
        break
      }
    }
  }

  rows <- list()

  walk <- function(node, path, is_required) {
    if (!is.null(node$allOf)) {
      for (entry in node$allOf) {
        walk(entry, path, is_required)
      }
      return()
    }
    if (!is.null(node$oneOf)) {
      for (entry in node$oneOf) {
        walk(entry, path, is_required)
      }
      return()
    }
    if (!is.null(node$anyOf)) {
      for (entry in node$anyOf) {
        walk(entry, path, is_required)
      }
      return()
    }

    if (path != "") {
      field_type <- node$type
      if (is.null(field_type) && !is.null(node$`$ref`)) {
        field_type <- "$ref"
      }
      rows[[length(rows) + 1]] <<- list(
        name = path,
        title = node$title,
        description = node$description,
        type = collapse_schema_value(field_type),
        required = if (is_required) "TRUE" else "",
        enum = collapse_schema_value(node$enum),
        format = collapse_schema_value(node$format),
        pattern = collapse_schema_value(node$pattern),
        example = collapse_schema_value(node$examples %||% node$example),
        ref = collapse_schema_value(node$`$ref`)
      )
    }

    if (!is.null(node$properties)) {
      required_names <- node$required %||% character()
      prop_names <- names(node$properties)
      for (prop in prop_names) {
        child <- node$properties[[prop]]
        next_path <- if (path == "") prop else paste(path, prop, sep = ".")
        walk(child, next_path, prop %in% required_names)
      }
    }

    if (!is.null(node$items)) {
      item_path <- paste0(path, "[]")
      walk(node$items, item_path, FALSE)
    }
  }

  walk(root, "", FALSE)

  if (length(rows) == 0) {
    return(tibble())
  }

  bind_rows(rows) |>
    mutate(across(everything(), as.character))
}

for (i in seq_along(all_files)) {
  schema_file <- all_files[[i]]
  schema <- read_json(
    paste0(repo_url, "/", schema_file),
    simplifyVector = FALSE
  )

  if (schema_file == profile_schema_file) {
    df <- flatten_profile_schema(schema)
    if (nrow(df) > 0 && "required" %in% names(df) && "name" %in% names(df)) {
      df <- df |>
        mutate(
          name = if_else(
            required == "TRUE" & name != "",
            paste0(name, "*"),
            name
          )
        )
    }
    drop_profile <- names(df) %in%
      c("required", "title", "enum", "ref", "format", "pattern")
    df <- df[, !drop_profile, drop = FALSE]
    entries[[i]] <- list(
      file = schema_file,
      data = df,
      description = schema$description
    )
    next
  }

  fields <- schema$fields

  if (is.null(fields)) {
    props <- schema$properties
    required <- schema$required
    if (is.null(props)) {
      for (entry in schema$allOf) {
        if (!is.null(entry$properties)) {
          props <- entry$properties
          required <- entry$required
          break
        }
      }
    }
    fields <- imap(props, function(field, name) {
      field$name <- name
      field$required <- name %in% required
      field
    })
  }

  rows <- vector("list", length(fields))

  for (j in seq_along(fields)) {
    field <- fields[[j]]
    flat <- list()

    stack <- list(list(prefix = "", value = field))
    while (length(stack) > 0) {
      item <- stack[[length(stack)]]
      stack <- stack[-length(stack)]

      prefix <- item$prefix
      value <- item$value

      if (is.null(value)) {
        flat[[prefix]] <- NA_character_
        next
      }

      if (is.list(value)) {
        if (length(value) == 0) {
          flat[[prefix]] <- NA_character_
          next
        }

        if (is.null(names(value))) {
          if (all(map_lgl(value, is.atomic))) {
            flat[[prefix]] <- paste(unlist(value), collapse = ",")
          } else {
            collapsed <- map_chr(value, function(item) {
              if (is.null(item)) {
                return(NA_character_)
              }
              if (is.list(item)) {
                return(toJSON(item, auto_unbox = TRUE))
              }
              if (length(item) > 1) {
                return(paste(item, collapse = ","))
              }
              as.character(item)
            })
            flat[[prefix]] <- paste(collapsed, collapse = ",")
          }
          next
        }

        names_value <- names(value)
        for (k in seq_along(value)) {
          key <- names_value[[k]]
          full_key <- if (is.null(prefix) || prefix == "") {
            key
          } else {
            paste(prefix, key, sep = "_")
          }
          stack[[length(stack) + 1]] <- list(
            prefix = full_key,
            value = value[[k]]
          )
        }
        next
      }

      if (length(value) > 1) {
        flat[[prefix]] <- paste(value, collapse = ",")
      } else {
        flat[[prefix]] <- as.character(value)
      }
    }

    rows[[j]] <- flat
  }

  df <- bind_rows(rows) |>
    mutate(across(everything(), as.character))

  if ("constraints_required" %in% names(df)) {
    df <- df |>
      mutate(required = if_else(constraints_required == "TRUE", "TRUE", ""))
  }

  if ("required" %in% names(df) && "name" %in% names(df)) {
    df <- df |>
      mutate(
        name = if_else(required == "TRUE" & name != "", paste0(name, "*"), name)
      )
  }

  drop <- grepl("^skos:", names(df), ignore.case = TRUE) |
    grepl("^constraints_", names(df)) |
    grepl("(^|_)unit$", names(df)) |
    names(df) %in% c("unit", "required", "title", "enum")

  df <- df[, !drop, drop = FALSE]

  base_order <- c("name", "description", "type", "example")
  ordered_cols <- c(
    intersect(base_order, names(df)),
    sort(setdiff(names(df), base_order))
  )
  df <- df |>
    select(all_of(ordered_cols))

  entries[[i]] <- list(
    file = schema_file,
    data = df,
    description = schema$description
  )
}

if (FALSE) {
  for (entry in entries) {
    out_file <- sub("\\.json$", ".csv", basename(entry$file))
    out_path <- file.path(out_dir, out_file)
    write.csv(entry$data, out_path, row.names = FALSE, na = "")
  }
}


code_props <- fp_text(font.family = "Courier New")

format_cell <- function(text) {
  text <- replace_na(as.character(text), "")
  text <- str_replace_all(text, "\\r\\n|\\r|\\n", " ")
  text <- str_replace_all(text, "<br\\s*/?>", "\n")
  text <- str_replace_all(
    text,
    "<a\\s+[^>]*href=['\"]([^'\"]+)['\"][^>]*>(.*?)</a>",
    "[\\2](\\1)"
  )
  text <- str_replace_all(text, "<span[^>]*>", "")
  text <- str_replace_all(text, "</span>", "")
  text <- str_replace_all(text, "<[^>]+>", "")
  text <- str_remove_all(text, "Same as\\s+https?://\\S+")
  text <- str_replace_all(text, "\\s+-\\s+", "\n• ")
  if (text == "") {
    return(as_paragraph(""))
  }

  chunks <- list()
  while (nchar(text) > 0) {
    link_loc <- str_locate(text, "\\[[^\\]]+\\]\\([^\\)]+\\)")
    code_loc <- str_locate(text, "`[^`]+`")
    link_start <- ifelse(is.na(link_loc[1]), Inf, link_loc[1])
    code_start <- ifelse(is.na(code_loc[1]), Inf, code_loc[1])
    next_start <- min(link_start, code_start)

    if (is.infinite(next_start)) {
      chunks <- c(chunks, list(as_chunk(text)))
      break
    }

    if (next_start > 1) {
      chunks <- c(chunks, list(as_chunk(substr(text, 1, next_start - 1))))
      text <- substr(text, next_start, nchar(text))
    }

    if (link_start <= code_start) {
      match <- str_match(text, "^\\[([^\\]]+)\\]\\(([^)]+)\\)")
      if (!is.na(match[1, 1])) {
        chunks <- c(
          chunks,
          list(hyperlink_text(match[1, 2], url = match[1, 3]))
        )
        text <- substr(text, nchar(match[1, 1]) + 1, nchar(text))
        next
      }
    }

    match <- str_match(text, "^`([^`]+)`")
    if (!is.na(match[1, 1])) {
      chunks <- c(chunks, list(as_chunk(match[1, 2], props = code_props)))
      text <- substr(text, nchar(match[1, 1]) + 1, nchar(text))
    } else {
      chunks <- c(chunks, list(as_chunk(substr(text, 1, 1))))
      text <- substr(text, 2, nchar(text))
    }
  }

  do.call(as_paragraph, chunks)
}

format_caption_chunks <- function(text) {
  text <- replace_na(as.character(text), "")
  text <- str_replace_all(text, "\\r\\n|\\r|\\n", " ")
  text <- str_replace_all(text, "<br\\s*/?>", " ")
  text <- str_replace_all(text, "<span[^>]*>", "")
  text <- str_replace_all(text, "</span>", "")
  text <- str_replace_all(text, "<[^>]+>", "")
  text <- str_remove_all(text, "\\{:[^}]*\\}")
  text <- str_squish(text)

  if (text == "") {
    return(list(ftext("")))
  }

  chunks <- list()
  while (nchar(text) > 0) {
    link_loc <- str_locate(text, "\\[[^\\]]+\\]\\([^\\)]+\\)")
    link_start <- ifelse(is.na(link_loc[1]), Inf, link_loc[1])

    if (is.infinite(link_start)) {
      chunks <- c(chunks, list(ftext(text)))
      break
    }

    if (link_start > 1) {
      chunks <- c(chunks, list(ftext(substr(text, 1, link_start - 1))))
      text <- substr(text, link_start, nchar(text))
    }

    match <- str_match(text, "^\\[([^\\]]+)\\]\\(([^)]+)\\)")
    if (!is.na(match[1, 1])) {
      chunks <- c(
        chunks,
        list(hyperlink_ftext(match[1, 2], href = match[1, 3]))
      )
      text <- substr(text, nchar(match[1, 1]) + 1, nchar(text))
    } else {
      chunks <- c(chunks, list(ftext(substr(text, 1, 1))))
      text <- substr(text, 2, nchar(text))
    }
  }

  chunks
}

doc <- read_docx()

for (idx in seq_along(entries)) {
  entry <- entries[[idx]]
  df <- entry$data

  if (ncol(df) > 60) {
    id_cols <- intersect(c("name", "title"), names(df))
    df <- df |>
      pivot_longer(
        cols = -all_of(id_cols),
        names_to = "attribute",
        values_to = "value"
      ) |>
      filter(!is.na(value), value != "") |>
      filter(!str_detect(attribute, "skos")) |>
      select(all_of(id_cols), attribute, value)
  }

  description <- replace_na(entry$description, "")
  description <- str_replace_all(description, "\\r\\n|\\r|\\n", " ")
  if (is.null(description) || description == "") {
    description <- basename(entry$file) |>
      tools::file_path_sans_ext() |>
      str_replace("-table-schema$", "") |>
      str_replace("-profile$", "profile") |>
      str_replace_all("-", " ") |>
      str_to_sentence()
  }

  caption_prefix <- paste0("Table ", idx, ". ")
  caption_chunks <- format_caption_chunks(description)
  caption_para <- do.call(fpar, c(list(ftext(caption_prefix)), caption_chunks))

  ft <- flextable(df)
  if (nrow(df) > 0 && ncol(df) > 0) {
    df <- df |>
      mutate(across(everything(), as.character))
    for (col in names(df)) {
      cell_paragraphs <- map(df[[col]], format_cell)
      cell_paragraphs <- structure(
        map(cell_paragraphs, ~ .x[[1]]),
        class = "paragraph"
      )
      ft <- compose(ft, j = col, value = cell_paragraphs)
    }
  }
  ft <- ft |>
    theme_booktabs() |>
    bold(part = "header") |>
    align(align = "left", part = "all") |>
    valign(valign = "top", part = "body") |>
    fontsize(size = 9, part = "all") |>
    padding(padding = 2, part = "all")

  page_width <- 6.5
  ft <- set_table_properties(ft, layout = "fixed")
  if ("description" %in% names(df)) {
    desc_width <- min(3.5, page_width * 0.6)
    other_cols <- setdiff(names(df), "description")
    other_width <- (page_width - desc_width) / max(1, length(other_cols))
    ft <- ft |>
      width(j = "description", width = desc_width) |>
      width(j = other_cols, width = other_width)
  } else {
    ft <- width(ft, j = names(df), width = page_width / max(1, ncol(df)))
  }

  doc <- body_add_fpar(doc, caption_para, style = "Normal")
  doc <- body_add_flextable(doc, ft)
  if (idx < length(entries)) {
    doc <- body_add_par(doc, "", style = "Normal")
  }
}

print(doc, target = docx_path)
