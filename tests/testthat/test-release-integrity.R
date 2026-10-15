test_that("the release identifies its version and public source datasets", {
  skip_without_release()
  expect_true(nzchar(master_descriptor$id))
  expect_true(nzchar(master_descriptor$version))
  expect_identical(master_descriptor$access_status, "open")
  expect_s3_class(master_release$datapackages, "data.frame")
  expect_equal(anyDuplicated(master_release$datapackages$datapackage_id), 0L)
  expect_false(anyNA(master_tags[c("tag_id", "datapackage_id", "scientific_name")]))
  expect_equal(anyDuplicated(master_tags$tag_id), 0L)
  expect_true(all(nzchar(trimws(master_tags$tag_id))))
  included <- dplyr::semi_join(master_release$datapackages, master_tags, by = "datapackage_id")
  expect_true(all(included$access_status == "open"))
  expect_true(all(master_tags$datapackage_id %in% included$datapackage_id))
  expect_true(all(!is.na(included$licenses) & nzchar(included$licenses)))
  expect_true(all(!is.na(included$bibliographicCitation) & nzchar(included$bibliographicCitation)))
})

test_that("every CSV header matches its exported descriptor", {
  skip_without_release()
  for (resource in master_descriptor$resources) {
    header <- names(readr::read_csv(file.path(master_dir, resource$path), n_max = 0, show_col_types = FALSE))
    expect_identical(header, vapply(resource$schema$fields, `[[`, character(1), "name"), info = resource$name)
  }
  for (name in c("datapackages", "species")) {
    csv <- readr::read_csv(file.path(master_dir, paste0(name, ".csv")),
      col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    embedded <- dplyr::bind_rows(lapply(master_descriptor[[name]], function(row) {
      lapply(row, function(value) if (is.null(value)) NA else value)
    }))
    expect_identical(names(csv), names(embedded), info = name)
    for (column in names(csv)) {
      expect_identical(replace(as.character(csv[[column]]), is.na(csv[[column]]), ""),
        replace(as.character(embedded[[column]]), is.na(embedded[[column]]), ""), info = paste(name, column))
    }
  }
})

test_that("resource keys and declared foreign keys support unambiguous joins", {
  skip_without_release()
  for (resource in master_release$resources) {
    data <- resource$data
    key <- unlist(resource$schema$primaryKey)
    if (length(key)) {
      expect_false(anyNA(data[key]), info = resource$name)
      expect_equal(dplyr::n_distinct(data[key]), nrow(data), info = resource$name)
    }
    # Quality diagnostics may describe an orphan identifier in a source table.
    if ("tag_id" %in% names(data) && resource$name != "quality") {
      expect_true(all(stats::na.omit(data$tag_id) %in% master_tags$tag_id), info = resource$name)
    }
    for (fk in resource$schema$foreignKeys) {
      child <- unlist(fk$fields)
      parent <- unlist(fk$reference$fields)
      parent_data <- master_release$resources[[match(fk$reference$resource,
        vapply(master_release$resources, `[[`, character(1), "name"))]]$data
      keys <- unique(data[stats::complete.cases(data[child]), child, drop = FALSE])
      expect_no_rows(dplyr::anti_join(keys, parent_data, by = stats::setNames(parent, child)),
        paste(resource$name, "foreign key to", fk$reference$resource))
    }
  }
})

test_that("coordinate pairs, numeric values and datetimes survive reading", {
  skip_without_release()
  advisory <- list(tags = c("ring_number", "manufacturer", "model"),
    observations = c("ring_number", "datetime", "latitude", "longitude", "age_class", "sex"))
  for (resource in master_release$resources) {
    data <- resource$data
    for (field in resource$schema$fields) {
      if (field$type %in% c("number", "integer")) {
        expect_true(is.numeric(data[[field$name]]), info = paste(resource$name, field$name))
        expect_true(all(is.na(data[[field$name]]) | is.finite(data[[field$name]])), info = paste(resource$name, field$name))
      }
      if (identical(field$type, "datetime")) expect_s3_class(data[[field$name]], "POSIXct")
      if (isTRUE(field$constraints$required) && !field$name %in% advisory[[resource$name]]) {
        expect_false(anyNA(data[[field$name]]), info = paste(resource$name, field$name))
      }
    }
    for (pair in list(c("lat", "lon"), c("latitude", "longitude"), c("lat_s", "lon_s"), c("lat_t", "lon_t"), c("known_lat", "known_lon"))) {
      if (!all(pair %in% names(data))) next
      lat <- data[[pair[1]]]; lon <- data[[pair[2]]]
      expect_identical(is.na(lat), is.na(lon), info = resource$name)
      expect_true(all(is.na(lat) | dplyr::between(lat, -90, 90)), info = resource$name)
      expect_true(all(is.na(lon) | dplyr::between(lon, -180, 180)), info = resource$name)
    }
  }
})

test_that("stationary periods have unique keys and ordered non-overlapping times", {
  skip_without_release()
  expect_equal(anyDuplicated(master_staps[c("tag_id", "stap_id")]), 0L)
  expect_true(all(master_staps$end > master_staps$start))
  ordered <- master_staps |> dplyr::arrange(tag_id, start) |> dplyr::group_by(tag_id) |>
    dplyr::filter(start < dplyr::lag(end))
  expect_no_rows(ordered, "Overlapping stationary periods")
})

test_that("paths and edges retain separate trajectory identities", {
  skip_without_release()
  expect_equal(anyDuplicated(master_paths[c("tag_id", "type", "j", "stap_id")]), 0L)
  expect_equal(anyDuplicated(master_edges[c("tag_id", "type", "j", "stap_s", "stap_t", "n")]), 0L)
  expect_true(all(master_paths$type %in% c("tag", "most_likely", "simulation", "geopressureviz")))
  expect_true(all(master_edges$type %in% c("tag", "most_likely", "simulation", "geopressureviz")))
  expect_true(all(master_paths$j >= 1 & master_paths$j == floor(master_paths$j)))
  expect_true(all(master_edges$j >= 1 & master_edges$j == floor(master_edges$j)))
  for (endpoint in c("s", "t")) {
    keys <- master_edges[c("tag_id", "type", "j", paste0("stap_", endpoint))]
    names(keys)[4] <- "stap_id"
    expect_no_rows(dplyr::anti_join(unique(keys), master_paths, by = c("tag_id", "type", "j", "stap_id")),
      paste("Edge endpoint missing from paths:", endpoint))
  }
})

test_that("edges join to the correct departure and arrival periods", {
  skip_without_release()
  joined <- master_edges |>
    dplyr::left_join(dplyr::select(master_staps, tag_id, stap_s = stap_id, departure = end),
      by = c("tag_id", "stap_s"), relationship = "many-to-one") |>
    dplyr::left_join(dplyr::select(master_staps, tag_id, stap_t = stap_id, arrival = start),
      by = c("tag_id", "stap_t"), relationship = "many-to-one")
  expect_equal(nrow(joined), nrow(master_edges))
  expect_true(all(joined$stap_t > joined$stap_s))
  expect_true(all(joined$end > joined$start))
  expect_true(all(joined$start >= joined$departure & joined$end <= joined$arrival))
  expect_true(all(joined$distance >= 0))
  expect_true(all(joined$n >= 1 & joined$n == floor(joined$n)))
})

test_that("stationary pressure rows join without treating flight fractions as keys", {
  skip_without_release()
  stationary <- master_pressure |> dplyr::filter(stap_id == floor(stap_id)) |>
    dplyr::distinct(tag_id, stap_id)
  expect_no_rows(dplyr::anti_join(stationary, master_staps, by = c("tag_id", "stap_id")),
    "Stationary pressure rows without a stationary period")
  expect_false(anyNA(master_pressure[c("tag_id", "datetime", "type")]))
  simulated <- master_pressure |> dplyr::filter(type == "simulation")
  expect_false(anyNA(simulated$j))
  expect_true(all(simulated$j >= 1))
  expect_equal(dplyr::n_distinct(master_pressure[c("tag_id", "type", "j", "datetime")]), nrow(master_pressure))
  usable <- master_pressure |> dplyr::filter(is.na(label) | label != "discard")
  expect_true(all(is.na(usable$pressure_tag) | usable$pressure_tag > 0))
  expect_true(all(is.na(master_pressure$surface_pressure) | master_pressure$surface_pressure > 0))
})
