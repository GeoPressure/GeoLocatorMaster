# Test a built/downloaded release without credentials or network requests.
# Rscript tests/test_release.R [package-directory] [diagnostic-directory]
args <- commandArgs(trailingOnly = TRUE)
master_dir <- if (length(args)) args[[1]] else "data/master"
stopifnot(file.exists(file.path(master_dir, "datapackage.json")))
audit_dir <- if (length(args) >= 2) args[[2]] else "output/release-audit"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(GLDP_MASTER_DIR = normalizePath(master_dir), GLDP_AUDIT_DIR = normalizePath(audit_dir))
if (dir.exists("../GeoLocatoR")) pkgload::load_all("../GeoLocatoR", quiet = TRUE)
pkgload::load_all(quiet = TRUE)
testthat::set_max_fails(Inf)
results <- testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter$new(max_reports = Inf, show_praise = FALSE), stop_on_failure = FALSE)
results <- as.data.frame(results)
readr::write_csv(results[, setdiff(names(results), "result")], file.path(audit_dir, "test-results.csv"))
if (any(results$failed > 0 | results$error)) stop("Release checks failed; see ", audit_dir, call. = FALSE)
