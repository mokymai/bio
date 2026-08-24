# Shared safety helpers for automated tests.
# These keep test actions non-destructive and make the intent explicit for
# future tests that need to exercise UI or environment-mutating helpers.

with_safe_local_env <- function(code) {
  env <- new.env(parent = parent.frame())
  force(code(env))
}

with_mocked_rstudio_api <- function(expr) {
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    hasFun = function(...) TRUE,
    executeCommand = function(...) invisible(NULL),
    showQuestion = function(...) FALSE,
    applyTheme = function(...) invisible(NULL),
    .package = "rstudioapi"
  )

  force(expr)
}

skip_rstudio_ui_tests <- function(message = "RStudio UI actions are intentionally skipped in automated tests.") {
  if (!rstudioapi::isAvailable() || identical(Sys.getenv("CI"), "true")) {
    skip(message)
  }
}
