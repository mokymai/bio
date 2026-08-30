# Shared safety helpers for automated tests.
# These keep test actions non-destructive and make the intent explicit for
# future tests that need to exercise UI or environment-mutating helpers.

with_safe_local_env <- function(code) {
  env <- new.env(parent = parent.frame())
  force(code(env))
}

# `usethis::ui_*()` helpers emit via `message()` (stderr), while plain
# `cat()` writes to stdout; capture both interleaved, in order, as one
# character vector of lines (like the user would see in a console).
capture_all_output <- function(expr) {
  con <- textConnection("captured_lines", "w", local = TRUE)
  on.exit(close(con))
  sink(con)
  sink(con, type = "message")
  on.exit(
    {
      sink(type = "message")
      sink()
    },
    add = TRUE,
    after = FALSE)

  force(expr)
  captured_lines
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
