# List packages --------------------------------------------------------------

#' Get Standard Base R Packages
#'
#' Returns a character vector of standard packages included in the base R
#' distribution to allow filtering them out from dependencies.
#'
#' @return Character vector of base R package names.
#' @keywords internal
get_base_packages <- function() {
  c(
    "base", "compiler", "datasets", "graphics", "grDevices",
    "grid", "methods", "parallel", "profile", "splines",
    "stats", "stats4", "tcltk", "tools", "utils"
  )
}

#' List Packages Used in Directory
#'
#' Scans a directory for R, Rmd, and Qmd files and returns a vector of
#' unique package names used across those files.
#'
#' @param path Path to directory. Defaults to `"."`.
#' @param exclude_base Logical. If `TRUE` (default), base R packages
#'   (e.g., `stats`, `utils`, `graphics`) are excluded from the output.
#' @param progress Logical. Whether to show a progress bar. Defaults to `FALSE`.
#' @param ... Further arguments passed to [renv::dependencies()].
#'
#' @return Character vector of unique, sorted package names.
#' @export
#' @concept packages
list_pkgs_used_in_dir <- function(path = ".",
                                  exclude_base = TRUE,
                                  progress = FALSE,
                                  ...) {
  deps <- renv::dependencies(path = path, progress = progress, ...)

  if (nrow(deps) == 0) {
    return(character(0))
  }

  pkgs <- unique(deps$Package)

  if (exclude_base) {
    pkgs <- setdiff(pkgs, get_base_packages())
  }

  sort(pkgs)
}

#' List Packages Used in Specific File(s)
#'
#' Scans specific R, Rmd, or Qmd file(s) and returns a vector of
#' unique package names used inside them.
#'
#' @param files Character vector of file paths (R, Rmd, Qmd).
#' @param exclude_base Logical. If `TRUE` (default), base R packages
#'   (e.g., `stats`, `utils`, `graphics`) are excluded from the output.
#' @param progress Logical. Whether to show a progress bar. Defaults to `FALSE`.
#' @param ... Further arguments passed to [renv::dependencies()].
#'
#' @return Character vector of unique, sorted package names.
#' @export
#' @concept packages
list_pkgs_used_in_files <- function(files,
                                    exclude_base = TRUE,
                                    progress = FALSE,
                                    ...) {
  # Keep only existing files to prevent errors
  valid_files <- files[file.exists(files)]

  if (length(valid_files) == 0) {
    return(character(0))
  }

  deps <- renv::dependencies(path = valid_files, progress = progress, ...)

  if (nrow(deps) == 0) {
    return(character(0))
  }

  pkgs <- unique(deps$Package)

  if (exclude_base) {
    pkgs <- setdiff(pkgs, get_base_packages())
  }

  sort(pkgs)
}
