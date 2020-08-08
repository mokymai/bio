# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deprecated. Use `open_in_rstudio()` instead
#'
#' This function was replaced by [open_in_rstudio()] and will be removed soon.
#'
#' @export
#' @concept deprecated
open_in_rs <- function(path) {
  .Deprecated("open_in_rstudio")
  fs::file_show(path = path, browser = "RStudio")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deprecated. Use `check_packages_by_topic()` instead
#'
#' This function was replaced by  [check_packages_by_topic()] and will be
#'  removed soon.
#'
#' @param ... See [check_packages_by_topic()].
#'
#' @name check_packages_by_topic-deprecated
#'
#' @export
#'
#' @concept deprecated
#'
check_installed_packages <- function(...) {
  # TODO: Create new function `check_installed_packages()` that checks info
  # about package.
  .Deprecated("check_packages_by_topic")
  check_packages_by_topic(...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deprecated. Use `check_updates_pkg_bio()` instead
#'
#' This function was replaced by [check_updates_pkg_bio()] and will be removed
#' soon.
#'
#' @export
#' @concept deprecated
check_updates_bio  <- function(show_status = "always", install = "outdated",
  upgrade = FALSE, ...) {

  .Deprecated("check_updates_pkg_bio")

  check_updates_pkg_bio(
    show_status = show_status,
    install = install,
    upgrade = upgrade,
    ...
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Deprecated. Use `get_vesion_pkg_bio()` instead
#'
#' This function was replaced by [get_vesion_pkg_bio()] and will be removed
#' soon.
#'
#' @export
#' @concept deprecated
get_vesion_bio  <- function() {
  .Deprecated("get_vesion_pkg_bio")
  packageVersion("bio")
}
