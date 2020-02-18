
#' @name bio_version
#' @title Version management of package 'bio'.
#' @description
#' `get_vesion_bio()` returns version number of package \pkg{bio}.
#' `check_updates_bio()` checks if package \pkg{bio} has a recommended version.
#' `update_bio()` tries to update package \pkg{bio}.
#'
#' @examples
#' get_vesion_bio()
#'
#' \dontrun{\donttest{
#'
#' check_updates_bio()
#' update_bio()
#'
#' }}
NULL

#' @rdname bio_version
#' @export
get_vesion_bio  <- function() {
  packageVersion("bio")
}

#' @rdname bio_version
#' @export
#' @param show_status See argument `show_status` in [get_pkgs_installation_status()].
#' @param install See argument `install` in [get_pkgs_installation_status()].
#' @param ... Arguments to further methods.
#'
check_updates_bio  <- function(show_status = "always", install = "outdated", ...) {
  get_pkgs_installation_status(list_name = "bio", show_status = show_status,
    install = install, ...)
}

#' @rdname bio_version
#' @inheritParams update_pkg_snippets
#' @export
update_pkg_bio <- function(upgrade = FALSE, force = FALSE) {
  checkmate::assert_flag(upgrade)
  checkmate::assert_flag(force)

  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    force_str <-
      if (force) {
        ", force = TRUE"
      } else {
        ""
      }

    command <- stringr::str_glue(
      paste(sep = "\n",
        '# Updating package "bio"...',
        'remotes::install_github(c("GegznaV/backup.tools", "mokymai/bio"), ',
        '  dependencies = TRUE, upgrade = {upgrade}{force_str})',
        'bio::get_vesion_bio()'
        # 'bio::check_installed_packages("bio", show_status = "newer_on_cran")'
      )
    )
    rstudioapi::restartSession(command)

  } else {
    # usethis::ui_stop(
    #   'To run this function, RStudio version 1.1.281 is required. You may also use code:
    #   remotes::install_github("mokymai/bio", dependencies = TRUE, upgrade = FALSE)'
    #   )
    remotes::install_github(c("GegznaV/backup.tools", "mokymai/bio"),
      dependencies = TRUE, upgrade = upgrade, force = force)
    bio::get_vesion_bio()
    # bio::check_installed_packages("bio", show_status = "newer_on_cran")
  }
}
