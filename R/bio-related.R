
#' @name bio_version
#' @title Version management of package 'bio'.
#' @description
#' `get_vesion_bio()` returns version number of package \pkg{bio}.
#' `check_updates_bio()` checks if package \pkg{bio} has a recommended version.
#' `update_pkg_bio()` tries to update package \pkg{bio}.
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
#' @inheritParams check_installed_packages
#' @param ... Arguments to further methods.
#'
check_updates_bio  <- function(show_status = "always", install = "outdated",
  upgrade = FALSE, ...) {

  check_installed_packages(list_name = "bio", show_status = show_status,
    install = install, upgrade = upgrade, ...)

  # get_pkgs_installation_status(list_name = "bio", show_status = show_status,
  # install = install, ...)
}

#' @rdname bio_version
#' @inheritParams update_pkg_snippets
#' @export
update_pkg_bio <- function(upgrade = FALSE, force = FALSE) {
  checkmate::assert_flag(force)
  upgrade_str <- chk_arg_upgrade(upgrade)

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
        'remotes::install_github(c("GegznaV/backup.tools", "GegznaV/snippets", "mokymai/bio"), ',
        '  dependencies = TRUE, upgrade = {upgrade_str}{force_str})',
        "",
        'bio::get_vesion_bio()',
        "",
        'bio::check_installed_packages("bio", show_status = "newer_on_cran", upgrade = {upgrade_str})'
      )
    )
    rstudioapi::restartSession(command)

  } else {
    remotes::install_github(c("GegznaV/backup.tools", "mokymai/bio"),
      dependencies = TRUE, upgrade = upgrade, force = force)
    bio::get_vesion_bio()
    bio::check_installed_packages("bio", show_status = "newer_on_cran",
      upgrade = upgrade)
  }
}
