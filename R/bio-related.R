
#' @name bio_version
#' @title Manage Package '\pkg{bio}'
#' @description
#' `get_vesion_pkg_bio()` returns version number of package \pkg{bio}.
#' `check_updates_pkg_bio()` checks if package \pkg{bio} has a recommended version.
#' `update_pkg_bio()` tries to update package \pkg{bio}.
#'
#' @examples
#' get_vesion_pkg_bio()
#'
#' \dontrun{\donttest{
#'
#' check_updates_pkg_bio()
#' update_bio()
#'
#' }}
NULL

#' @rdname bio_version
#' @export
#' @concept packages
get_vesion_pkg_bio  <- function() {
  packageVersion("bio")
}

#' @rdname bio_version
#'
#' @param show_status See argument `show_status` in [get_pkgs_installation_status()].
#' @param install See argument `install` in [get_pkgs_installation_status()].
#' @param ... Arguments to further methods.
#'
#' @export
#' @concept packages
check_updates_pkg_bio  <- function(show_status = "always", install = "outdated",
  upgrade = TRUE, ...) {

  check_packages_by_topic(list_name = "bio", show_status = show_status,
    install = install, upgrade = upgrade, ...)

  # get_pkgs_installation_status(list_name = "bio", show_status = show_status,
  # install = install, ...)
}


#' @rdname bio_version
#'
#' @param upgrade
#'        Upgrade dependencies.
#'        See `upgrade` in [remotes::install_cran()].
#' @param force (logical) `TRUE` or `FALSE`.
#'        Force to update.
#'        See `force` in [remotes::install_cran()].
#' @param quiet (logical) `TRUE` or `FALSE`.
#'        Should installation messages be printed?
#'        See `quiet` in [remotes::install_cran()].
#' @param ... Further arguments passed to [remotes::install_cran()].
#'
#' @export
#' @concept packages
update_pkg_bio <- function(upgrade = TRUE, force = FALSE, quiet = TRUE) {
  checkmate::assert_flag(force)
  checkmate::assert_flag(quiet)
  upgrade_str <- chk_arg_upgrade(upgrade)

  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    force_str <- if (force) {", force = TRUE"} else {""}
    quiet_str <- if (quiet) {", quiet = TRUE"} else {""}

    command <- glue::glue(
      paste(sep = "\n",
        '# Updating package "bio"...',
        '',
        'repos <- c("https://mokymai.github.io/download/", getOption("repos"))',
        'remotes::install_cran(c("backup.tools", "snippets", "bio"), repos = repos, ',
        '  dependencies = TRUE, upgrade = {upgrade_str}{force_str}{quiet_str})',
        '',
        'bio::get_vesion_pkg_bio()',
        '',
        'bio::check_packages_by_topic("bio", show_status = "newer_on_cran", upgrade = {upgrade_str})'
      )
    )
    rstudioapi::restartSession(command)

  } else {
    repos <- c("https://mokymai.github.io/download/", getOption("repos"))
    remotes::install_cran(
      c("backup.tools", "snippets", "bio"), repos = repos,
      dependencies = TRUE, upgrade = upgrade, force = force, quiet = quiet
    )
    bio::get_vesion_pkg_bio()
    bio::check_packages_by_topic(
      "bio", show_status = "newer_on_cran", upgrade = upgrade
    )
  }
}
