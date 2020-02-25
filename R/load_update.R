#' @name update_pkg_rcmdr_biostat
#' @title Update package RcmdrPlugin.biostat and related packages
#'
#' @export
#' @inheritParams update_pkg_snippets
#' @inheritParams check_updates_bio
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_rcmdr_biostat()
#'
#' check_updates_rcmdr_biostat()
#'
#' }}
check_updates_rcmdr_biostat  <- function(show_status = "always",
  install = "outdated", upgrade = FALSE, ...) {

  # get_pkgs_installation_status(list_name = "Rcmdr-biostat", show_status = show_status,
  #   install = install, ...)
  check_installed_packages(list_name = "Rcmdr-biostat", show_status = show_status,
    install = install, upgrade = upgrade, ...)
}

#' @rdname update_pkg_rcmdr_biostat
#' @export
update_pkg_rcmdr_biostat <- function(upgrade = FALSE, force = FALSE) {
  update_pkg_from_github("RcmdrPlugin.biostat", "GegznaV/RcmdrPlugin.biostat",
    "Rcmdr-biostat", upgrade = upgrade, force = force)
}

#' Update RcmdrPlugin.biostat and related packages
#'
#' @export
#' @param upgrade
#'        Upgrade dependencies.
#'        See `upgrade` in [remotes::install_github()].
#' @param force (logical) `TRUE` or `FALSE`.
#'        Force to update.
#'        See `force` in [remotes::install_github()].
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_snippets()
#'
#' }}
update_pkg_snippets <- function(upgrade = FALSE, force = FALSE) {
  update_pkg_from_github("snippets", "GegznaV/snippets", "snippets",
    upgrade = upgrade, force = force)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_pkg_from_github <- function(pkg = "", github_repo = "", update_list = "",
  upgrade = FALSE, force = FALSE) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(github_repo)
  checkmate::assert_string(update_list)
  checkmate::assert_flag(force)
  upgrade <- chk_arg_upgrade(upgrade)


  force_str <-
    if (force) {
      ", force = TRUE"
    } else {
      ""
    }

  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {

    command <-
      stringr::str_glue(
        paste(sep = "\n",
          '# Updating package "{pkg}"...',
          'remotes::install_github(',
          '  "{github_repo}",',
          '  dependencies = TRUE, upgrade = {upgrade}{force_str}',
          ')',
          "",
          'packageVersion("{pkg}")',
          "",
          'bio::check_installed_packages("{update_list}", show_status = "newer_on_cran")'
        )
      )
    rstudioapi::restartSession(command)

  } else {
    remotes::install_github(github_repo, dependencies = TRUE, force = force,
      upgrade = upgrade)
    bio::check_installed_packages(update_list, show_status = "newer_on_cran")
  }
}
