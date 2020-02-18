#' Update package RcmdrPlugin.biostat and related packages
#'
#' @export
#' @inheritDotParams update_pkg_snippets
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_rcmdr_biostat()
#'
#' }}
update_pkg_rcmdr_biostat <- function(upgrade = TRUE, force = FALSE) {
  update_pkg_from_github("RcmdrPlugin.biostat", "GegznaV/RcmdrPlugin.biostat",
    "Rcmdr-biostat", upgrade = upgrade, force = force)
}

#' Update RcmdrPlugin.biostat and related packages
#'
#' @export
#' @param upgrade (logical) `TRUE` or `FALSE`.
#'        Upgrade dependencies.
#'        See `upgrade` in [remotes::install_github()].
#' @param force (logical) `TRUE` or `FALSE`.
#'        Force to update.
#'        See `upgrade` in [remotes::install_github()].
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_snippets()
#'
#' }}
update_pkg_snippets <- function(upgrade = TRUE, force = FALSE) {
  update_pkg_from_github("snippets", "GegznaV/snippets", "snippets",
    upgrade = upgrade, force = force)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_pkg_from_github <- function(pkg = "", github_repo = "", update_list = "",
  upgrade = FALSE, force = FALSE) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(github_repo)
  checkmate::assert_string(update_list)
  checkmate::assert_flag(upgrade)
  checkmate::assert_flag(force)

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
          'packageVersion("{pkg}")',
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
