#' @name update_pkg_rcmdr_biostat
#' @title Update '\pkg{RcmdrPlugin.biostat}' and related packages
#'
#' @export
#' @inheritParams update_pkg_snippets
#' @inheritParams check_updates_pkg_bio
#'
#' @concept packages
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_rcmdr_biostat()
#'
#' check_updates_rcmdr_biostat()
#'
#' }}
check_updates_rcmdr_biostat <- function(show_status = "always",
  install = "outdated", upgrade = TRUE, ...) {

  check_packages_by_topic(
    list_name = "Rcmdr-biostat",
    show_status = show_status,
    install = install,
    upgrade = upgrade,
    ...
  )
}

#' @rdname update_pkg_rcmdr_biostat
#' @export
update_pkg_rcmdr_biostat <- function(upgrade = TRUE, force = FALSE, ...) {
  repos <- c("https://mokymai.github.io/download/", getOption("repos"))
  remotes::install_cran(
    "RcmdrPlugin.biostat", repos = repos, upgrade = upgrade, force = force, ...
  )
}

#' Update '\pkg{snippets}' and related packages
#'
#' @inheritParams update_pkg_bio
#' @export
#'
#' @concept packages
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_snippets()
#'
#' }}
update_pkg_snippets <- function(upgrade = TRUE, force = FALSE, ...) {
  repos <- c("https://mokymai.github.io/download/", getOption("repos"))
  remotes::install_cran(
    "snippets", repos = repos, upgrade = upgrade, force = force, ...
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIXME: This function is not used anymore
#
update_pkg_from_github <- function(pkg = "", github_repo = "", update_list = "",
  upgrade = TRUE, force = FALSE, quiet = TRUE) {
  checkmate::assert_string(pkg)
  checkmate::assert_string(github_repo)
  checkmate::assert_string(update_list)
  checkmate::assert_flag(force)
  checkmate::assert_flag(quiet)
  upgrade_str <- chk_arg_upgrade(upgrade)

  force_str <- if (force) {", force = TRUE"} else {""}
  quiet_str <- if (quiet) {", quiet = TRUE"} else {""}

  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {

    command <-
      glue::glue(
        paste(sep = "\n",
          '# Updating package "{pkg}"...',
          'Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")',
          'remotes::install_github(',
          '  "{github_repo}",',
          '  dependencies = TRUE, upgrade = {upgrade_str}{force_str}{quiet_str}',
          ')',
          "",
          'packageVersion("{pkg}")',
          "",
          'bio::check_packages_by_topic("{update_list}", show_status = "newer_on_cran", upgrade = {upgrade_str})'
        )
      )
    rstudioapi::restartSession(command)

  } else {
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
    remotes::install_github(github_repo, dependencies = TRUE, force = force,
      upgrade = upgrade, quiet = quiet)
    bio::check_packages_by_topic(update_list, show_status = "newer_on_cran",
      upgrade = upgrade)
  }
}
