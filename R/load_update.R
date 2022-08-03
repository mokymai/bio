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

