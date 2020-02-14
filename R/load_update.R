#' Update package RcmdrPlugin.biostat and related packages
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_rcmdr_biostat()
#'
#' }}
update_pkg_rcmdr_biostat <- function() {
  update_pkg_from_github("RcmdrPlugin.biostat", "GegznaV/RcmdrPlugin.biostat",
    "Rcmdr-biostat")
}

#' Update RcmdrPlugin.biostat and related packages
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_pkg_snippets()
#'
#' }}
update_pkg_snippets <- function() {
  update_pkg_from_github("snippets", "GegznaV/snippets", "snippets")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
update_pkg_from_github <- function(pkg = "", github_repo = "", update_list = "") {
  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    rstudioapi::restartSession(
      stringr::str_glue(
        paste(sep = "\n",
          '# Updating package "{pkg}"...',
          'remotes::install_github(',
          '  "{github_repo}",',
          '  dependencies = TRUE, upgrade = FALSE',
          ')',
          'packageVersion("{pkg}")',
          'bio::check_installed_packages("{update_list}", show_status = "newer_on_cran")'
        )
      )
    )

  } else {
    remotes::install_github(github_repo, dependencies = TRUE,
      upgrade = FALSE)
    bio::check_installed_packages(update_list, show_status = "newer_on_cran")
  }
}
