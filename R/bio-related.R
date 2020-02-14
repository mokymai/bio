
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
check_updates_bio  <- function(show_status = "always", install = "outdated", ...) {
  get_pkgs_installation_status(list_name = "bio", show_status = show_status,
    install = install, ...)
}

#' @rdname bio_version
#' @export
update_bio <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    rstudioapi::restartSession(
      paste(sep = "\n",
        '# Updating package "bio"...',
        'remotes::install_github("mokymai/bio", dependencies = TRUE, upgrade = FALSE)',
        'bio::get_vesion_bio()'
      )
    )

  } else {
    # usethis::ui_stop(
    #   'To run this function, RStudio version 1.1.281 is required. You may also use code:
    #   remotes::install_github("mokymai/bio", dependencies = TRUE, upgrade = FALSE)'
    #   )
    remotes::install_github("mokymai/bio", dependencies = TRUE, upgrade = FALSE)
    bio::get_vesion_bio()
  }
}

#' Update Rcmdr Biostat
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' update_rcmdr_biostat()
#'
#' }}
update_rcmdr_biostat <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    rstudioapi::restartSession(
      paste(sep = "\n",
        '# Updating package "RcmdrPlugin.biostat"...',
        'remotes::install_github("GegznaV/RcmdrPlugin.biostat", dependencies = TRUE, upgrade = FALSE)',
        'bio::check_installed_packages(',
        '  "Rcmdr-biostat", ',
        '  show_status = "newer_on_cran",',
        '  cran = "newer_on_cran"',
        ')'

      )
    )

  } else {
    # usethis::ui_stop(
    #   'To run this function, RStudio version 1.1.281 is required. You may also use code:
    #   remotes::install_github("mokymai/bio", dependencies = TRUE, upgrade = FALSE)'
    #   )
    remotes::install_github("GegznaV/RcmdrPlugin.biostat", dependencies = TRUE, upgrade = FALSE)
    bio::check_installed_packages(
      "Rcmdr-biostat",
      show_status = "newer_on_cran", cran = "newer_on_cran"
    )
  }
}
