
#' @title bio_version
#' @title Version management of package 'bio'.
#'
#' @return
#' `get_vesion_bio()` returns version number.
#'
#' @examples
#' get_vesion_bio()
#'
#' \dontrun{\donttest{
#'
#' check_vesion_bio()
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
check_vesion_bio  <- function(show_status = "always", install = "outdated", ...) {
  get_pkgs_installation_status("bio", include = "always",
    show_status = show_status, install = install, ...)
}

#' @rdname bio_version
#' @export
update_bio <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.1.281")) {
    rstudioapi::restartSession(
      paste(sep = "\n",
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
