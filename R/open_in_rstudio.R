#' @name open_in_rstudio
#' @title Open file in RStudio
#'
#' @description
#' Function tries opening a file in RStudio.
#' @param path (sting) Path to file.
#' @param ... Further arguments to [rstudioapi::navigateToFile()].
#'
#' @concept open
#' @concept open files
#' @concept paths and dirs
#'
#' @seealso
#' - [rstudioapi::navigateToFile()],
#' - [fs::file_show()], [browseURL()],
#' - [utils::file.edit()]
#'
#' @export
open_in_rstudio <- function(path, ...) {
  if (rstudioapi::isAvailable("0.99.719") && rstudioapi::hasFun("navigateToFile")) {
    rstudioapi::navigateToFile(path, ...)
  } else {
    # TODO (SEE ALSO): usethis::edit_file()
    fs::file_show(path = path, browser = "RStudio")
  }
}
