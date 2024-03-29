#' Determine Operating System (OS)
#'
#' Determine the operating system (OS) of your machine.
#'
#' @return OS name in lower case: windows, mac, linux, etc.
#' @author
#' The code is based on https://www.r-bloggers.com/identifying-the-os-from-r/
#' @seealso
#' https://www.r-bloggers.com/identifying-the-os-from-r/
#'
#' @export
#' @concept utilities
#'
#' @examples
#' get_os_type()
get_os_type <- function() {
  sys_info <- Sys.info()
  if (!is.null(sys_info)) {
    os <- sys_info["sysname"]
    if (os == "Darwin") {os <- "mac"}
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin",   R.version$os, useBytes = TRUE)) {os <- "mac"}
    if (grepl("linux-gnu", R.version$os, useBytes = TRUE)) {os <- "linux"}
  }
  unname(tolower(os))
}

#' @rdname get_os_type
#' @export
is_32bit_os <- function() {
  stringr::str_detect(version$arch, "32$")
}

#' @rdname get_os_type
#' @export
is_64bit_os <- function() {
  stringr::str_detect(version$arch, "64$")
}


