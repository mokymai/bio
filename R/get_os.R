#' Detect the current operating system
#'
#' Returns a normalized operating-system label for the current R session.
#' The result is a single lowercase string such as "windows", "mac", or
#' "linux"; other Unix-like systems are normalized to their platform name.
#'
#' @return A length-1 character string with the current OS name in lowercase.
#' @concept utilities
#' @export
#'
#' @examples
#' get_os_type()
get_os_type <- function() {
  sysname <- Sys.info()[["sysname"]]

  if (!is.null(sysname) && nzchar(sysname)) {
    os <- switch(
      sysname,
      "Darwin"  = "mac",
      "Windows" = "windows",
      "Linux"   = "linux",
      tolower(sysname)
    )
  } else {
    os_type <- tolower(.Platform$OS.type)

    if (identical(os_type, "windows")) {
      os <- "windows"
    } else if (grepl("darwin", R.version$os, ignore.case = TRUE)) {
      os <- "mac"
    } else if (grepl("linux", R.version$os, ignore.case = TRUE)) {
      os <- "linux"
    } else {
      os <- os_type
    }
  }

  as.character(os)
}

#' Check whether the current session is running on a 64-bit OS
#'
#' @rdname get_os_type
#' @export
is_64bit_os <- function() {
  isTRUE(.Machine$sizeof.pointer == 8L)
}

#' Check whether the current session is running on a 32-bit OS
#'
#' @rdname get_os_type
#' @export
is_32bit_os <- function() {
  isTRUE(.Machine$sizeof.pointer == 4L)
}
