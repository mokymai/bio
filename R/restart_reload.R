# Restart/Reload -------------------------------------------------------------
#' @name restart-reload
#' @title Functions to Restart R and Reload RStudio
#' @description
#' - `rstudio_restart_r()` restarts R session in RStudio.
#' - `rstudio_reload()` reloads RStudio without closing it.
#'
#' @export
#'
#' @concept utilities
#'
rstudio_restart_r <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("restartR", quiet = TRUE))
  }
}

#' @rdname restart-reload
#' @export
#' @concept utilities
rstudio_reload_ui <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("reloadUi", quiet = TRUE))
  }
}
