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



#' @name restart-reload
#' @export
restart_rstudio <- function() {
  rstudio_restart_r()
}


rstudio_activate_console <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("activateConsole", quiet = TRUE))
  }
}

rstudio_clear_console_ask <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    ans <-
      rstudioapi::showQuestion(
        "Clear console", "Do you want to clear console?", "No", "Yes"
      )
    if (!ans) {
      invisible(rstudioapi::executeCommand("consoleClear", quiet = TRUE))
    }
  }
}
