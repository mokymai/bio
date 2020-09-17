# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `open_in_rstudio()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [open_in_rstudio()] and will be removed soon.
#'
#' @param path See [open_in_rstudio()].
#'
#' @export
#' @concept deprecated
open_in_rs <- function(path) {
  .Deprecated("bio::open_in_rstudio()")
  fs::file_show(path = path, browser = "RStudio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `check_packages_by_topic()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by  [check_packages_by_topic()] and will be
#'  removed soon.
#'
#' @param ... See [check_packages_by_topic()].
#'
#' @name check_packages_by_topic-deprecated
#'
#' @export
#'
#' @concept deprecated
#'
check_installed_packages <- function(...) {
  # TODO: Create new function `check_installed_packages()` that checks info
  # about package.
  .Deprecated("bio::check_packages_by_topic()")
  check_packages_by_topic(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `check_updates_pkg_bio()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [check_updates_pkg_bio()] and will be removed
#' soon.
#'
#' @param show_status See [check_updates_pkg_bio()].
#' @param install See [check_updates_pkg_bio()].
#' @param upgrade See [check_updates_pkg_bio()].
#' @param ... See [check_updates_pkg_bio()].
#'
#' @export
#' @concept deprecated
check_updates_bio  <- function(show_status = "always", install = "outdated",
  upgrade = TRUE, ...) {

  .Deprecated("bio::check_updates_pkg_bio()")

  check_updates_pkg_bio(
    show_status = show_status,
    install = install,
    upgrade = upgrade,
    ...
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `get_vesion_pkg_bio()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [get_vesion_pkg_bio()] and will be removed
#' soon.
#'
#' @export
#' @concept deprecated
get_vesion_bio  <- function() {
  .Deprecated("bio::get_vesion_pkg_bio()")
  packageVersion("bio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_gmc()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_gmc()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_gmc()]
#'
#' @export
#' @concept deprecated
reset_rstudio_gmc <- function(...) {
  .Deprecated("bio::rstudio_reset_gmc()")
  rstudio_reset_gmc(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_clear_history()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_clear_history()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_clear_history()]
#'
#' @export
#' @concept deprecated
clear_rs_history <- function(...) {
  .Deprecated("bio::rstudio_clear_history()")
  rstudio_clear_history(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_layout()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_layout()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_layout()]
#'
#' @export
#' @concept deprecated
reset_rstudio_layout <- function(...) {
  .Deprecated("bio::rstudio_reset_layout()")
  rstudio_reset_layout(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_user_settings()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_user_settings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_user_settings()]
#'
#' @export
#' @concept deprecated
reset_rstudio_user_settings <- function(...) {
  .Deprecated("bio::rstudio_reset_user_settings()")
  rstudio_reset_user_settings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_keybindings()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_keybindings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_keybindings()]
#'
#' @export
#' @concept deprecated
reset_rstudio_keybindings <- function(...) {
  .Deprecated("bio::rstudio_reset_keybindings()")
  rstudio_reset_keybindings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_restart_r()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_restart_r()] and will be removed soon.
#'
#' @export
#' @concept deprecated
restart_r <- function() {
  .Deprecated("bio::rstudio_restart_r()")
  rstudio_restart_r()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reload_ui()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reload_ui()] and will be removed soon.
#'
#' @export
#' @concept deprecated
reload_rstudio <- function() {
  .Deprecated("bio::rstudio_reload_ui()")
  rstudio_reload_ui()
}
