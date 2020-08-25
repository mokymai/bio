# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `open_in_rstudio()` instead
#'
#' This function was replaced by [open_in_rstudio()] and will be removed soon.
#'
#' @param path See [open_in_rstudio()].
#'
#' @export
#' @concept deprecated
open_in_rs <- function(path) {
  .Deprecated("open_in_rstudio")
  fs::file_show(path = path, browser = "RStudio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `check_packages_by_topic()` instead
#'
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
  .Deprecated("check_packages_by_topic")
  check_packages_by_topic(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `check_updates_pkg_bio()` instead
#'
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
  upgrade = FALSE, ...) {

  .Deprecated("check_updates_pkg_bio")

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
#' This function was replaced by [get_vesion_pkg_bio()] and will be removed
#' soon.
#'
#' @export
#' @concept deprecated
get_vesion_bio  <- function() {
  .Deprecated("get_vesion_pkg_bio")
  packageVersion("bio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_gmc()` instead
#'
#' This function was replaced by [rstudio_reset_gmc()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_gmc()]
#'
#' @export
#' @concept deprecated
reset_rstudio_gmc <- function(...) {
  .Deprecated("rstudio_reset_gmc")
  rstudio_reset_gmc(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_clear_history()` instead
#'
#' This function was replaced by [rstudio_clear_history()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_clear_history()]
#'
#' @export
#' @concept deprecated
clear_rs_history <- function(...) {
  .Deprecated("rstudio_clear_history")
  rstudio_clear_history(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_layout()` instead
#'
#' This function was replaced by [rstudio_reset_layout()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_layout()]
#'
#' @export
#' @concept deprecated
reset_rstudio_layout <- function(...) {
  .Deprecated("rstudio_reset_layout")
  rstudio_reset_layout(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_user_settings()` instead
#'
#' This function was replaced by [rstudio_reset_user_settings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_user_settings()]
#'
#' @export
#' @concept deprecated
reset_rstudio_user_settings <- function(...) {
  .Deprecated("rstudio_reset_user_settings")
  rstudio_reset_user_settings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_read_user_settings()` instead
#'
#' This function was replaced by [rstudio_read_user_settings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_read_user_settings()]
#'
#' @export
#' @concept deprecated
read_rs_user_settings <- function(...) {
  .Deprecated("rstudio_read_user_settings")
  rstudio_read_user_settings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_read_ui_prefs()` instead
#'
#' This function was replaced by [rstudio_read_ui_prefs()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_read_ui_prefs()]
#'
#' @export
#' @concept deprecated
read_rs_ui_prefs <- function(...) {
  .Deprecated("rstudio_read_ui_prefs")
  rstudio_read_ui_prefs(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_get_ui_prefs()` instead
#'
#' This function was replaced by [rstudio_get_ui_prefs()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_get_ui_prefs()]
#'
#' @export
#' @concept deprecated
get_rs_ui_prefs <- function(...) {
  .Deprecated("rstudio_get_ui_prefs")
  rstudio_get_ui_prefs(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_get_user_setting_names()` instead
#'
#' This function was replaced by [rstudio_get_user_setting_names()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_get_user_setting_names()]
#'
#' @export
#' @concept deprecated
get_rs_user_settings_names <- function(...) {
  .Deprecated("rstudio_get_user_setting_names")
  rstudio_get_user_setting_names(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_get_ui_pref_names()` instead
#'
#' This function was replaced by [rstudio_get_ui_pref_names()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_get_ui_pref_names()]
#'
#' @export
#' @concept deprecated
get_rs_ui_pref_names <- function(...) {
  .Deprecated("rstudio_get_ui_pref_names")
  rstudio_get_ui_pref_names(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reset_keybindings()` instead
#'
#' This function was replaced by [rstudio_reset_keybindings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_keybindings()]
#'
#' @export
#' @concept deprecated
reset_rstudio_keybindings <- function(...) {
  .Deprecated("rstudio_reset_keybindings")
  rstudio_reset_keybindings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_restart_r()` instead
#'
#' This function was replaced by [rstudio_restart_r()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_restart_r()]
#'
#' @export
#' @concept deprecated
restart_r <- function(...) {
  .Deprecated("rstudio_restart_r")
  rstudio_restart_r(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEPRECATED. Use `rstudio_reload()` instead
#'
#' This function was replaced by [rstudio_reload()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reload()]
#'
#' @export
#' @concept deprecated
reload_rstudio <- function(...) {
  .Deprecated("rstudio_reload")
  rstudio_reload(...)
}
