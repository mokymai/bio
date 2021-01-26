# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `open_in_rstudio()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [open_in_rstudio()] and will be removed soon.
#'
#' @param path See [open_in_rstudio()].
#'
#' @export
#' @concept defunct
open_in_rs <- function(path) {
  .Defunct("bio::open_in_rstudio()")
  fs::file_show(path = path, browser = "RStudio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `check_packages_by_topic()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by  [check_packages_by_topic()] and will be
#'  removed soon.
#'
#' @param ... See [check_packages_by_topic()].
#'
#' @name check_packages_by_topic-defunct
#'
#' @export
#'
#' @concept defunct
#'
check_installed_packages <- function(...) {
  # TODO: Create new function `check_installed_packages()` that checks info
  # about package.
  .Defunct("bio::check_packages_by_topic()")
  check_packages_by_topic(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `check_updates_pkg_bio()` instead
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
#' @concept defunct
check_updates_bio  <- function(show_status = "always", install = "outdated",
  upgrade = TRUE, ...) {

  .Defunct("bio::check_updates_pkg_bio()")

  check_updates_pkg_bio(
    show_status = show_status,
    install = install,
    upgrade = upgrade,
    ...
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `get_vesion_pkg_bio()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [get_vesion_pkg_bio()] and will be removed
#' soon.
#'
#' @export
#' @concept defunct
get_vesion_bio  <- function() {
  .Defunct("bio::get_vesion_pkg_bio()")
  packageVersion("bio")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_reset_gmc()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_gmc()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_gmc()]
#'
#' @export
#' @concept defunct
reset_rstudio_gmc <- function(...) {
  .Defunct("bio::rstudio_reset_gmc()")
  rstudio_reset_gmc(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_clear_history()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_clear_history()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_clear_history()]
#'
#' @export
#' @concept defunct
clear_rs_history <- function(...) {
  .Defunct("bio::rstudio_clear_history()")
  rstudio_clear_history(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_reset_layout()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_layout()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_layout()]
#'
#' @export
#' @concept defunct
reset_rstudio_layout <- function(...) {
  .Defunct("bio::rstudio_reset_layout()")
  rstudio_reset_layout(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_reset_user_settings()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_user_settings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_user_settings()]
#'
#' @export
#' @concept defunct
reset_rstudio_user_settings <- function(...) {
  .Defunct("bio::rstudio_reset_user_settings()")
  rstudio_reset_user_settings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_reset_keybindings()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reset_keybindings()] and will be removed soon.
#' @param ... Arguments passed to [rstudio_reset_keybindings()]
#'
#' @export
#' @concept defunct
reset_rstudio_keybindings <- function(...) {
  .Defunct("bio::rstudio_reset_keybindings()")
  rstudio_reset_keybindings(...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_restart_r()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_restart_r()] and will be removed soon.
#'
#' @export
#' @concept defunct
restart_r <- function() {
  .Defunct("bio::rstudio_restart_r()")
  rstudio_restart_r()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' DEFUNCT. Use `rstudio_reload_ui()` instead
#'
#' **DO NOT USE THIS FUNCTION!**
#' This function was replaced by [rstudio_reload_ui()] and will be removed soon.
#'
#' @export
#' @concept defunct
reload_rstudio <- function() {
  .Defunct("bio::rstudio_reload_ui()")
  rstudio_reload_ui()
}
