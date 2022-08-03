# Settings and Preferences ===================================================

# For auto-completion
user_setting_set_names <- c(
  "bio-default", "bio-dark-blue", "bio-black", "rstudio-default"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-settings
#' @title Reset RStudio settings
#' @description
#' Reset RStudio to use predefined set of settings/preferences.
#' Correctly works only with RStudio 1.3 or newer.
#' Recommended to use with RStudio 2022.07.1 or newer.
#'
#' @param to The name of pre-defined set of RStudio settings/preferences.
#'        Options: "rstudio-default",
#'                 "bio-default",
#'                 "bio-dark-blue",
#'                 "bio-black".
#' @param backup (logical)
#'        If `TRUE`, a backup copy of files with settings is created.
#' @param ask (logical)
#'       If `TRUE`, additional confirmation to reset settings is required.
#'
#' @details
#' Settings that can be used in `rstudio-prefs.json` file:
#' https://docs.rstudio.com/ide/server-pro/session-user-settings.html
#'
#' @seealso
#' [get_path_rstudio_config_file()]
#'
#'
#' On [Customizing RStudio](https://support.rstudio.com/hc/en-us/articles/200549016-Customizing-the-RStudio-IDE) using point-and-click method.
#'
#' On [Configuration and Settings](https://www.rstudio.com/blog/rstudio-1-3-preview-configuration/).
#'
#' A list of [Session User Settings](https://docs.rstudio.com/ide/server-pro/session_user_settings/session_user_settings.html) to be used with
#' [rstudioapi::writeRStudioPreference()].
#'
#' On [RStudio setting locations](https://docs.rstudio.com/ide/desktop-pro/settings/settings.html).
#'
#' On [Resetting RStudio Desktop's State](https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State).
#'
#' StackOverflow threads on export/import RStudio of user preferences:
#' - https://stackoverflow.com/a/55940249/4783029
#' - https://stackoverflow.com/a/54982341/4783029
#'
#' @concept r and rstudio settings
#'
#' @examples
#' \dontrun{\donttest{
#'
#' rstudio_reset_user_settings(to = "rstudio-default")
#'
#' rstudio_reset_user_settings(to = "bio-default")
#'
#' rstudio_reset_user_settings(to = "bio-dark-blue")
#'
#' rstudio_reset_user_settings(to = "bio-black")
#'
#' }}
#' @export
rstudio_reset_user_settings <- function(to, backup = TRUE, ask = TRUE) {

  # Check arguments
  if (missing(to)) {
    # If the set of RStudio user settings is not chosen
    ui_stop(paste0(
      "The value of argument '{yellow('to')}' is missing.\n",
      'Possible choices: {ui_value(user_setting_set_names)}.'
    ))
  }

  checkmate::assert_choice(to, user_setting_set_names)

  # Take user inputs
  if (isTRUE(ask)) {
    rstudio_clear_console_ask()

    if (rstudioapi::isAvailable(version_needed = "1.1.67")) {
      ans <-
        rstudioapi::showQuestion(
          "Change User Settings",
          glue::glue("Do you want to set RStudio user settings to '{to}'?"),
          "No", "Yes"
        )

    } else {
      ans <- usethis::ui_nope(
        "Do you want to set RStudio user settings to {ui_value(to)}?",
        yes = "Yes"
      )
    }

    if (ans) {
      usethis::ui_warn("Cancelled.")
      return(invisible(NULL))
    }
  }

  # Change settings
  file_current <- get_path_rstudio_config_file("current")

  # Backup
  if (isTRUE(backup)) {
    create_backup_copy(file_current, "user_settings", "RStudio settings")
  }

  # Delete current settings (use RStudio defaults)
  fs::file_delete(file_current)

  # All other setup files contain differences from the default settings
  rs_default <- get_path_rstudio_config_file(which = "rstudio-default")
  success <- set_rstudio_preferences(rs_default)

  # Change what is different from the defaults
  switch(
    to,

    "rstudio-default" = NULL,

    "bio-default" = ,
    "bio-dark-blue" = ,
    "bio-black" = {
      # Change the default dir, if default UI preferences change
      fs::dir_create("~/R/darbinis", recurse = TRUE)

      file_default <- get_path_rstudio_config_file(which = "bio")
      success <- set_rstudio_preferences(file_default)

    },

    usethis::ui_stop(paste0(
      'Unknown option of user setting defaults: to = {usethis::ui_value(to[1])}. \n',
      "Possible options: {ui_value(user_setting_set_names)}."
    ))
  )


  # Change RStudio theme
  switch(
    to,
    "bio-default"   = rstudioapi::applyTheme("Textmate (default)"),
    "bio-dark-blue" = rstudioapi::applyTheme("Cobalt"),
    "bio-black"     = rstudioapi::applyTheme("Chaos")
  )

  if (isTRUE(success)) {
    usethis::ui_done("RStudio user settings were set to {green(to)}.")
    rstudio_reload_ui()
    ui_msg_restart_rstudio()

  } else {
    usethis::ui_oops("Failure to reset RStudio user settings.")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read preference from JSON file and set them in RStudio
set_rstudio_preferences <- function(file) {
  if (rstudioapi::isAvailable("1.3.387")) {
    pref <- jsonlite::fromJSON(file)
    purrr::walk2(
      names(pref), unname(pref),
      ~rstudioapi::writeRStudioPreference(.x, .y)
    )
    TRUE

  } else {
    FALSE
  }
}
