# Settings and Preferences ===================================================

# For auto-completion
user_settings_defaults <- c("bio-default", "bio-dark-blue", "bio-black",
  "rstudio-default")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-settings
#' @title Reset RStudio settings
#' @description
#' Reset RStudio settings. Correctly works only with RStudio 1.3 or newer.
#'
#' @param to The name of set with RStudio settings.
#'        One of: "rstudio-default", "bio-default", or "bio-dark-blue".
#' @param backup (logical) If `TRUE`, a backup copy of files with settings is
#'        created.
#' @param ask (logical) If `TRUE`, user confirmation to reset settings is
#'       required.
#'
#' @details
#' Settings that can be used in `rstudio-prefs.json` file
#' (for RStudio 1.3 or newer):
#' https://docs.rstudio.com/ide/server-pro/session-user-settings.html
#'
#' @seealso
#' [get_path_rstudio_config_file()]
#'
#' RStudio functions `.rs.readUiPref()`, `.rs.writeUiPref()`.
#' - https://stackoverflow.com/a/55940249/4783029
#' - https://stackoverflow.com/a/54982341/4783029
#'
#' @concept r and rstudio settings
#'
#' @examples
#' \dontrun{\donttest{
#' #-------------------------------------------------
#' # .rs.readUiPref()
#' # .rs.writeUiPref()
#'
#' #-------------------------------------------------
#'
#' rstudio_reset_user_settings(to = "rstudio-default")
#'
#' rstudio_reset_user_settings(to = "bio-default")
#'
#' rstudio_reset_user_settings(to = "bio-dark-blue")
#'
#' rstudio_reset_user_settings(to = "bio-black")
#'
#'
#' }}
#'
#' @export
rstudio_reset_user_settings <- function(to, backup = TRUE, ask = TRUE) {

  if (missing(to)) {
    # If the set of RStudio user settings is not chosen
    ui_stop(paste0(
      "The value of argument '{yellow('to')}' is missing.\n",
      'Possible choices: {ui_value(user_settings_defaults)}.'
    ))
  }

  checkmate::assert_choice(to, user_settings_defaults)

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

  file_current <- get_path_rstudio_config_file("current")

  if (isTRUE(backup)) {
    create_backup_copy(file_current, "user_settings", "RStudio settings")
  }

  switch(
    to,

    "rstudio-default" = {
      fs::file_delete(file_current)
      success <- !fs::file_exists(file_current)
    },

    "bio-default" = ,
    "bio-dark-blue" = ,
    "bio-black" = {
      # Change default dir if default UI preferences change
      fs::dir_create("~/R/darbinis", recurse = TRUE)

      file_default <- get_path_rstudio_config_file(which = "bio")
      success <- set_rstudio_preferences(file_default)
      # fs::dir_create(fs::path_dir(file_current), recurse = TRUE)
      # fs::file_copy(file_default, file_current,  overwrite = TRUE)
      # success <-
      #   unname(tools::md5sum(file_default) == tools::md5sum(file_current))
    },

    usethis::ui_stop(paste0(
      'Unknown option of user setting defaults: to = {usethis::ui_value(to[1])}. \n',
      "Possible options: {ui_value(user_settings_defaults)}."
    ))
  )

  # Adjust 'bio' theme
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
  pref <-
    readr::read_lines(file, lazy = FALSE) %>%
    jsonlite::parse_json() %>%
    purrr::map_chr(~ {
      if (is.character(.)) paste0("'", ., "'") else as.character(.)
    })

  fun <- "rstudioapi::writeRStudioPreference"
  txt <- glue::glue("{fun}('{names(pref)}', {unname(pref)})\n")

  eval(parse(text = txt))

  TRUE
}


# Find the names of the preferences at:
# https://github.com/rstudio/rstudio/blob/5f2b79427ed526e22f78c794a4b5894ebee2de75/src/cpp/session/SessionUserSettings.cpp#L357-L447
NULL

