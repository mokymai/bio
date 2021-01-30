# bio::rstudio_download_spellcheck_dictionaries()
# bio::rstudio_reset_user_settings(to = "bio-default", backup = TRUE)
# bio::rstudio_reset_keybindings(to = "bio-default", backup = TRUE)
# snippets::install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
# bio::rstudio_reload_ui()

# Clear and Reset ============================================================
ip_gmc_r209_compact <- "158.129.170.(3,200-237)"
ip_gmc_r209  <- paste0("158.129.170.", c(3, 200:237))

ip_gmc_c255_compact <- "158.129.170.240-253"
ip_gmc_c255  <- paste0("158.129.170.", 240:253)

ip_ec_108_compact <-
  "158.129.129.151-249, 158.129.136.241, 158.129.136.57, 158.129.136.61, 158.129.159.234"

ip_ec_108 <-
  c(
    paste0("158.129.129.", 151:249),
    "158.129.136.57",
    "158.129.136.61",
    "158.129.136.241",
    "158.129.159.234"
  )

is_classroom_ip <- function() {
  pingr::my_ip(method = "https") %in% c(ip_gmc_r209, ip_gmc_c255, ip_ec_108)
}

restriction_status <- function(ignore_ip = getOption("bio.ignore_ip", FALSE), ...) {
  isTRUE(ignore_ip)
}

#' Reset RStudio state in GMC R209 and clear environment
#'
#' This function:
#' 1) Resets RStudio state and user preferences (incl. color scheme)
#' 2) Clears function history, plot history, console, recent project list, etc.
#' 3) Closes unnecessary windows
#' 4) Resets custom keybindings to "bio-default"
#' 5) Resets R Markdown and R snippets to defaults in package "snippets"
#' 6) Creates folder "~/R/darbinis" and starts using it as working directory
#'    when no project is used.
#' 7) Clears (if possible)/creates folder "BS-2020" on Desktop.
#'
#' The function works only in GMC R209, GMC C255 and ITPC EC-108 classrooms.
#'
#' @param ... Further arguments for advanced users only.
#' @param force_update_dictionaries (logical) If `TRUE`, the dictionaries are
#' forced to be downloaded/updated.
#'
#' @export
#'
#' @concept r and rstudio settings
#'
#' @examples
#' \dontrun{\donttest{
#'
#' bio::rstudio_reset_in_gmc()
#'
#' }}
rstudio_reset_gmc <- function(..., force_update_dictionaries = FALSE) {

  status <- restriction_status(...)

  if (!(status || is_classroom_ip())) {
    usethis::ui_oops("Unfortunately, this function does not work on this computer.")
    return(invisible())
  }

  # Tab History
  rstudio_clear_history()
  # clear_r_history(backup = FALSE)
  unlink(".Rhistory")

  # Dictionaries
  dict_path <- rstudioapi::userDictionariesPath()
  lt_LT_is_missing <- !any(stringr::str_detect(dir(dict_path), "lt_LT"))
  if (force_update_dictionaries || lt_LT_is_missing) {
    bio::rstudio_download_spellcheck_dictionaries()
  }

  # Working directory
  rstudioapi::executeCommand("setWorkingDirToProjectDir", quiet = TRUE)

  # Create/Clean directories
  fs::dir_create(fs::path_expand_r("~/R/darbinis"))

  bs_folder <- fs::path_expand("~/Desktop/BS-2020/")
  try(fs::dir_delete(bs_folder), silent = TRUE)
  fs::dir_create(bs_folder)

  # User preferences
  bio::rstudio_reset_user_settings(to = "bio-default", backup = TRUE, ask = FALSE)

  # Tab Files
  # TODO: Go to home dir
  rstudioapi::executeCommand("clearRecentFiles",    quiet = TRUE)

  # Tab Plots
  rstudioapi::executeCommand("clearPlots",          quiet = TRUE)

  # Tab Help
  rstudioapi::executeCommand("clearHelpHistory",    quiet = TRUE)

  # Tab Viewer
  rstudioapi::executeCommand("viewerClearAll",      quiet = TRUE)

  # Projects
  rstudioapi::executeCommand("clearRecentProjects", quiet = TRUE)

  # Tab Environment
  bio::clear_r_workspace() # clearWorkspace

  # Layout
  bio::rstudio_reset_layout()
  rstudioapi::executeCommand("zoomActualSize",  quiet = TRUE)
  rstudioapi::executeCommand("zoomIn",          quiet = TRUE)
  rstudioapi::executeCommand("zoomIn",          quiet = TRUE)
  rstudioapi::executeCommand("activateConsole", quiet = TRUE)

  # Settings
  snippets::install_snippets_from_package(type = c("r", "markdown"))

  # Reset keybindings
  bio::rstudio_reset_keybindings("bio-default", backup = TRUE)

  # Console
  rstudioapi::executeCommand("closeAllTerminals", quiet = TRUE)
  rstudioapi::executeCommand("consoleClear",      quiet = TRUE)

  if (rstudioapi::isAvailable("1.2.879")) {

    light_theme <- rstudioapi::showQuestion(
      "Choose light or dark color theme",
      "Which theme (light/dark) should be used in RStudio?",
      " Light ",
      " Dark "
    )

    if (light_theme) {

      is_textmate <- rstudioapi::showQuestion(
        "Choose light color theme",
        "Which light theme should be used in RStudio?",
        " Textmate (default) ",
        " Crimson Editor "
      )

      if (is_textmate) {
        rstudioapi::applyTheme("Textmate (default)")

      } else {
        rstudioapi::applyTheme("Crimson Editor")
        # rstudioapi::applyTheme("Xcode")
        # rstudioapi::applyTheme("Clouds")
      }

    } else {
      is_cobalt <- rstudioapi::showQuestion(
        "Choose dark color theme",
        "Which dark theme should be used in RStudio?",
        " Cobalt (dark blue) ",
        " Tomorrow Night 80s (black) "
      )

      if (is_cobalt) {
        rstudioapi::applyTheme("Cobalt")

      } else {
        # rstudioapi::applyTheme("Vibrant Ink")
        # rstudioapi::applyTheme("Chaos")
        rstudioapi::applyTheme("Tomorrow Night 80s")
      }
    }
  }

  # Documents
  rstudioapi::executeCommand("closeAllSourceDocs", quiet = TRUE)

  # Sys.sleep(1)

  # Restart RS
  to_restart <- rstudioapi::showQuestion(
    "Restart RStudio",
    "Restart RStudio?",
    " Yes ",
    " No "
  )

  if (to_restart) {
    bio::rstudio_reload_ui()
  }

  invisible()


  # commands <- c(
  #   "cleanAll",
  #   "clearHelpHistory",
  #   "clearHistory",
  #   "clearJobs",
  #   "clearKnitrCache",
  #   "clearPlots",
  #   "clearPrerenderedOutput",
  #   "clearPresentationCache",
  #   "clearRecentFiles",
  #   "clearRecentProjects",
  #   "clearTerminalScrollbackBuffer",
  #   "clearUserPrefs",
  #   "clearWorkspace",
  #   "closeAllSourceDocs",
  #   "closeAllTerminals",
  #   # "closeOtherSourceDocs",
  #   "closeProject",
  #   # "closeSourceDoc",
  #   # "closeTerminal",
  #   "consoleClear",
  #   NULL
  # )
  # purrr::walk(commands, ~rstudioapi::executeCommand(. , quiet = TRUE))
}


#' @name clear_and_reset
#' @title Clear and Reset R and RStudio
#' @description Clear and Reset R and RStudio settings and preferences.
#'
#' @param backup (logical) If `TRUE`, a backup copy is created.
#'
#' @concept r and rstudio settings

NULL

# Clear R history
clear_r_history <- function(backup = TRUE) {
  # FIXME: jei Windows + RStudio, tai ši funkcija neveikia

  if (isTRUE(backup)) {

    new_name <- paste0("Rhistory", get_backup_stamp(), ".Rhistory")
    hist_backup <- fs::path(get_path_backup_dir(), new_name)

    withr::with_dir(get_path_backup_dir(), savehistory(file = new_name))

    usethis::ui_done("R history saved to {usethis::ui_path(hist_backup)}")
  }

  tmp_file <- tempfile()
  write("", file = tmp_file)
  loadhistory(tmp_file)
  unlink(tmp_file, recursive = TRUE, force = TRUE)
}

#' @rdname clear_and_reset
#' @export
rstudio_clear_history <- function(backup = FALSE) {
  if (isTRUE(backup)) {
    rstudioapi::executeCommand("saveHistory", quiet = TRUE)
  }

  unlink(".Rhistory")
  rstudioapi::executeCommand("clearHistory", quiet = TRUE)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname clear_and_reset
#' @export
clear_r_workspace <- function() {
  # Clear R workspace
  object_names <- ls(all.names = TRUE, envir = .GlobalEnv)
  rm(list = object_names, envir = .GlobalEnv)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname clear_and_reset
#' @param rs_layout (`"left"`|`"right"`) Type of RStudio panes layout.
#' @export
rstudio_reset_layout <- function(rs_layout = "left") {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    # Set opened RS tabs
    rstudioapi::executeCommand("activateFiles",       quiet = TRUE)
    rstudioapi::executeCommand("activateEnvironment", quiet = TRUE)
    rstudioapi::executeCommand("activateConsole",     quiet = TRUE)

    switch(rs_layout,
      "right" = rstudioapi::executeCommand("layoutConsoleOnRight", quiet = TRUE),
      "left"  = rstudioapi::executeCommand("layoutConsoleOnLeft",  quiet = TRUE)
    )

    # End zooming of single window
    rstudioapi::executeCommand("layoutEndZoom", quiet = TRUE)
  }
  invisible()
}


# Settings and Preferences ===================================================
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
    ans <- usethis::ui_nope(
      "Do you want to reset RStudio user settings to {ui_value(to)}?"
    )
    # ans <- usethis::ui_nope("...", yes = "Yes")

    if (ans) {
      usethis::ui_warn("Cancelled.")
      return(invisible())
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
      file_default <- get_path_rstudio_config_file(which = "bio")
      # TODO: change this value, when default UI preferences change:
      fs::dir_create("~/R/darbinis", recurse = TRUE)
      fs::dir_create(fs::path_dir(file_current), recurse = TRUE)
      fs::file_copy(file_default, file_current,  overwrite = TRUE)
      success <-
        unname(tools::md5sum(file_default) == tools::md5sum(file_current))
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
    ui_msg_restart_rstudio()

  } else {
    usethis::ui_oops("Failure to reset RStudio user settings.")
  }
}



# Find the names of the preferences at:
# https://github.com/rstudio/rstudio/blob/5f2b79427ed526e22f78c794a4b5894ebee2de75/src/cpp/session/SessionUserSettings.cpp#L357-L447
NULL

# For auto-completion
user_settings_defaults <- c("bio-default", "bio-dark-blue", "bio-black",
  "rstudio-default")



# Keybindings ================================================================

# For auto-completion
keybindings_defaults   <- c('bio-default', 'rstudio-default')

#' Set RStudio keybindings
#'
#' @param to (string) The type of keybindings.
#'        Currently available value is `"bio-default"`.
#' @param backup (logical) If `TRUE`, a back-up copy of files with current
#'        keybindings will be created.
#'
#' @export
#'
#' @concept r and rstudio settings
#'
#' @examples
#' \dontrun{\dontest{
#'
#' bio::rstudio_reset_keybindings(to = "bio-default")
#' bio::rstudio_reload()
#'
#' }}
rstudio_reset_keybindings <- function(to, backup = TRUE) {

  if (missing(to)) {
    ui_stop(paste0(
      "The value of argument '{yellow('to')}' is missing.\n",
      'Possible options: {ui_value(keybindings_defaults)}.'
    ))
  }
  checkmate::assert_string(to)

  switch(
    to,

    "bio-default" = {
      from_files <- fs::dir_ls(path_bio_rs(), regexp = "keybindings--.*?.json$")
      base_names <- stringr::str_extract(from_files, "(?<=keybindings--).*?.json$")
      current_files <- fs::path(get_path_rstudio_keybindings_dir(), base_names)
    },

    "rstudio-default" = {
      current_files <-
        if (fs::dir_exists(get_path_rstudio_keybindings_dir())) {
          fs::dir_ls(
            get_path_rstudio_keybindings_dir(),
            regexp = "[.]json$"
          )

        } else {
          character(0)
        }
    },

    usethis::ui_stop(paste0(
      'Unknown type of keybidings: to = {usethis::ui_value(to[1])}. \n',
      "Possible options: {ui_value(user_settings_defaults)}."
    ))
  )

  # Create back-up copies
  if (isTRUE(backup)) {
    create_backup_copy(current_files, "keybindings", "shortcut keys")
  }

  # Reset current keybindings
  switch(
    to,
    "rstudio-default" = {
      # RStudio defaults are set when setup files are deleted
      fs::file_delete(current_files)
    },
    {
      # To set other options, files must be copied
      fs::dir_create(fs::path_dir(current_files), recurse = TRUE)
      fs::file_copy(from_files, current_files, overwrite = TRUE)
    }
  )

  # Output message
  ui_done("Shortcut keys were reset to {green(to)}.")
}

# Restart/Reload -------------------------------------------------------------
#' @name restart-reload
#' @title Functions that to restart or reload programs
#' @description
#' `rstudio_restart_r()` restarts R session in RStudio.
#' `rstudio_reload()` reloads RStudio without closing it.
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
