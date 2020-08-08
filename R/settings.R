# bio::download_spellcheck_dictionaries()
# bio::reset_rstudio_user_settings(to = "bio-default", backup = TRUE)
# bio::reset_rstudio_keybindings(to = "bio-default", backup = TRUE)
# snippets::install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
# bio::reload_rstudio()

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
#' 6) Creates folder "~/R/Darbinis" and starts using it as working directory
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
#' bio::reset_rstudio_gmc()
#'
#' }}
reset_rstudio_gmc <- function(..., force_update_dictionaries = FALSE) {

  status <- restriction_status(...)

  if (!(status || is_classroom_ip())) {
    usethis::ui_oops("Unfortunately, this function does not work on this computer.")
    return(invisible())
  }

  # Tab History
  clear_rs_history()
  # clear_r_history(backup = FALSE)
  unlink(".Rhistory")

  # Dictionaries
  dict_path <- get_path_rs_system_dictionaries_dir()
  lt_LT_is_missing <- !any(stringr::str_detect(dir(dict_path), "lt_LT"))
  if (force_update_dictionaries || lt_LT_is_missing) {
    bio::download_spellcheck_dictionaries()
  }

  # Working directory
  rstudioapi::executeCommand("setWorkingDirToProjectDir", quiet = TRUE)

  # Create/Clean directories
  fs::dir_create(fs::path_expand_r("~/R/Darbinis"))

  bs_folder <- fs::path_expand("~/Desktop/BS-2020/")
  try(fs::dir_delete(bs_folder), silent = TRUE)
  fs::dir_create(bs_folder)

  # User preferences
  bio::reset_rstudio_user_settings(to = "bio-default", backup = TRUE, ask = FALSE)

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
  bio::reset_rstudio_layout()
  rstudioapi::executeCommand("zoomActualSize",  quiet = TRUE)
  rstudioapi::executeCommand("zoomIn",          quiet = TRUE)
  rstudioapi::executeCommand("zoomIn",          quiet = TRUE)
  rstudioapi::executeCommand("activateConsole", quiet = TRUE)

  # Settings
  snippets::install_snippets_from_package(type = c("r", "markdown"))

  # Reset keybindings
  bio::reset_rstudio_keybindings("bio-default", backup = TRUE)

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
    bio::reload_rstudio()
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
#' @title Clear and Reset R and RStudio.
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
clear_rs_history <- function(backup = FALSE) {
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
reset_rstudio_layout <- function(rs_layout = "left") {
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
#' @title RStudio settings and preferences.
#' @description  Get a list of current RStudio preferences.
#'
#' @details Find the names of the preferences at:
#' https://github.com/rstudio/rstudio/blob/5f2b79427ed526e22f78c794a4b5894ebee2de75/src/cpp/session/SessionUserSettings.cpp#L357-L447
#'
#' @return A named list with some of current RStudio preferences.
#' @seealso
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
#' head(read_rs_user_settings(), n = 2)
#'
#' head(read_rs_user_settings("bio-default"), n = 2)
#'
#' #-------------------------------------------------
#' head(read_rs_ui_prefs(), n = 2)
#'
#' head(read_rs_ui_prefs("bio-default"), n = 2)
#'
#' #-------------------------------------------------
#' head(get_rs_ui_prefs(), n = 2)
#'
#' #-------------------------------------------------
#' get_rs_user_settings_names()
#'
#' get_rs_ui_pref_names()
#'
#' #-------------------------------------------------
#'
#' reset_rstudio_user_settings(to = "bio-default")
#'
#' reset_rstudio_user_settings(to = "rstudio-default")
#'
#' }}
#' @inheritParams get_path_rs_user_settings
NULL

# For auto-completion
user_settings_defaults <- c('bio-default', 'rstudio-default')


#' @rdname RStudio-settings
#' @export
#' @param to The set of RStudio user settings.
#'        One of: "rstudio-default" or "bio-default".
#' @param backup (logical) If `TRUE`, a backup copy of files with settings is
#'        created.
#' @param ask (logical) If `TRUE`, user confirmation to reset settings is
#'       required.
reset_rstudio_user_settings <- function(to, backup = TRUE, ask = TRUE) {

  if (missing(to)) {
    ui_stop(paste0(
      "The set of RStudio user settings is not defined (argument '{yellow('to')}').\n",
      'Possible choices: {ui_value(user_settings_defaults)}.'
    ))
  }

  checkmate::assert_choice(to, c("rstudio-default", "bio-default"))

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

  file_current <- get_path_rs_user_settings("current")

  if (isTRUE(backup)) {
    create_backup_copy(file_current, "user_settings", "RStudio settings")
  }

  switch(
    to,

    "rstudio-default" = {
      fs::file_delete(file_current)
      success <- !fs::file_exists(file_current)
    },

    "bio-default" = {
      file_default <- get_path_rs_user_settings("bio-default")
      fs::dir_create("~/R/Darbinis") # TODO: change this value, when default UI preferences change.
      fs::dir_create(fs::path_dir(file_current))
      fs::file_copy(file_default, file_current, overwrite = TRUE)
      success <-
        unname(tools::md5sum(file_default) == tools::md5sum(file_current))
    },

    usethis::ui_stop(paste0(
      'Unknown option of user setting defaults: to = {usethis::ui_value(to[1])}. \n',
      "Possible options: {ui_value(user_settings_defaults)}."
    ))

  )

  if (isTRUE(success)) {
    usethis::ui_done("RStudio user settings were reset to {green(to)}.")
    ui_msg_restart_rstudio()

  } else {
    usethis::ui_oops("Failure to reset RStudio user settings.")

  }

}


#' @rdname RStudio-settings
#' @export
read_rs_user_settings <- function(which = "current") {
  file   <- get_path_rs_user_settings(which)
  liness <- readr::read_lines(file)
  prefs  <- str_glue_eval('list({stringr::str_c(liness, collapse = ", ")})')
  prefs
}

#' @rdname RStudio-settings
#' @export
read_rs_ui_prefs <- function(which = "current") {
  prefs <- read_rs_user_settings(which)
  uiPrefs <- jsonlite::parse_json(prefs$uiPrefs)
  # prefs$uiPrefs <- NULL
  # list(prefs, uiPrefs)
  uiPrefs
}

#' @rdname RStudio-settings
#' @export
get_rs_ui_prefs <- function() {

  prefs <-
    c("use_spaces_for_tab",
      "num_spaces_for_tab",
      "auto_append_newline",
      "strip_trailing_whitespace",
      "default_encoding",
      "default_sweave_engine",
      "default_latex_program",
      "always_enable_concordance",
      "spelling_dictionary_language",
      "spelling_custom_dictionaries",
      "handle_errors_in_user_code_only",
      "shiny_viewer_type",
      "plumber_viewer_type",
      "enable_rstudio_connect",
      "diagnostics_in_function_calls",
      "check_arguments_to_r_function_calls",
      "check_unexpected_assignment_in_function_call",
      "warn_if_no_such_variable_in_scope",
      "warn_if_variable_defined_but_not_used",
      "enable_style_diagnostics",
      "ansi_console_mode",
      "terminal_websockets",
      "terminal_autoclose",
      "terminal_track_env",
      "busy_detection",
      "show_rmd_render_command",
      "show_publish_diagnostics",
      "publish_check_certificates",
      "use_publish_ca_bundle",
      "publish_ca_bundle"
    )
  if (rstudioapi::isAvailable() && exists(".rs.readUiPref")) {
    vals <- purrr::map(prefs, ~.rs.readUiPref(.))
    purrr::set_names(vals, prefs)
  }
}

#' @rdname RStudio-settings
#' @export
get_rs_user_settings_names <- function(which = "current") {
  names(read_rs_user_settings(which))
}

#' @rdname RStudio-settings
#' @export
get_rs_ui_pref_names <- function(which = "current") {
  names(read_rs_ui_prefs(which))
}


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
#' bio::reset_rstudio_keybindings(to = "bio-default")
#' bio::reload_rstudio()
#'
#' }}
reset_rstudio_keybindings <- function(to, backup = TRUE) {

  if (missing(to)) {
    ui_stop(paste0(
      "The set of RStudio shortcut keys is not defined (argument '{yellow('to')}').\n",
      'Possible options: {ui_value(keybindings_defaults)}.'
    ))
  }
  checkmate::assert_string(to)

  switch(
    to,

    "bio-default" = {
      from_files <- fs::dir_ls(path_bio_rs(), regexp = "keybindings--.*?.json$")
      base_names <- stringr::str_extract(from_files, "(?<=keybindings--).*?.json$")
      current_files <- fs::path(get_path_rs_keybindings_dir(), base_names)
    },

    "rstudio-default" = {
      current_files <-
        if (fs::dir_exists(get_path_rs_keybindings_dir())) {
          fs::dir_ls(
            get_path_rs_keybindings_dir(),
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
#' `restart_r()` restarts R session in RStudio.
#' `reload_rstudio()` reloads RStudio without closing it.
#'
#' @export
#' @concept utilities
restart_r <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("restartR", quiet = TRUE))
  }
}

#' @rdname restart-reload
#' @export
#' @concept utilities
reload_rstudio <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("reloadUi", quiet = TRUE))
  }
}


# Other ======================================================================
# @name rs-settings
# @title RStudio management and settings
# @export
browse_rs_addins <- function() {
  invisible(rstudioapi::executeCommand("browseAddins", quiet = TRUE))
}

browse_r_cheat_sheets <- function() {
  invisible(rstudioapi::executeCommand("browseCheatSheets", quiet = TRUE))
}

show_console <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("activateConsole", quiet = TRUE))
  }
}

switch_to_tab <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261") ) {
    invisible(rstudioapi::executeCommand("switchToTab", quiet = TRUE))
  }
}

# ============================================================================
list_files_on_desktop <- function(type = "file") {

  present_files <- fs::dir_ls(fs::path_expand("~/desktop/"), type = type)

  exts <- paste0(
    "zip|rar|gif|jpg|png|tiff?|docx?|pptx?|xlsx?|r|rmd|rdata|rds|py|",
    "txt|data?|csv|tab|tar|tat|",
    "pdf|fas|mdsx|mtsx|mas|meg|gz"
  )
  other <- "3.6.1| 3.6.2|bs-2020|bs-2019|r-2019"

  files_to_remove <-
    stringr::str_subset(present_files, stringr::str_glue("(\\.({exts})$)|({other})"))

  structure(fs::path_file(files_to_remove), class = "glue")
  invisible(files_to_remove)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str_to_quotes <- function(x) {
  if (is.character(x)) {
    x <- stringr::str_glue('"{x}"')
  }
  x
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If x is string, quotes " are added on both sides of this string to work well
# with glue().
chk_arg_upgrade <- function(x) {
  checkmate::assert_choice(
    as.character(x),
    c(TRUE, "default", "ask", "always", "never", FALSE)
  )
  str_to_quotes(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui_msg_restart_rstudio <- function() {
  usethis::ui_todo(paste0(
    "To take effect, {underline('RStudio')} should be ",
    "{underline('closed and reopened')}."
  ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
