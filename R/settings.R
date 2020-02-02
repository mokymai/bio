# Clear and Reset ============================================================

reset_rstudio <- function() {
  stop("Function is harmful")

  # rstudioapi::executeCommand("clearUserPrefs")
  bio::reset_rs_user_settings("bio-default", backup = TRUE)

  rstudioapi::executeCommand("clearPlots",          quiet = TRUE)
  rstudioapi::executeCommand("clearRecentFiles",    quiet = TRUE)
  rstudioapi::executeCommand("clearRecentProjects", quiet = TRUE)
  rstudioapi::executeCommand("clearHelpHistory",    quiet = TRUE)
  rstudioapi::executeCommand("closeAllSourceDocs",  quiet = TRUE)
  bio::reset_rstudio_layout()
  # clearWorkspace
  bio::clear_r_workspace()
  rstudioapi::executeCommand("consoleClear",         quiet = TRUE)

  snippets::install_snippets_from_package(type = "r")
  snippets::install_snippets_from_package(type = "markdown")

  # TODO: Reset keybindings

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


# Clear R history
#' @name clear_and_reset
#' @title Clear and Reset R and RStudio.
#' @description Clear and Reset R and RStudio settings and preferences.
#'
#' @param backup (logical) If `TRUE`, a backup copy is created.
#'
#' @export
clear_r_history <- function(backup = TRUE) {
  if (isTRUE(backup)) {
    base <- make.names(Sys.time())
    savehistory(file = paste0(base, ".Rhistory"))
  }

  tmp_file <- tempfile()
  write("", file = tmp_file)
  loadhistory(tmp_file)
  unlink(tmp_file, recursive = TRUE, force = TRUE)
}

#' @rdname clear_and_reset
#' @export
clear_rs_history <- function() {
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

# Initial config =============================================================

# Mod.: 2019-08-26
#
# USAGE:
# source("https://mokymai.github.io/resursai/rs-settings/nustatymai-rs-v1.2.R", encoding = "UTF-8")
#
# NOTE: now works on Windows.
#
# TODO:
# 1. Adapt code for Linux and Mac.
# 2. Create file backups, if they are already present.
# 3. check if RStudio version is at least 1.2.
# 4. If case no snippet file exists, a file with default snippets should be
#    added before adding new snippet.
# 5. Add key bindings and snippets only if they do not exist or differ.
#    - In case of differences, allow choosing which one should be kept.

# set_initial_rs_configuration()

set_initial_rs_configuration <- function() {
  # if (!require("fs")) {install.packages("fs")}

  # Download files
  config_url  = "https://mokymai.github.io/resursai/rs-settings/nustatymai-rs-v1.2.zip"
  config_dir  = tempdir()
  config_file = fs::file_temp("nustatymai-rs-v1.2__", config_dir, ext = ".zip")

  on.exit(fs::dir_delete(config_dir)) # Clean up

  download.file(url = config_url, destfile = config_file)

  # Unzip
  message("Unzipping files. Please wait...")
  unzip(config_file, exdir = config_dir, setTimes = TRUE)

  # Set R snippent and key bindings
  message("Copying configuration files...")

  rs_settings_from <-  fs::path(config_dir, ".Windows/.R")
  rs_settings_to   <-  fs::path_expand_r("~/.R")

  fs::dir_create(rs_settings_to, recurse = TRUE)

  fs::dir_copy(
    path     = rs_settings_from,
    new_path = rs_settings_to,
    overwrite = TRUE
  )

  # Set RStudio configuration
  rs_config_from <- fs::path(config_dir, ".Windows/RStudio-Desktop")
  rs_config_to   <- fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop")

  fs::dir_create(rs_config_to, recurve = TRUE)

  fs::dir_copy(
    path     = rs_config_from,
    new_path = rs_config_to,
    overwrite = TRUE
  )

  # Follow up message
  message(
    "\n\n",
    "Now you should TURN OFF and restart RStudio to complete the configutation.\n"
  )
}


# Preferences ================================================================
#' @name RStudio-user-prefs
#' @title RStudio preferences.
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
#' @export
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
#' }}

read_rs_user_settings <- function(which = "current") {
  file   <- get_path_rs_user_settings(which)
  liness <- readr::read_lines(file)
  prefs  <- str_glue_eval('list({stringr::str_c(liness, collapse = ", ")})')
  prefs
}

#' @rdname RStudio-user-prefs
#' @export
read_rs_ui_prefs <- function(which = "current") {
  prefs <- read_rs_user_settings(which)
  uiPrefs <- jsonlite::parse_json(prefs$uiPrefs)
  # prefs$uiPrefs <- NULL
  # list(prefs, uiPrefs)
  uiPrefs
}

#' @rdname RStudio-user-prefs
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

#' @rdname RStudio-user-prefs
#' @export
get_rs_user_settings_names <- function(which = "current") {
  names(read_rs_user_settings(which))
}

#' @rdname RStudio-user-prefs
#' @export
get_rs_ui_pref_names <- function(which = "current") {
  names(read_rs_ui_prefs(which))
}

#' @rdname RStudio-user-prefs
#' @export
reset_rs_user_settings <- function(to = "bio-default", backup = TRUE, ask = TRUE) {

  # to = c("rstudio-default", "bio-default")

  if (isTRUE(ask)) {
    ans <- usethis::ui_nope("Do you want to reset RStudio user settings?",
      yes = "Yes")

    if (ans) {
      usethis::ui_warn("Cancelled.")
      return(invisible())
    }
  }


  if (isTRUE(backup)) {
    # FIXME: not implemented
    stop("not implemented")
  }

  file_current <- get_path_rs_user_settings("current")

  switch(
    to,
    "rstudio-default" = {
      fs::file_delete(file_current)
      success <- !fs::file_exists(file_current)
    },
    "bio-default" = {
      file_default <- get_path_rs_user_settings("bio-default")
      fs::dir_create(fs::path_dir(file_default))
      fs::file_copy(file_default, file_current, overwrite = TRUE)
      success <-
        unname(tools::md5sum(file_default) == tools::md5sum(file_current))
    },
    usethis::ui_stop('Not recognozed option: to = {usethis::ui_value(to[1])}')

  )

  if (success) {
    usethis::ui_done("RStudio setting were reset. Now you should restart RStudio.")

  } else {
    usethis::ui_oops("Resetting RStudio settings failed.")

  }

}



# Settings ====================================================================
#' @name rs-settings
#' @title RStudio settings
#' @export
browse_rs_addins <- function() {
  rstudioapi::executeCommand("browseAddins", quiet = TRUE)
}

#' @name rs-settings
#' @export
browse_r_cheat_sheets <- function() {
  rstudioapi::executeCommand("browseCheatSheets", quiet = TRUE)
}

check_spelling <- function() {
  rstudioapi::executeCommand("checkSpelling", quiet = TRUE)
}