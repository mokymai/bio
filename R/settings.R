# bio::rstudio_download_spellcheck_dictionaries()
# bio::rstudio_reset_user_settings(to = "bio-default", backup = TRUE)
# bio::rstudio_reset_keybindings(to = "bio-default", backup = TRUE)
# snippets::install_snippets_from_package("snippets", type = c("r", "markdown"), backup = TRUE)
# bio::rstudio_reload_ui()

# Clear and Reset ============================================================

#' Check whether local reset safeguards are intentionally bypassed.
#'
#' This helper keeps the legacy override semantics but avoids any hard-coded
#' network allow-list. It is intended as a small guard for destructive local
#' reset actions, and it can be bypassed explicitly when a user opts in.
#'
#' @param ignore_ip Logical scalar that bypasses the local reset safeguard.
#' @param ... Additional arguments ignored for compatibility with older callers.
#' @return Logical scalar, `TRUE` when the safeguard is intentionally overridden.
#' @keywords internal
#' @examples
#' if (interactive()) {
#'   restriction_status(ignore_ip = TRUE)
#'   restriction_status(ignore_ip = FALSE)
#' }
restriction_status <- function(ignore_ip = getOption("bio.ignore_ip", FALSE), ...) {
  isTRUE(ignore_ip)
}

#' Reset the local RStudio session to a known-good classroom/lab state.
#'
#' The function performs several destructive cleanup steps aimed at restoring a
#' consistent RStudio environment:
#' 1. clears history and recent-session state;
#' 2. resets user settings and keybindings;
#' 3. clears the current R workspace;
#' 4. restores the default snippets and layout;
#' 5. optionally updates spellcheck dictionaries; and
#' 6. restarts RStudio when the user confirms.
#'
#' This helper is intentionally conservative and protects destructive reset
#' actions behind a simple override flag. The code does not rely on external
#' IP metadata or a hard-coded allow-list.
#'
#' @param ... Further arguments used by `restriction_status()` for compatibility.
#' @param force_update_dictionaries Logical scalar. If `TRUE`, the dictionaries
#'   are refreshed even when the current locale is present.
#'
#' @return Invisibly returns `NULL` after the reset workflow completes.
#' @keywords internal
#' @examples
#' if (interactive()) {
#'   options(bio.ignore_ip = TRUE)
#'   bio::rstudio_reset_gmc()
#' }
rstudio_reset_gmc <- function(..., force_update_dictionaries = FALSE) {

  status <- restriction_status(...)

  if (!status) {
    usethis::ui_oops("This action is restricted. You may explicitly bypass it.")
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
  fs::dir_create(fs::path_expand_r("~/R/main"))

  bs_folder <- fs::path_expand("~/Desktop/BS-pratybos/")
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
  clear_r_workspace() # clearWorkspace

  # Layout
  rstudio_reset_layout()
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
    bio::restart_rstudio()
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
#' @noRd
#' @concept r and rstudio settings

NULL

# Clear R history
clear_r_history <- function(backup = TRUE) {
  # FIXME: if Windows + RStudio, then this function does not work

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

# @rdname clear_and_reset
# @noRd
rstudio_clear_history <- function(backup = FALSE) {
  if (isTRUE(backup)) {
    rstudioapi::executeCommand("saveHistory", quiet = TRUE)
  }

  unlink(".Rhistory")
  rstudioapi::executeCommand("clearHistory", quiet = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Clear the global R workspace.
#'
#' Useful for the "reset" flows in RStudio when the user wants to remove all
#' objects from the global environment without removing attached packages or
#' environment state outside `.GlobalEnv`.
#'
#' @param envir Environment to clear. Defaults to `.GlobalEnv`.
#' @return Invisibly returns the cleared environment.
#' @keywords internal
#' @examples
#' x <- 1
#' bio:::clear_r_workspace()
#' exists("x", where = .GlobalEnv)
clear_r_workspace <- function(envir = .GlobalEnv) {
  if (!is.environment(envir)) {
    stop("`envir` must be an environment.", call. = FALSE)
  }

  object_names <- ls(all.names = TRUE, envir = envir)
  if (length(object_names) > 0L) {
    rm(list = object_names, envir = envir)
  }

  invisible(envir)
}

#' Reset the RStudio pane layout.
#'
#' @param rs_layout Character scalar: either `"left"` or `"right"`.
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @examples
#' \dontrun{
#' rstudio_reset_layout("left")
#' }
rstudio_reset_layout <- function(rs_layout = "left") {
  rs_layout <- match.arg(tolower(rs_layout), c("left", "right"))

  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    # Set opened RS tabs
    rstudioapi::executeCommand("activateFiles", quiet = TRUE)
    rstudioapi::executeCommand("activateEnvironment", quiet = TRUE)
    rstudioapi::executeCommand("activateConsole", quiet = TRUE)

    switch(rs_layout,
      "right" = rstudioapi::executeCommand("layoutConsoleOnRight", quiet = TRUE),
      "left"  = rstudioapi::executeCommand("layoutConsoleOnLeft", quiet = TRUE)
    )

    # End zooming of single window
    rstudioapi::executeCommand("layoutEndZoom", quiet = TRUE)
  }

  invisible(NULL)
}

#' Activate the console in RStudio when available.
#'
#' @return Invisibly returns `NULL` when RStudio is unavailable.
#' @keywords internal
#' @examples
#' \dontrun{
#' rstudio_activate_console()
#' }
rstudio_activate_console <- function() {
  if (rstudioapi::isAvailable(version_needed = "1.2.1261")) {
    invisible(rstudioapi::executeCommand("activateConsole", quiet = TRUE))
  }

  invisible(NULL)
}

#' Ask before clearing the RStudio console.
#'
#' @return Invisibly returns `NULL` if RStudio is unavailable or the user says no.
#' @keywords internal
#' @examples
#' \dontrun{
#' rstudio_clear_console_ask()
#' }
rstudio_clear_console_ask <- function() {
  if (!rstudioapi::isAvailable(version_needed = "1.2.1261")) {
    return(invisible(NULL))
  }

  ans <- rstudioapi::showQuestion(
    "Clear console", "Do you want to clear console?", "No", "Yes"
  )

  if (!ans) {
    invisible(rstudioapi::executeCommand("consoleClear", quiet = TRUE))
  }

  invisible(NULL)
}

