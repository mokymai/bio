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

#' Run one reset step without letting it stop the rest of the workflow.
#'
#' Evaluates `expr`, reports success or failure to the console, and turns
#' warnings into non-fatal notices. Used by [rstudio_reset_gmc()] so a single
#' failing step (e.g. no network for dictionaries) never blocks later steps
#' and never fails silently.
#'
#' @param label Character scalar describing the step, used in progress output.
#' @param expr Expression to evaluate (wrap multiple statements in `{ }`).
#' @return Invisibly returns a list with `label`, `ok` (logical), and
#'   `message` (the error message, or `NA` on success).
#' @keywords internal
#' @examples
#' bio:::run_reset_step("A step that works", 1 + 1)
#' bio:::run_reset_step("A step that fails", stop("boom"))
run_reset_step <- function(label, expr) {
  checkmate::assert_string(label)

  result <- tryCatch(
    {
      withCallingHandlers(
        expr,
        warning = function(w) {
          usethis::ui_warn(paste0(label, ": ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )
      list(label = label, ok = TRUE, message = NA_character_)
    },
    error = function(e) {
      list(label = label, ok = FALSE, message = conditionMessage(e))
    }
  )

  if (isTRUE(result$ok)) {
    usethis::ui_done(label)
  } else {
    usethis::ui_oops(paste0(label, " failed: ", result$message))
  }

  invisible(result)
}

#' Reset the local RStudio session to a known-good classroom/lab state.
#'
#' The function performs several destructive cleanup steps aimed at restoring
#' a consistent RStudio environment for a lab/classroom computer:
#' 1. optionally updates spellcheck dictionaries;
#' 2. resets user settings, keybindings, snippets, theme and layout to course
#'    defaults;
#' 3. clears recent files/projects, help history, plots and viewer tabs;
#' 4. clears the current R workspace; and
#' 5. as the very last steps, clears the R console and the command history.
#'
#' Every step is run through [run_reset_step()], so a failure in one step
#' (e.g. missing internet connection while downloading dictionaries) is
#' reported but does not prevent the remaining steps from running. A summary
#' of what worked and what did not is printed at the end. None of the steps
#' below open interactive confirmation popups.
#'
#' This helper is intentionally conservative and protects destructive reset
#' actions behind a simple override flag (see `restriction_status()`). It is
#' meant to be run on classroom/lab computers, never on a developer's own,
#' non-standardized RStudio session.
#'
#' @param ... Further arguments used by `restriction_status()` for compatibility.
#' @param force_update_dictionaries Logical scalar. If `TRUE`, the dictionaries
#'   are refreshed even when the current locale is present.
#'
#' @return Invisibly returns a data frame with one row per step, its `ok`
#'   status, and an error `message` (if any).
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

  steps <- list()

  # Dictionaries
  steps$dictionaries <- run_reset_step("Update spellcheck dictionaries", {
    dict_path <- get_path_rstudio_config_dir("dictionaries/languages-system")
    lt_LT_is_missing <- !any(stringr::str_detect(dir(dict_path), "lt_LT"))
    if (force_update_dictionaries || lt_LT_is_missing) {
      bio::rstudio_download_spellcheck_dictionaries()
    }
  })

  # Working directory
  steps$working_dir <- run_reset_step("Set working directory to project directory", {
    rstudioapi::executeCommand("setWorkingDirToProjectDir", quiet = TRUE)
  })

  # User preferences (ask = FALSE: no confirmation popup)
  steps$user_settings <- run_reset_step("Reset user settings", {
    bio::rstudio_reset_user_settings(to = "bio-default", backup = TRUE, ask = FALSE)
  })

  # Tabs: Files, Plots, Help, Viewer, Projects
  steps$recent_files <- run_reset_step("Clear recent files", {
    rstudioapi::executeCommand("clearRecentFiles", quiet = TRUE)
  })
  steps$plots <- run_reset_step("Clear plots", {
    rstudioapi::executeCommand("clearPlots", quiet = TRUE)
  })
  steps$help <- run_reset_step("Clear help history", {
    rstudioapi::executeCommand("clearHelpHistory", quiet = TRUE)
    rstudioapi::executeCommand("helpHome", quiet = TRUE)
  })
  steps$viewer <- run_reset_step("Clear viewer tab", {
    rstudioapi::executeCommand("viewerClearAll", quiet = TRUE)
  })
  steps$recent_projects <- run_reset_step("Clear recent projects", {
    rstudioapi::executeCommand("clearRecentProjects", quiet = TRUE)
  })

  # Environment tab
  steps$workspace <- run_reset_step("Clear R workspace", {
    clear_r_workspace()
  })

  # Layout
  steps$layout <- run_reset_step("Reset pane layout and zoom", {
    rstudio_reset_layout()
    rstudioapi::executeCommand("zoomActualSize", quiet = TRUE)
    rstudioapi::executeCommand("zoomIn", quiet = TRUE)
    rstudioapi::executeCommand("activateConsole", quiet = TRUE)
  })

  # Snippets
  steps$snippets <- run_reset_step("Install default snippets", {
    snippets::install_snippets_from_package("snippets", type = c("r", "markdown"))
  })

  # Keybindings
  steps$keybindings <- run_reset_step("Reset keybindings", {
    bio::rstudio_reset_keybindings("bio-default", backup = TRUE)
  })

  # Theme
  steps$theme <- run_reset_step("Apply default theme", {
    rstudioapi::applyTheme("Textmate (default)")
  })

  # Documents
  steps$documents <- run_reset_step("Close all source documents", {
    rstudioapi::executeCommand("closeAllSourceDocs", quiet = TRUE)
  })

  # Create/clean course directories
  steps$directories <- run_reset_step("Create/clean course directories", {
    fs::path_expand_r("~/R/main") |> fs::dir_create()

    bs_folder <- fs::path_expand("~/Desktop/BS-pratybos/")
    if (fs::dir_exists(bs_folder)) {
      fs::dir_delete(bs_folder)
    }
    fs::dir_create(bs_folder)
  })

  # Terminals
  steps$terminals <- run_reset_step("Close all terminals", {
    rstudioapi::executeCommand("closeAllTerminals", quiet = TRUE)
  })

  # Console — must run near the very end
  steps$console <- run_reset_step("Clear R console", {
    rstudioapi::executeCommand("consoleClear", quiet = TRUE)
  })

  # History — the very last step
  steps$history <- run_reset_step("Clear R history", {
    rstudio_clear_history(backup = FALSE)
    unlink(".Rhistory")
  })

  summary_df <- data.frame(
    step    = names(steps),
    ok      = vapply(steps, function(x) isTRUE(x$ok), logical(1)),
    message = vapply(steps, function(x) x$message, character(1)),
    stringsAsFactors = FALSE
  )

  n_failed <- sum(!summary_df$ok)
  if (n_failed > 0) {
    usethis::ui_oops("RStudio reset finished with {n_failed} failed step(s).")
  } else {
    usethis::ui_done("RStudio reset finished successfully.")
  }

  invisible(summary_df)
}

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


# Sys.sleep(1)

# Restart RS
# bio::restart_rstudio()  # Hangs RStudio

# if (rs_restart == "ask") {
#   to_restart <- rstudioapi::showQuestion(
#     "Restart RStudio",
#     "Restart RStudio?",
#     " Yes ",
#     " No "
#   )
# } else if (isTRUE(rs_restart)) {
#   to_restart <- TRUE
# } else {
#   return(invisible())
# }
#
# if (to_restart) {
#   bio::restart_rstudio()
# }


# if (rstudioapi::isAvailable("1.2.879")) {
#
#   light_theme <- rstudioapi::showQuestion(
#     "Choose light or dark color theme",
#     "Which theme (light/dark) should be used in RStudio?",
#     " Light ",
#     " Dark "
#   )
#
#   if (light_theme) {
#
#     is_textmate <- rstudioapi::showQuestion(
#       "Choose light color theme",
#       "Which light theme should be used in RStudio?",
#       " Textmate (default) ",
#       " Crimson Editor "
#     )
#
#     if (is_textmate) {
#       rstudioapi::applyTheme("Textmate (default)")
#
#     } else {
#       rstudioapi::applyTheme("Crimson Editor")
#       # rstudioapi::applyTheme("Xcode")
#       # rstudioapi::applyTheme("Clouds")
#     }
#
#   } else {
#     is_cobalt <- rstudioapi::showQuestion(
#       "Choose dark color theme",
#       "Which dark theme should be used in RStudio?",
#       " Cobalt (dark blue) ",
#       " Tomorrow Night 80s (black) "
#     )
#
#     if (is_cobalt) {
#       rstudioapi::applyTheme("Cobalt")
#
#     } else {
#       # rstudioapi::applyTheme("Vibrant Ink")
#       # rstudioapi::applyTheme("Chaos")
#       rstudioapi::applyTheme("Tomorrow Night 80s")
#     }
#   }
# }




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
#' env <- new.env()
#' env$x <- 1
#' bio:::clear_r_workspace(env)
#' exists("x", envir = env)
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
#' if (interactive()) {
#'   rstudio_reset_layout("left")
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
