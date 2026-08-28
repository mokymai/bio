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
#' if (interactive()) {
#'
#'   rstudio_reset_user_settings(to = "rstudio-default")
#'   rstudio_reset_user_settings(to = "bio-default")
#'   rstudio_reset_user_settings(to = "bio-dark-blue")
#'   rstudio_reset_user_settings(to = "bio-black")
#'
#' }
#' @export
rstudio_reset_user_settings <- function(to, backup = TRUE, ask = TRUE) {
  # Check arguments
  if (missing(to)) {
    # If the set of RStudio user settings is not chosen
    ui_stop(paste0(
      "The value of argument '{yellow('to')}' is missing.\n",
      "Possible choices: {ui_value(user_setting_set_names)}."
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
  success <- rstudio_set_preferences(rs_default)

  # Change what is different from the defaults
  switch(
    to,

    "rstudio-default" = {
      if (isTRUE(ask)) {
        rstudioapi::executeCommand("clearUserPrefs", quiet = TRUE)
      }
    },

    "bio-default" = ,
    "bio-dark-blue" = ,
    "bio-black" = {
      # Change the default dir, if default UI preferences change
      fs::dir_create("~/R/main", recurse = TRUE)

      file_default <- get_path_rstudio_config_file(which = "bio")
      success <- rstudio_set_preferences(file_default)

    },

    usethis::ui_stop(paste0(
      "Unknown option of user setting defaults: to = {usethis::ui_value(to[1])}. \n",
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
    ui_msg_restart_rstudio()
    # rstudio_reload_ui()

  } else {
    usethis::ui_oops("Failure to reset RStudio user settings.")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read preference from JSON file and set them in RStudio
normalize_cran_mirror_pref <- function(value) {
  has_https_url <- function(x) {
    is.character(x) && length(x) >= 1L && nzchar(x[1]) &&
      startsWith(x[1], "https://")
  }

  # Keep valid mirror objects untouched.
  if (is.list(value)) {
    if (!is.null(value$url) && has_https_url(value$url)) {
      return(value)
    }

    # Empty or malformed mirror objects are normalized to a secure default.
    return(list(
      name = "Posit Package Manager",
      url = "https://packagemanager.posit.co/cran/latest"
    ))
  }

  if (is.character(value) && length(value) >= 1L && has_https_url(value)) {
    return(list(name = "CRAN", url = value[1]))
  }

  list(
    name = "Posit Package Manager",
    url = "https://packagemanager.posit.co/cran/latest"
  )
}

#' Merge a preset preferences JSON file straight into `rstudio-prefs.json`.
#'
#' Headless fallback used by [rstudio_set_preferences()] when no RStudio
#' session is available (e.g. run via `Rscript`). Values are merged directly
#' into the preferences file instead of going through
#' [rstudioapi::writeRStudioPreference()], which requires a live session.
#'
#' @param file Path to a JSON file with preferences to merge in.
#' @return Logical scalar, `TRUE` on success.
#' @keywords internal
rstudio_merge_preferences_file <- function(file) {
  preset <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  preset_names <- names(preset)
  if (is.null(preset_names)) {
    return(TRUE)
  }

  target <- get_path_rstudio_config_file("current")
  current <-
    if (fs::file_exists(target)) {
      jsonlite::fromJSON(target, simplifyVector = FALSE)
    } else {
      list()
    }
  if (is.null(current)) {
    current <- list()
  }

  for (nm in preset_names[nzchar(preset_names)]) {
    value <- preset[[nm]]
    if (identical(nm, "cran_mirror")) {
      value <- normalize_cran_mirror_pref(value)
    }
    current[[nm]] <- value
  }

  fs::dir_create(fs::path_dir(target), recurse = TRUE)
  jsonlite::write_json(current, target, auto_unbox = TRUE, pretty = TRUE, null = "null")
  TRUE
}

rstudio_set_preferences <- function(file) {
  if (rstudioapi::isAvailable("1.3.387")) {
    pref <- jsonlite::fromJSON(file)

    pref_names <- names(pref)
    if (is.null(pref_names)) {
      return(TRUE)
    }

    valid_idx <- which(nzchar(pref_names))

    purrr::walk2(
      pref_names[valid_idx], unname(pref)[valid_idx],
      ~ {
        pref_name <- .x
        pref_value <- .y

        if (identical(pref_name, "cran_mirror")) {
          pref_value <- normalize_cran_mirror_pref(pref_value)
        }

        tryCatch(
          rstudioapi::writeRStudioPreference(pref_name, pref_value),
          error = function(e) {
            e_msg <- e$message
            if (stringr::str_detect(e_msg, "expected <Integer>")) {
              rstudioapi::writeRStudioPreference(pref_name, as.integer(pref_value))
            } else if (stringr::str_detect(e_msg, "expected <Real>")) {
              rstudioapi::writeRStudioPreference(pref_name, as.numeric(pref_value))
            } else if (stringr::str_detect(e_msg, "expected <Array>")) {
              rstudioapi::writeRStudioPreference(pref_name, as.list(pref_value))
            } else {
              print(glue::glue("'In {pref_name}' = {pref_value}\n{e}\n"))
            }
          }
        )
      }
    )
    TRUE

  } else {
    rstudio_merge_preferences_file(file)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

normalize_rstudio_preference_value <- function(value) {
  if (is.integer(value)) {
    return(as.numeric(value))
  }

  if (!is.list(value)) {
    return(value)
  }

  if (length(value) == 0L && is.null(names(value))) {
    return(character())
  }

  if (
    is.null(names(value)) &&
      all(vapply(value, function(item) !is.list(item) && length(item) == 1L, logical(1)))
  ) {
    return(unlist(value, use.names = FALSE))
  }

  lapply(value, normalize_rstudio_preference_value)
}

#' Format a preference value for display in a one-line diff summary.
#' @keywords internal
format_pref_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.list(x)) {
    return(paste0("<list, length ", length(x), ">"))
  }
  paste(utils::head(x, 5L), collapse = ", ")
}

#' Recursively compare two (possibly nested) named preference lists.
#'
#' @return A data frame with one row per compared key: `path`, `status`
#'   (one of `"identical"`, `"different"`, `"missing_in_current"`,
#'   `"missing_in_default"`), and formatted `default`/`current` values.
#' @keywords internal
summarize_pref_diff <- function(default_prefs, current_prefs, parent = character()) {
  all_names <- union(names(default_prefs), names(current_prefs))

  rows <- purrr::map(all_names, function(nm) {
    path <- paste(c(parent, nm), collapse = "$")
    has_default <- nm %in% names(default_prefs)
    has_current <- nm %in% names(current_prefs)

    d_val <- if (has_default) default_prefs[[nm]] else NULL
    c_val <- if (has_current) current_prefs[[nm]] else NULL

    if (!has_current) {
      return(data.frame(
        path = path, status = "missing_in_current",
        default = format_pref_value(d_val), current = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    if (!has_default) {
      return(data.frame(
        path = path, status = "missing_in_default",
        default = NA_character_, current = format_pref_value(c_val),
        stringsAsFactors = FALSE
      ))
    }

    if (is.list(d_val) && is.list(c_val) && !is.null(names(d_val))) {
      return(summarize_pref_diff(d_val, c_val, c(parent, nm)))
    }

    status <- if (identical(d_val, c_val)) "identical" else "different"
    data.frame(
      path = path, status = status,
      default = format_pref_value(d_val), current = format_pref_value(c_val),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Print a concise summary of `summarize_pref_diff()` output.
#'
#' @param details (logical) If `FALSE`, print only the match/difference
#'   counts (no per-key breakdown).
#' @keywords internal
print_pref_diff_summary <- function(diff_df, x_arg, y_arg, details = TRUE) {
  n_total <- nrow(diff_df)
  same <- diff_df[diff_df$status == "identical", , drop = FALSE]
  diffs <- diff_df[diff_df$status != "identical", , drop = FALSE]

  usethis::ui_info(
    "Comparing {usethis::ui_value(y_arg)} RStudio settings to {usethis::ui_value(x_arg)} ({n_total} keys)."
  )

  if (nrow(diffs) == 0L) {
    usethis::ui_done("All settings match.")
    return(invisible(diff_df))
  }

  usethis::ui_done("{nrow(same)} settings match.")

  if (!isTRUE(details)) {
    usethis::ui_oops("{nrow(diffs)} difference(s) found.")
    return(invisible(diff_df))
  }

  usethis::ui_oops("{nrow(diffs)} difference(s) found:")

  missing_current <- diffs[diffs$status == "missing_in_current", , drop = FALSE]
  missing_default <- diffs[diffs$status == "missing_in_default", , drop = FALSE]
  changed <- diffs[diffs$status == "different", , drop = FALSE]

  if (nrow(missing_current) > 0L) {
    cat("\n  Not set / unsupported in", y_arg, paste0("(", nrow(missing_current), "):\n"))
    cat(
      paste0("    - ", missing_current$path, " (", x_arg, " = ", missing_current$default, ")"),
      sep = "\n"
    )
  }

  if (nrow(missing_default) > 0L) {
    cat("\n  Extra in", y_arg, paste0("(", nrow(missing_default), "):\n"))
    cat(
      paste0("    - ", missing_default$path, " (", y_arg, " = ", missing_default$current, ")"),
      sep = "\n"
    )
  }

  if (nrow(changed) > 0L) {
    cat("\n  Changed values", paste0("(", nrow(changed), "):\n"))
    cat(
      paste0(
        "    - ", changed$path, ": ", x_arg, " = ", changed$default,
        "  |  ", y_arg, " = ", changed$current
      ),
      sep = "\n"
    )
  }

  cat("\n")
  usethis::ui_todo(paste0(
    "Many \"not set\" entries are pseudo-differences: preferences unsupported by ",
    "your installed RStudio version, or ones that need a restart/manual step to ",
    "register. Run with `output = \"verbose\"` for the full diff."
  ))

  invisible(diff_df)
}

#' Show differences in sets of settings
#'
#' @param to One of: "bio-default", "rstudio-default"
#'        (or an unambiguous abbreviation of these).
#' @param output One of:
#'        - `"minimal"`: print only the match/difference counts.
#'        - `"concise"` (default): print how many settings match, plus a
#'          short list of what differs.
#'        - `"verbose"`: fall back to the full `waldo::compare()` output
#'          (useful for deep debugging, but can be very verbose for large
#'          preference sets).
#'
#' @return Invisibly, a data frame of per-key comparison results
#'         (`"concise"`/`"minimal"`), or the `waldo::compare()` result
#'         (`"verbose"`). Settings, which are not in `to` list, will not be
#'         displayed at all.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   rstudio_compare_user_settings(to = "bio-default")
#'   rstudio_compare_user_settings(to = "rstudio-default")
#'   rstudio_compare_user_settings(to = "bio-default", output = "minimal")
#'   rstudio_compare_user_settings(to = "bio-default", output = "verbose")
#' }
rstudio_compare_user_settings <- function(to = "bio-default", output = "concise") {
  to <- match.arg(to, c("bio-default", "rstudio-default"))
  output <- match.arg(output, c("concise", "minimal", "verbose"))

  file <- get_path_rstudio_config_file(which = to)
  default_prefs <-
    jsonlite::fromJSON(file, simplifyVector = FALSE) |>
    purrr::map(normalize_rstudio_preference_value)

  pref_names <- names(default_prefs) |> purrr::set_names()
  current_prefs <-
    purrr::map(pref_names, ~ rstudioapi::readRStudioPreference(., NULL)) |>
    purrr::map(normalize_rstudio_preference_value)

  if (output == "verbose") {
    usethis::ui_info(
      "Show differences between {green('current')} and {green(to)} setting lists.\n"
    )

    # Unify names and number of fields
    all_names <- unique(names(current_prefs), names(default_prefs))
    named_list <- setNames(vector("list", length(all_names)), all_names)

    default_prefs <- utils::modifyList(named_list, default_prefs, keep.null = TRUE)
    current_prefs <- utils::modifyList(named_list, current_prefs, keep.null = TRUE)

    return(waldo::compare(
      default_prefs, current_prefs,
      x_arg = to, y_arg = "current",
      max_diffs = Inf,
      list_as_map = TRUE
    ))
  }

  diff_df <- summarize_pref_diff(default_prefs, current_prefs)
  print_pref_diff_summary(diff_df, x_arg = to, y_arg = "current", details = output == "concise")
}
