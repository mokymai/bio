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
#' https://docs.posit.co/ide/server-pro/session-user-settings.html
#'
#' @seealso
#' [get_path_rstudio_config_file()]
#'
#'
#' On [Customizing RStudio](https://support.posit.co/hc/en-us/articles/200549016-Customizing-the-RStudio-IDE) using point-and-click method.
#'
#' On [Configuration and Settings](https://www.rstudio.com/blog/rstudio-1-3-preview-configuration/).
#'
#' A list of [Session User Settings](https://docs.posit.co/ide/server-pro/session_user_settings/session_user_settings.html) to be used with
#' [rstudioapi::writeRStudioPreference()].
#'
#' On [RStudio setting locations](https://docs.posit.co/ide/desktop-pro/settings/settings.html).
#'
#' On [Resetting RStudio Desktop's State](https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State).
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
      if (isTRUE(ask) && rstudioapi::isAvailable()) {
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


  # Change RStudio theme (only possible in a live RStudio session)
  if (rstudioapi::isAvailable()) {
    switch(
      to,
      "bio-default"   = rstudioapi::applyTheme("Textmate (default)"),
      "bio-dark-blue" = rstudioapi::applyTheme("Cobalt"),
      "bio-black"     = rstudioapi::applyTheme("Chaos")
    )
  }

  if (isTRUE(success)) {
    usethis::ui_done("RStudio user settings were set to {green(to)}.")
    ui_msg_restart_rstudio()
    # It might be also needed to rstudio_reload_ui()

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
#' Headless fallback used by `rstudio_set_preferences()` when no RStudio
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

# Format a preference value for display in a one-line diff summary.
format_pref_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.list(x)) {
    return(paste0("<list, length ", length(x), ">"))
  }
  paste(utils::head(x, 5L), collapse = ", ")
}

# Recursively compare two (possibly nested) named preference lists.
# Returns a data frame with one row per compared key in `default_prefs`:
# `path`, `status` ("identical" / "different" / "missing_in_current"),
# and formatted `default`/`current` values. Settings outside `default_prefs`
# are intentionally excluded to keep comparison results focused on `to`.
summarize_pref_diff <- function(default_prefs, current_prefs, parent = character()) {
  # all_names <- union(names(default_prefs), names(current_prefs))
  all_names <- names(default_prefs)
  if (is.null(all_names)) {
    return(data.frame(
      path = character(), status = character(),
      default = character(), current = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- purrr::map(all_names, function(nm) {
    path <- paste(c(parent, nm), collapse = "$")
    has_current <- nm %in% names(current_prefs)

    d_val <- default_prefs[[nm]]
    c_val <- if (has_current) current_prefs[[nm]] else NULL

    if (!has_current) {
      return(data.frame(
        path = path, status = "missing_in_current",
        default = format_pref_value(d_val), current = NA_character_,
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

  if (length(rows) == 0L) {
    return(data.frame(
      path = character(), status = character(),
      default = character(), current = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

# Print a concise summary of `summarize_pref_diff()` output.
# `details = FALSE` prints only the match/difference counts.
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
    cat("\n  Not set in", paste0(y_arg, " (", nrow(missing_current), "):\n"))
    cat(
      paste0("    - ", missing_current$path, " (", x_arg, " = ", missing_current$default, ")"),
      sep = "\n"
    )
  }

  if (nrow(missing_default) > 0L) {
    cat("\n  Extra in", paste0(y_arg, " (", nrow(missing_default), "):\n"))
    cat(
      paste0("    - ", missing_default$path, " (", y_arg, " = ", missing_default$current, ")"),
      sep = "\n"
    )
  }

  if (nrow(changed) > 0L) {
    cat("\n  Different values", paste0("(", nrow(changed), "):\n"))
    cat(
      paste0(
        "    - ", changed$path, " (", x_arg, " = ", changed$default,
        ", ", y_arg, " = ", changed$current, ")"
      ),
      sep = "\n"
    )
  }

  cat("\n")
  usethis::ui_todo("\"Not set\" often just means RStudio doesn't know that setting yet (older/newer version) or it needs a restart/manual step.")
  usethis::ui_todo("Run with `output = \"verbose\"` for the full technical diff.")

  invisible(diff_df)
}

# Read and normalize an RStudio preferences JSON file.
read_pref_file <- function(file) {
  jsonlite::fromJSON(file, simplifyVector = FALSE) |>
    purrr::map(normalize_rstudio_preference_value)
}

# Path to the bundled `user-prefs-schema.json` (documents RStudio's built-in
# default value for each preference), if the local install can be found.
find_rstudio_prefs_schema_file <- function(install_dir = find_rstudio_install_dir()) {
  if (is.null(install_dir)) {
    return(NULL)
  }

  candidates <- c(
    fs::path(install_dir, "resources", "app", "resources", "schema", "user-prefs-schema.json"),
    fs::path(install_dir, "Contents", "Resources", "app", "resources", "schema", "user-prefs-schema.json"),
    fs::path(install_dir, "resources", "schema", "user-prefs-schema.json")
  )
  candidates <- candidates[fs::file_exists(candidates)]
  if (length(candidates) == 0L) NULL else candidates[[1]]
}

# Named list of RStudio's built-in default preference values, read from the
# local installation's schema file. `NULL` if no schema file can be found.
get_rstudio_prefs_schema_defaults <- function() {
  schema_file <- find_rstudio_prefs_schema_file()
  if (is.null(schema_file)) {
    return(NULL)
  }

  schema <- tryCatch(
    jsonlite::fromJSON(schema_file, simplifyVector = FALSE),
    error = function(e) NULL
  )
  props <- schema[["properties"]]
  if (is.null(props)) {
    return(NULL)
  }

  defaults <- purrr::map(props, "default")
  defaults <- defaults[!vapply(defaults, is.null, logical(1))]
  purrr::map(defaults, normalize_rstudio_preference_value)
}

# Read "current" preference values live from an active RStudio session.
read_current_prefs_live <- function(pref_names) {
  pref_names |>
    purrr::set_names() |>
    purrr::map(~ rstudioapi::readRStudioPreference(., NULL)) |>
    purrr::map(normalize_rstudio_preference_value)
}

# Recursively fill keys/sub-keys that are present in `wanted` (the settings
# we're actually comparing against) but missing from `current`, using the
# matching value from `defaults`. Never adds a key `wanted` doesn't have, so
# schema keys the `to` preset doesn't care about are ignored. Returns the
# number of top-level-or-nested keys filled as the `"n_filled"` attribute.
fill_missing_defaults <- function(current, defaults, wanted) {
  n_filled <- 0L

  for (nm in names(wanted)) {
    default_val <- defaults[[nm]]
    if (is.null(default_val)) {
      next # Not in the schema either; leave as-is.
    }

    if (!nm %in% names(current)) {
      current[[nm]] <- default_val
      n_filled <- n_filled + 1L
      next
    }

    is_nested <- is.list(current[[nm]]) && is.list(default_val) && is.list(wanted[[nm]]) &&
      !is.null(names(current[[nm]])) && !is.null(names(default_val))
    if (is_nested) {
      nested <- fill_missing_defaults(current[[nm]], default_val, wanted[[nm]])
      current[[nm]] <- nested
      n_filled <- n_filled + attr(nested, "n_filled", exact = TRUE)
      attr(current[[nm]], "n_filled") <- NULL
    }
  }

  attr(current, "n_filled") <- n_filled
  current
}

# Read "current" preference values from the saved `rstudio-prefs.json` file,
# filling in keys/sub-keys missing from `default_prefs` with the local
# install's schema defaults (the file only stores values overridden from
# RStudio's built-in defaults, so unset keys would otherwise look "missing").
read_current_prefs_from_file <- function(current_file, default_prefs) {
  current_prefs <- read_pref_file(current_file)

  schema_defaults <- get_rstudio_prefs_schema_defaults()
  if (is.null(schema_defaults)) {
    usethis::ui_info(paste0(
      "Could not find your local RStudio installation's list of built-in ",
      "default settings, so unchanged settings may be shown as \"not set\" below."
    ))
    return(current_prefs)
  }

  filled <- fill_missing_defaults(current_prefs, schema_defaults, default_prefs)
  n_filled <- attr(filled, "n_filled", exact = TRUE)
  attr(filled, "n_filled") <- NULL

  if (n_filled > 0L) {
    usethis::ui_info(
      "Filled in {n_filled} setting(s) left at RStudio's built-in default value."
    )
  }

  filled
}

#' Show differences in sets of settings
#'
#' @param to One of: "bio-default", "rstudio-default"
#'        (or an unambiguous abbreviation of these).
#' @param source One of:
#'        - `"auto"` (default): use a live RStudio session if one is
#'          running, otherwise fall back to the saved preferences file.
#'        - `"live"`: read "current" settings live via
#'          [rstudioapi::readRStudioPreference()]; fails gracefully if
#'          RStudio is not running.
#'        - `"file"`: always read "current" settings from the saved
#'          `rstudio-prefs.json` file on disk, even if RStudio is running.
#' @param output One of:
#'        - `"minimal"`: print only the match/difference counts.
#'        - `"concise"` (default): print how many settings match, plus a
#'          short list of what differs.
#'        - `"verbose"`: fall back to the full `waldo::compare()` output
#'          (useful for deep debugging, but can be very verbose for large
#'          preference sets).
#'
#' @details
#' `source = "live"` (or `"auto"` with RStudio running) reads "current"
#' settings live via [rstudioapi::readRStudioPreference()].
#' `source = "file"` (or `"auto"` without RStudio running) reads the saved
#' `rstudio-prefs.json` file on disk (see [get_path_rstudio_config_file()]).
#' Since that file only stores values overridden from RStudio's built-in
#' defaults, keys left at their default are also filled in (when possible)
#' from the local RStudio installation's `user-prefs-schema.json`, so they
#' aren't misreported as "missing". The file-based comparison may still not
#' reflect unsaved, in-memory session state.
#'
#' @return Invisibly, a data frame of per-key comparison results
#'         (`"concise"`/`"minimal"`), or the `waldo::compare()` result
#'         (`"verbose"`). Settings, which are not in `to` list, will not be
#'         displayed at all. Returns `invisible(NULL)` if the requested
#'         `source` is unavailable (e.g. `"live"` without a running RStudio
#'         session, or `"file"`/`"auto"` with no saved preferences file).
#' @export
#'
#' @examples
#' if (interactive()) {
#'   rstudio_compare_user_settings(to = "bio-default")
#'   rstudio_compare_user_settings(to = "rstudio-default")
#'   rstudio_compare_user_settings(to = "bio-default", source = "file")
#'   rstudio_compare_user_settings(to = "bio-default", output = "minimal")
#'   rstudio_compare_user_settings(to = "bio-default", output = "verbose")
#' }
rstudio_compare_user_settings <- function(to = "bio-default", source = "auto", output = "concise") {
  to <- match.arg(to, c("bio-default", "rstudio-default"))
  source <- match.arg(source, c("auto", "live", "file"))
  output <- match.arg(output, c("concise", "minimal", "verbose"))

  default_prefs <- read_pref_file(get_path_rstudio_config_file(which = to))
  current_file <- get_path_rstudio_config_file("current")
  live_available <- rstudioapi::isAvailable()
  use_live <- if (source == "auto") live_available else source == "live"

  if (use_live && !live_available) {
    usethis::ui_oops(paste0(
      "RStudio is not running. `source = \"live\"` requires an active ",
      "RStudio session; use `source = \"file\"` or `source = \"auto\"` instead."
    ))
    return(invisible(NULL))
  }

  if (use_live) {
    current_prefs <- read_current_prefs_live(names(default_prefs))

  } else if (fs::file_exists(current_file)) {
    intro <- if (live_available) {
      "Comparing against the saved preferences file, "
    } else {
      "RStudio is not running; comparing against the saved preferences file, "
    }
    usethis::ui_info(paste0(intro, "not a live session ({usethis::ui_path(current_file)})."))
    current_prefs <- read_current_prefs_from_file(current_file, default_prefs)

  } else {
    usethis::ui_oops(paste0(
      "No saved preferences file was found at {usethis::ui_path(current_file)}",
      if (!live_available) " and RStudio is not running." else "."
    ))
    return(invisible(NULL))
  }

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
