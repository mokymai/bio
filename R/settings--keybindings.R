# Keybindings ================================================================

# For auto-completion
keybindings_defaults   <- c("bio-default", "rstudio-default")

#' Reset RStudio keybindings to a packaged preset.
#'
#' This helper copies the packaged keybindings files into the user-level
#' RStudio folder, or removes the current keybinding files when the preset is
#' `"rstudio-default"`.
#'
#' @param to String scalar. Supported values are `"bio-default"` and
#'   `"rstudio-default"`.
#' @param backup Logical scalar. If `TRUE`, a backup copy of the current
#'   keybinding files is created before resetting.
#'
#' @return Invisibly returns `NULL` after resetting the keybindings.
#' @export
#'
#' @concept r and rstudio settings
#'
#' @examples
#' if (interactive()) {
#'   bio::rstudio_reset_keybindings(to = "bio-default")
#'   bio::rstudio_reload_ui()
#' }
rstudio_reset_keybindings <- function(to, backup = TRUE) {

  if (missing(to)) {
    ui_stop(paste0(
      "The value of argument '{yellow('to')}' is missing.\n",
      "Possible options: {ui_value(keybindings_defaults)}."
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
      "Unknown type of keybindings: to = {usethis::ui_value(to[1])}. \n",
      "Possible options: {ui_value(keybindings_defaults)}."
    ))
  )

  # Create back-up copies
  if (isTRUE(backup)) {
    backup.tools::create_backup_copy(current_files, "keybindings", "shortcut keys")
  }

  # Reset current keybindings
  switch(
    to,
    "rstudio-default" = {
      # RStudio defaults are set when setup files are deleted
      fs::file_delete(current_files)
    },    {
      # To set other options, files must be copied
      fs::dir_create(fs::path_dir(current_files), recurse = TRUE)
      fs::file_copy(from_files, current_files, overwrite = TRUE)
    }
  )

  # Output message
  ui_done("Shortcut keys were reset to {green(to)}.")
}
