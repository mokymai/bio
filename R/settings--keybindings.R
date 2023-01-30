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
      'Unknown type of keybindings: to = {usethis::ui_value(to[1])}. \n',
      "Possible options: {ui_value(user_setting_set_names)}."
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
