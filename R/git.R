# path_npp <- "C:/Program Files (x86)/Notepad++/notepad++.exe"

# GIT ========================================================================

# ==========================================================================~~
# Kodas, kuris konfigūruoja git username ir password

# git config --global user.name  "Vardas Pavarde"
# git config --global user.email "el@pastas.lt"
#
# usethis::use_git_config(
#   user.name  = "Vardas Pavarde",
#   user.email = "el@pastas.lt"
# )
# ==========================================================================~~

# @name git
# @title Helper functions for "Git"
# @description
# Helper functions for "Git".
# @concept programs
NULL


# @rdname git
# @importFrom usethis edit_git_config
# @export
# @concept programs
usethis::edit_git_config
# usethis::edit_git_config("user")

# @rdname git
# @importFrom usethis use_git_config
# @export
# @concept programs
usethis::use_git_config


# @rdname git
#
# @param core_editor (character) Name of Git core editor.
# TODO: pasirinkti programą, kuri atidaro git config failus [core] editor
# https://help.github.com/en/articles/associating-text-editors-with-git
#
# @export
# @concept programs

get_git_core_editor_cmd <- function(core_editor) {
  core_editor_cmd <- switch(
    get_os_type(),
    windows = switch(
      core_editor,
      npp  = "'C:/Program Files (x86)/Notepad++/notepad++.exe' -multiInst -notabbar -nosession -noPlugin",
      GitExtensions = "'C:/Program Files (x86)/GitExtensions/GitExtensions.exe' fileeditor"
    )
  )
  glue::glue('git config --global core.editor "{core_editor_cmd}"')
}


# Check if Git is installed
#
# @return logical value.
# @export
# @concept programs
#
# @examples
# is_git_installed()
is_git_installed <- function() {
  # suppressWarnings(
  #   system("git --version", show.output.on.console = FALSE) == 0
  # )
  tryCatch(
    {
      system2("git", "--version", stdout = TRUE, stderr = TRUE)
      # If no error occurs in system2(), TRUE is returned.
      TRUE
    },

    error = function(e) {
      FALSE
    }
  )
}

# Get path to Git.
#
# @return Path to Git as string of NULL, if Git is not installed or not properly configured.
# @export
# @concept programs
#
# @examples
# get_path_to_git()
get_path_to_git <- function() {
  if (is_git_installed()) {
    # cmd <- switch(get_os_type(), "windows" = "where git", "which git")
    # file <- fs::path(system(cmd, intern = TRUE, show.output.on.console = FALSE))
    cmd <- switch(get_os_type(), "windows" = "where", "which")
    file <- fs::path(system2(cmd, "git", stdout = TRUE))
    # Replace cmd to bin on Windows
    sub("/cmd/git.exe$", "/bin/git.exe", file)

  } else {
    NULL
  }
}
