# path_npp <- "C:/Program Files (x86)/Notepad++/notepad++.exe"

# RStudio version control ====================================================

#' @name version-control
#' @title Version control in RStudio.
#' @description
#' Helper functions for version control system (vcs) in RStudio.
#' - `open_rs_proj_vcs_opts()` opens version control setup for RStudio project.
#' - `show_rs_vcs_rsa_key()` shows version RSA key.
#' - `browse_rs_vcs_help()` opens website with help of version control in RStudio .
#' @export
# open_rs_verion_control_opts <- function() {
open_rs_proj_vcs_opts <- function() {
  rstudioapi::executeCommand("versionControlProjectSetup", quiet = TRUE)
}

#' @name version-control
#' @export
show_rs_vcs_rsa_key <- function() {
  rstudioapi::executeCommand("versionControlShowRsaKey", quiet = TRUE)
}

#' @name version-control
#' @export
browse_rs_vcs_help <- function() {
  rstudioapi::executeCommand("versionControlHelp", quiet = TRUE)
}



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
#' @name git
#' @title Helper functions for "Git".
#' @description
#' Helper functions for "Git".
NULL


# @rdname git
#' @importFrom usethis edit_git_config
#' @export
usethis::edit_git_config
# usethis::edit_git_config("user")

# @rdname git
#' @importFrom usethis use_git_config
#' @export
usethis::use_git_config


#' @rdname git
#' @export
#' @param core_editor (character) Name of Git core editor.
# TODO: pasirinkti programą, kuri atidaro git config failus [core] editor
# https://help.github.com/en/articles/associating-text-editors-with-git

get_git_core_editor_cmd <- function(core_editor = "atom") {
  # core_editor = "atom"  # "atom", "npp", "GitExtensions"
  core_editor_cmd <- switch(
    get_os_type(),
    windows = switch(
      core_editor,
      atom = 'atom --wait',
      npp  = "'C:/Program Files (x86)/Notepad++/notepad++.exe' -multiInst -notabbar -nosession -noPlugin",
      GitExtensions = "'C:/Program Files (x86)/GitExtensions/GitExtensions.exe' fileeditor"
    )
  )
  glue::glue('git config --global core.editor "{core_editor_cmd}"')
}


#' Check if GIT is installed.
#'
#' @return logical value.
#' @export
#'
#' @examples
#' is_git_installed()
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

#' Get path to Git.
#'
#' @return Path to Git as string of NULL, if Git is not installed or not configured properly.
#' @export
#'
#' @examples
#' get_path_to_git()
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


# Meld =======================================================================

#' @name meld
#' @title Helper functions for "Meld".
#'
#' @export
#'
#' @examples
#' is_meld_installed()
browse_meld_homepage <- function() {
  browseURL("https://meldmerge.org/")
}

#' @rdname meld
#' @export
download_meld <- function() {
  switch(get_os_type(),
    windows = browseURL("https://download.gnome.org/binaries/win32/meld/3.18/Meld-3.18.3-win32.msi"),
    # Otherwise
    {
      warning(
        "Downloading 'Meld' is supported on Windows only. ",
        "Trying to open homepage instead. "
      )
      bowse_meld_homepage()
    }
  )
}

#' @rdname meld
#' @export
get_default_path_to_meld <- function() {

  path_to <-
    switch(
      get_os_type(),
      # FIXME: not tested well
      "windows" =
        suppressWarnings({
          out <- system2("where", "meld", stdout = TRUE)
          if (is.null(attr(out, "status"))) {
            out
          } else {
            "C:/Program Files (x86)/Meld/meld.exe"
          }
        }),

      # "linux"   = "/usr/bin/meld",
      # "osx"     = "/usr/bin/meld", # <- neašku, kaip Mac'e

      # FIXME: Might not work if Meld is not installed
      suppressWarnings({
        out <- system2("which", "meld", stdout = TRUE)
        if (is.null(attr(out, "status"))) {
          out
        } else {
          ""
        }
      })
      # system2("which", "meld", stdout = TRUE)
      # system("which meld", intern = TRUE, show.output.on.console = FALSE)
    )
  # if (!fs::file_exists(path_to)) {
  #   usethis::ui_stop("Path to Meld was not found. ")
  # }

  fs::path(path_to)
}

#' @rdname meld
#' @export
is_meld_installed <- function(path_to_meld = get_default_path_to_meld()) {
  file.exists(path_to_meld)
}

#' @param path_to_meld (character|`NULL`)
#'
#' @param prompt (logical)
#' @param keepBackup (logical)
#' @param trustExitCode (logical)
#'
#' @rdname meld
#' @export
set_meld_as_git_difftool_mergetool <- function(path_to_meld = NULL,
  prompt = FALSE, keepBackup = FALSE, trustExitCode = TRUE) {

  checkmate::assert_string(path_to_meld, null.ok = TRUE)
  checkmate::assert_flag(prompt)
  checkmate::assert_flag(keepBackup)
  checkmate::assert_flag(trustExitCode)

  if (!is_git_installed()) {
    stop("Git is either not installed or is not configured properly.")
  }

  if (is.null(path_to_meld)) {
    path_to_meld <- get_default_path_to_meld()
  }

  if (!is_meld_installed(path_to_meld)) {
    stop("Meld is not found in: \n", path_to_meld,
      "\nChoose a correct path to Meld.")
  }

  rstudioapi::verifyAvailable("1.1.350")


  terminal_commands <- glue::glue('

git config --global merge.tool meld
git config --global mergetool.meld.path "{path_to_meld}"
git config --global mergetool.meld.cmd "\\"{path_to_meld}\\" \\"\\$LOCAL\\" \\"\\$BASE\\" \\"\\$REMOTE\\" --output \\"\\$MERGED\\""
git config --global mergetool.meld.trustExitCode {tolower(trustExitCode)}
git config --global mergetool.keepBackup {tolower(keepBackup)}

git config --global diff.guitool meld
git config --global difftool.meld.path  "{path_to_meld}"
git config --global difftool.meld.cmd "\\"{path_to_meld}\\" \\"\\$LOCAL\\" \\"\\$REMOTE\\""
git config --global difftool.meld.trustExitCode {tolower(trustExitCode)}
git config --global difftool.prompt {tolower(prompt)}

')

  terminal_id <- rstudioapi::terminalVisible()

  if (is.null(terminal_id)) {
    terminal_id <- rstudioapi::terminalCreate()
  }
  rstudioapi::terminalActivate(terminal_id, show = TRUE)
  rstudioapi::terminalSend(terminal_id, terminal_commands)
}

# Atom =======================================================================

#' @name atom
#' @title Helper functions for "Atom".
#'
#' @export
#'
#' @examples
#' is_atom_installed()
browse_atom_homepage <- function() {
  browseURL("https://atom.io/")
}

#' @rdname atom
#' @export
is_atom_installed <- function() {
  # suppressWarnings(
  #   system("atom --version", show.output.on.console = FALSE, intern = FALSE) == 0
  # )
  tryCatch(
    {
      system2("atom", "--version", stdout = TRUE, stderr = TRUE)
      # If no error occurs in system2(), TRUE is returned.
      TRUE
    },

    error = function(e) {
      FALSE
    }
  )
}

#' @rdname atom
#' @export
set_atom_as_git_core_editor <- function() {
  if (is_atom_installed()) {
    glue::glue('git config --global core.editor "atom --wait"')
  }
}

#' @rdname atom
#' @export
install_atom_meld <- function() {
  # Installs Atom package "atim-meld"
  if (is_atom_installed()) {
    system("apm install atom-meld")
  }
}

