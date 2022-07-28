# Meld =======================================================================

# @name meld
# @title Helper functions for "Meld"
#
# @export
# @concept programs
#
# @examples
# is_meld_installed()
browse_meld_homepage <- function() {
  browseURL("https://meldmerge.org/")
}

# @rdname meld
# @export
# @concept programs
download_meld <- function() {
  switch(get_os_type(),
    windows = browseURL("https://download.gnome.org/binaries/win32/meld/3.20/Meld-3.20.4-mingw.msi"),
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

# @rdname meld
# @export
# @concept programs
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
      # "mac"     = "/usr/bin/meld", # <- neašku, kaip Mac'e

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

# @rdname meld
# @export
# @concept programs
is_meld_installed <- function(path_to_meld = get_default_path_to_meld()) {
  file.exists(path_to_meld)
}

# @rdname meld
#
# @param path_to_meld (character|`NULL`)
# @param prompt (logical)
# @param keepBackup (logical)
# @param trustExitCode (logical)
#
# @export
# @concept programs
set_meld_as_git_difftool_mergetool <- function(path_to_meld = NULL,
  prompt = FALSE, keepBackup = FALSE, trustExitCode = TRUE) {

  checkmate::assert_string(path_to_meld, null.ok = TRUE)
  checkmate::assert_flag(prompt)
  checkmate::assert_flag(keepBackup)
  checkmate::assert_flag(trustExitCode)

  if (!is_git_installed()) {
    stop("Git is either not installed or is not properly configured.")
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
