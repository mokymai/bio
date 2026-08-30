# General ====================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct path and check if it exists
#'
#' @param base (character) The base for the path name.
#' @param ... (character) Parts of the path.
#'
#' @return Path or error if the path does not exist.
#'
#' @concept paths and dirs
#' @keywords internal
#' @noRd
#'
#' @examples
#' path_construct_and_check(".")
#'
#' if (interactive()) {
#'   # Expect error:
#'   path_construct_and_check("uiuuuu")
#' }
path_construct_and_check <- function(base, ...) {
  file <- fs::path(base, ...)

  if (fs::file_exists(file)) {
    file
  } else {
    usethis::ui_stop("The path does not exist: \n{usethis::ui_path(file)}")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on: usethis:::scoped_path_r()
scoped_path_r <- function(scope = c("user", "project"), ..., envvar = NULL) {
  scope <- match.arg(scope)
  if (scope == "user" && !is.null(envvar)) {
    env <- Sys.getenv(envvar, unset = "")
    if (!identical(env, "")) {
      return(fs::path_expand(env))
    }
  }
  root <- switch(
    scope,
    user    = fs::path_home_r(),
    project = usethis::proj_get()
  )
  fs::path(root, ...)
}

open_path <- function(path) {
  utils::browseURL(path)
}


# Path to Desktop ============================================================

#' @title Path to Desktop Folder
#' @description Get path to desktop folder of current user and open it.
#'
#' @param ... (character) file or folder name on desktop.
#'
#' @return String with path to desktop or path to file or folder on a desktop.
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @examples
#' get_path_desktop()
get_path_desktop <- function(...) {
  fs::path(fs::path_expand("~/Desktop"), ...)
}

#' @rdname get_path_desktop
#' @export
open_desktop <- function() {
  open_path(get_path_desktop())
}


# RStudio-related-dirs =======================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get path to RStudio configuration directory.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name RStudio-related-dirs
#' @title Directories of RStudio-Related Files
#' @description
#' Directories of RStudio (desktop) settings, preferences and other files.
#'
#' @concept paths and dirs
#' @seealso
#' - [fs::file_show()], [browseURL()],
#' - [rstudioapi::navigateToFile()],
#' - [utils::file.edit()]

NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @description
#' - `get_path_rstudio_config_dir()`` - gets path to RStudio configuration
#'   directory (and its sub-directories).
#'
#' @param ... (character) Parts of the path. Path to sub-directories.
#'
#' @param .check (logical) If `TRUE`, additionally checks for path existence.
#'
#' @return (string) path to RStudio configuration directory.
#'         When `.check = TRUE`, returns an error if the path does not exist.
#'
#' @seealso
#' - `get_path_rstudio_config_dir()`:
#' https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   get_path_rstudio_config_dir()
#'
#'   get_path_rstudio_config_dir("dictionaries")
#' }
get_path_rstudio_config_dir <- function(..., .check = FALSE) {
  # https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
  # RStudio's configuration is per OS user, regardless of application install scope.

  # styler: off
  # nolint start
  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("APPDATA"), "RStudio"),
      "linux"   = fs::path_expand_r("~/.config/rstudio"),
      "mac"     = fs::path_expand_r("~/.config/rstudio"),
                  fs::path_expand_r("~/.config/rstudio")  # Other OS'es
    )
  # nolint end
  # styler: on

  xdg_config <- Sys.getenv("XDG_CONFIG_HOME", unset = "")
  if (nzchar(xdg_config)) {
    base <- fs::path(xdg_config, "rstudio")
  }

  rstudio_config <- Sys.getenv("RSTUDIO_CONFIG_HOME", unset = "")
  if (nzchar(rstudio_config)) {
    base <- rstudio_config
  }

  if (.check) {
    path_construct_and_check(base, ...)

  } else {
    fs::path(base, ...)
  }
}

# `Sys.getenv(x, unset = default)` only falls back to `default` when `x` is
# not set at all, not when it is set to an empty string. Treat both the same.
env_var_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @export
get_path_rstudio_internal_state_dir <- function(..., .check = FALSE) {
  # https://support.posit.co/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
  # Section:
  # Accessing the RStudio-Desktop Directory (Internal State)
  #
  # Windows:       %localappdata%\RStudio-Desktop
  # Linux/Mac:     ~/.rstudio-desktop


  # bio supports RStudio 2026.08+, which uses these current state directories.
  # nolint start
  # styler: off
  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
      "linux"   = fs::path_expand_r("~/.local/share/rstudio"),
      "mac"     = fs::path_expand_r("~/.local/share/rstudio"),
                  fs::path_expand_r("~/.local/share/rstudio")
    )
  # nolint end
  # styler: on

  if (.check) {
    path_construct_and_check(base, ...)

  } else {
    fs::path(base, ...)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @concept paths and dirs
#' @export
#' @examples
#' if (interactive()) {
#'   get_path_rstudio_keybindings_dir()
#' }
get_path_rstudio_keybindings_dir <- function() {
  get_path_rstudio_config_dir("keybindings")
}


# ===========================================================================~
# Open Directories ==========================================================
# ===========================================================================~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_config_dir <- function() {
  open_path(get_path_rstudio_config_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_internal_state_dir <- function() {
  open_path(get_path_rstudio_internal_state_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_keybindings_dir <- function() {
  open_path(get_path_rstudio_keybindings_dir())
}



# ===========================================================================~
# Open files ================================================================
# ===========================================================================~

#' @name RStudio-config-file
#' @title Manage RStudio Configuration (Preferences) File
#' @description Manage file with RStudio configuration (user preferences).
#' @param which (character) type of settings:
#'  - "current": file with current RStudio settings (that differ from the defaults);
#'  - "bio-default": file with setting from "bio-default" list (except theme);
#'  - "rstudio-default": a preset compiled from the most recent RStudio
#'    settings documentation available when it was downloaded.
#'    For comparisons, [rstudio_compare_user_settings()] fills unset values
#'    from the local RStudio `user-prefs-schema.json` when available.
#'
#' @export
#' @concept paths and dirs
#'
#' @seealso
#' - [get_path_rstudio_config_dir()]
#'
#' @examples
#' if (interactive()) {
#'   get_path_rstudio_config_file()
#'
#'   get_path_rstudio_config_file("bio-default")
#' }
get_path_rstudio_config_file <- function(which = "current") {

  if (which == "current") {
    get_path_rstudio_config_dir("rstudio-prefs.json")

  } else if (stringr::str_detect(which, "^bio$|^bio-")) {
    system.file(
      "rs-settings", "rstudio-prefs--bio-default.json", package = "bio"
    )

  } else if (stringr::str_detect(which, "^rstudio$|^rstudio-")) {
    system.file(
      "rs-settings", "rstudio-prefs--rstudio-default.json", package = "bio"
    )

  } else {
    stop("Unknown value: ", which)
  }
}

#' @rdname RStudio-config-file
#' @export
open_rstudio_config_file <- function(which = "current") {
  open_in_rstudio(path = get_path_rstudio_config_file(which = which))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open r_environ =============================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name open_r_environ
#' @export
#' @title Open `.Renviron` File
#' @description
#' Functions to get path to and open `.Renviron` file that contains
#' definitions of R environment variables.
#'
#' Compared to  [usethis::edit_r_environ()], `open_r_environ()` does not create
#' file if it does not exist.
#'
#' @param scope (character) The scope of file. One of "user" or "project".
#'
#' @concept paths and dirs
#' @seealso
#' - [usethis::edit_r_environ()]
#'
#' @examples
#' get_path_r_environ()
#'
get_path_r_environ <- function(scope = c("user", "project")) {
  scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
}

#' @rdname open_r_environ
#' @export
open_r_environ <- function() {
  open_path(get_path_r_environ())
}
