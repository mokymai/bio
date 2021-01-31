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
#' \dontrun{\donttest{
#' # Expect error:
#' path_construct_and_check("uiuuuu")
#' }}
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
  browseURL(get_path_desktop())
}


# Get paths ==================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get path to RStudio configuration directory.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name RStudio-related-dirs
#' @title Directories of R and RStudio Related Files
#' @description
#' Directories of R and RStudio (desktop) settings, preferences and other files.
#'
#' - `get_path_r_user_dir()`-- gets path to the main R user directory.
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @seealso
#' - [fs::file_show()], [browseURL()],
#' - [rstudioapi::navigateToFile()],
#' - [utils::file.edit()]
#'
#' @examples
#' get_path_r_user_dir()
#'

# fs::path_home_r()
# # For Windows only:
# Sys.getenv("R_USER")
# fs::path(Sys.getenv("R_USER"), ".R", ...)

get_path_r_user_dir <- function(...) {
  fs::path_home_r(".R", ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @description
#' - `get_path_rstudio_config_dir()`` - gets path to RStudio configuration
#'   directory (and its sub-directories).
#'
#' @param ... (character) Parts of the path. Path to sub-directories.
#'
#' @param .check (logical) If `TRUE`, additionally checks for path existance.
#'
#' @return (string) path to RStudio configuration directory.
#'         When `.check = TRUE`, renturns error, if the path does not exist.
#'
#' @seealso
#' - `get_path_rstudio_config_dir()`:
#' https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' get_path_rstudio_config_dir()
#'
#' get_path_rstudio_config_dir("dictionaries")
#' }}
get_path_rstudio_config_dir <- function(..., .check = FALSE) {
  # https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
  # Section
  # Accessing the RStudio Configuration Directory (Preferences)
  #
  # Windows:       %appdata%\RStudio
  # Linux/Mac:     ~/.config/rstudio


  # # In get_path_user_settings_dir_rs_1.3() # REMOVED
  #
  # get_path_user_settings_dir_rs_1.3 <- function(...) {
  #   base_path <-
  #     if (get_os_type() == "windows") {
  #       fs::path(Sys.getenv("APPDATA"), "RStudio")
  #     } else {
  #       fs::path_expand_r("~/.config/rstudio/")
  #     }
  #   fs::path(base_path, ...)
  # }
  #
  # # In get_path_rs_config_dir() # REMOVED
  #  base <-
  #   switch(get_os_type(),
  #     "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
  #     "linux"   = fs::path_expand_r("~/.config/RStudio"),
  #     "mac"     = {
  #       # TODO: check if correct on Mac
  #       # defaults read com.rstudio.desktop > ~/backup-rstudio-prefs
  #       # warning("Function get_path_rstudio_config_dir() may give a wrong result on Mac OS X.")
  #       fs::path_expand_r("~/.config/RStudio")
  #     },
  #     # Otherwise:
  #     {
  #       warning("Your OS is not supported by get_path_rstudio_config_dir().")
  #       fs::path_expand_r("~/.config/RStudio")
  #     }
  #   )
  # path_construct_and_check(base, ...)


  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("APPDATA"), "RStudio"),
      # FIXME: what is the correct dir in RStudio 1.4 on Unix like OS'es?
      # fs::path_expand("~/.config/RStudio")
      "linux"   = fs::path_expand_r("~/.config/rstudio"),
      "mac"     = fs::path_expand_r("~/.config/rstudio"),
                  fs::path_expand_r("~/.config/rstudio")    # Other OS'es
    )

  base <- Sys.getenv("XDG_CONFIG_DIRS",    unset = base)
  base <- Sys.getenv("RSTUDIO_CONFIG_DIR", unset = base)

  if (.check) {
    path_construct_and_check(base, ...)

  } else {
    fs::path(base, ...)
  }

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @export
get_path_rstudio_internal_state_dir <- function(..., .check = FALSE) {
  # https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
  # Section:
  # Accessing the RStudio-Desktop Directory (Internal State)
  #
  # Windows:       %localappdata%\RStudio-Desktop
  # Linux/Mac:     ~/.rstudio-desktop

  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop"),
      # Linux / Mac OS X:
      fs::path_expand("~/.rstudio-desktop")
    )

  if (.check) {
    path_construct_and_check(base, ...)

  } else {
    fs::path(base, ...)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#'
#' @concept paths and dirs
#' @export
#' @examples
#' \dontrun{\donttest{
#' get_path_rstudio_snippets_dir()
#' }}

get_path_rstudio_snippets_dir <- function() {
  get_path_rstudio_config_dir("snippets")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-related-dirs
#' @concept paths and dirs
#' @export
#' @examples
#' \dontrun{\donttest{
#' get_path_rstudio_keybindings_dir()
#' }}
get_path_rstudio_keybindings_dir <- function() {
  get_path_rstudio_config_dir("keybindings")
}


# ===========================================================================~
# Open Directories ==========================================================
# ===========================================================================~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname RStudio-related-dirs
#' @export
open_r_user_dir <- function() {
  browseURL(get_path_r_user_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_config_dir <- function() {
  browseURL(get_path_rstudio_config_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_internal_state_dir <- function() {
  browseURL(get_path_rstudio_internal_state_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_snippets_dir <- function() {
  browseURL(get_path_rstudio_snippets_dir())
}

#' @rdname RStudio-related-dirs
#' @export
open_rstudio_keybindings_dir <- function() {
  browseURL(get_path_rstudio_keybindings_dir())
}



# ===========================================================================~
# Open files ================================================================
# ===========================================================================~

#' @name RStudio-config-file
#' @title Manage RStudio Configuration (Preferences) File
#' @description Manage file with RStudio configuration (user preferences).
#' @param which (character) type of settings: "current", "bio-default".
#'
#' @export
#' @concept paths and dirs
#'
#' @seealso
#' - [get_path_rstudio_config_dir()]
#'
#' @examples
#' \dontrun{\donttest{
#'
#' get_path_rstudio_config_file()
#'
#' get_path_rstudio_config_file("bio-default")
#'
#' }}
get_path_rstudio_config_file <- function(which = "current") {

  if (which == "current") {
    get_path_rstudio_config_dir("rstudio-prefs.json")

  } else if (stringr::str_detect(which, "^bio$|^bio-")) {
    system.file(
      "rs-settings", "rstudio-prefs--bio-default.json", package = "bio"
    )
  } else {

    stop("Unknown value: ", which)
  }
}

#' @rdname RStudio-config-file
#' @export
open_rstudio_config_file <- function() {
  open_in_rstudio(path = get_path_rstudio_config_file())
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
  browseURL(get_path_r_environ())
}
