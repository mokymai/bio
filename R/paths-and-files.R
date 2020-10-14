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
    # stop("The path does not exist: \n", file, call. = FALSE)
  }
}

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
# Get paths ==================================================================

#' Get path to desktop folder
#'
#' Get path to desktop folder of current user.
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

# Get path to RStudio configuration directory.

#' @name RStudio-config-dir
#' @title Directories of R and RStudio (desktop) settings, preferences, etc.
#' @description
#' - `get_path_r_user_dir()`-- gets path to the main R user directory.
#'
#' @concept paths and dirs
#'
#' @export
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
#' @name RStudio-config-dir
#' @description
#' - `get_path_rs_config_dir()` -- gets path to RStudio configuration directory
#'   or its sub-directories.
#'
#' @param ... (character) Parts of the path. Path to sub-directories.
#'
#' @return
#' - `get_path_rs_config_dir()` (string) path, if it exists, or error.
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_config_dir()
#' }}

# FIXME: get_path_rs_config_dir() previously was get_rs_state_dir()
get_path_rs_config_dir  <- function(...) {
  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
      "linux"   = fs::path_expand_r("~/.config/RStudio"), # FIXME: path_expand_r() or path_expand() ?
      "mac"     = {
        # FIXME: Fix for Mac OS X
        # defaults read com.rstudio.desktop > ~/backup-rstudio-prefs
        warning("Function get_path_rs_config_dir() may give a wrong result in Mac OS X.")
        fs::path_expand_r("~/.config/RStudio") # FIXME: path_expand_r() or path_expand() ?
      },
      # Otherwise:
      {
        warning("Your OS is not supported by get_path_rs_config_dir().")
        fs::path_expand_r("~/.config/RStudio") # FIXME: path_expand_r() or path_expand() ?
      }
    )
  path_construct_and_check(base, ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-config-dir
#' @export
get_path_user_settings_dir_rs_1.3 <- function(...) {
  base_path <-
    if (get_os_type() == "windows") {
      fs::path(Sys.getenv("APPDATA"), "RStudio")
    } else {
      fs::path_expand_r("~/.config/rstudio/")
    }
  fs::path(base_path, ...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If "auto", returns version of RStudio or error if RStudio does not run.
# If character with version number, returns that value as numeric version.
resolve_rs_version <- function(rstudio_version) {
  if (tolower(rstudio_version) == "auto") {
    rstudioapi::versionInfo()$version
  } else {
    as.numeric_version(rstudio_version)
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname RStudio-config-dir
#' @description
#' - `get_path_rs_desktop_config_dir()`` - gets path to directory (and its
#'    sub-directories) of RStudio desktop user interface settings.
#'
#' @param .check (logical) If `TRUE`, additionally checks for path existance.
#'
#' @return (string) path to RStudio desktop user settings directory.
#'         When `.check = TRUE`, renturns error, if the path does not exist.
#'
#' @seealso
#' - `get_path_rs_desktop_config_dir()`:
#' https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#'
#' @concept paths and dirs
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_desktop_config_dir()
#'
#' get_path_rs_desktop_config_dir("dictionaries")
#' }}

get_path_rs_desktop_config_dir <- function(...,
                                           .check = FALSE,
                                           rstudio_version = "auto") {
  # base <-
  #   switch(
  #     get_os_type(),
  #     "windows" = file.path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop"),
  #     path.expand("~/.rstudio-desktop")
  #   )
  # normalizePath(file.path(base, ...))

  rstudio_version <- resolve_rs_version(rstudio_version)
  base <-
    switch(get_os_type(),
      "windows" = fs::path(
        Sys.getenv("LOCALAPPDATA"),
        if (rstudio_version < 1.4) {
          "RStudio-Desktop"
        } else {
          "RStudio"
        }
      ),
      # Other OS'es
      # FIXME: what is the correct dir in RStudio 1.4 on Unix like OS'es?
      fs::path_expand("~/.rstudio-desktop")
    )

  base <- Sys.getenv("RSTUDIO_CONFIG_DIR", unset = base)

  if (.check) {
    path_construct_and_check(base, ...)

  } else {
    fs::path(base, ...)
  }
}

# RStudio version 1.1.423:
# On Mac: ~/.rstudio-desktop
# On Mac: ~/.rstudio-desktop/monitored/lists/user_dictionary
#
# https://stackoverflow.com/a/52377127/4783029


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-config-dir
#'
#' @inheritParams RStudio-dictionaries
#'
#' @concept paths and dirs
#' @export
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_snippets_dir()
#' }}
#'
#' get_path_rs_snippets_dir(rstudio_version = "1.3.1073")
#'

get_path_rs_snippets_dir <- function(rstudio_version = "auto") {
  if (resolve_rs_version(rstudio_version) > "1.3") {
    get_path_user_settings_dir_rs_1.3("snippets")
  } else {
    get_path_r_user_dir("snippets")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-config-dir
#' @concept paths and dirs
#' @export
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_keybindings_dir()
#' }}
#'
#' get_path_rs_keybindings_dir(rstudio_version = "1.3.1073")
#'
get_path_rs_keybindings_dir <- function(rstudio_version = "auto") {

  if (resolve_rs_version(rstudio_version) > "1.3") {
    get_path_user_settings_dir_rs_1.3("keybindings")
  } else {
    get_path_r_user_dir("rstudio", "keybindings")
  }
}


# ===========================================================================~
# Create files and directories -----------------------------------------------
create_r_user_dir <- function(...) {
  r_dir <- get_path_r_user_dir(...)
  if (!dir.exists(r_dir)) {
    dir.create(r_dir, recursive = TRUE)
  }
  invisible(r_dir)
}

create_rs_snippets_dir <- function() {
  create_r_user_dir("snippets")
}

create_rs_keybindings_dir <- function() {
  create_r_user_dir("rstudio", "keybindings")
}


# ----------------------------------------------------------------------------
#
# library(tidyverse)
#' @name get_rs_file_ids
#' @title Get RStudio document IDs and paths
#' @description Get RStudio file IDs and paths.
#'
#' @return Data frame with columns `id` (for IDs) and `path` (for file paths).
#'
#' @concept paths and dirs
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' head(get_rs_file_ids_user())
#' }}
get_rs_file_ids <- function() {
  dplyr::bind_rows(
    get_rs_file_ids_user(),
    get_rs_file_ids_project()
  )
}

#' @rdname get_rs_file_ids
#' @concept paths and dirs
#' @export
get_rs_file_ids_user <- function() {
  get_path_rs_desktop_config_dir("notebooks/paths") %>%
    readr::read_lines() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      id   = stringr::str_extract(value, '(?<=").*?(?="$)'),
      path = stringr::str_extract(value, '^.*?(?==")')
    )
}

#' @rdname get_rs_file_ids
#' @concept paths and dirs
#' @export
get_rs_file_ids_project <- function() {
  # TODO: Not implemented yet
  NULL
}


# Working directories ========================================================
#' Change working directory in RStudio
#'
#' Functions do the same as RStudio menus
#' RStudio menu -> Session -> Set Working Directory -> ...:
#'
#' - `set_wd_to_active_doc()`: ... -> To Source File Location
#' - `set_wd_to_files_pane()`: ... -> to Files Pane Directory
#'   (directory opened in files tab)
#' - `set_wd_to_project_dir()`: ... -> To Project Directory
#' - `set_wd_interactive()`: ... -> Choose Directory...
#'
#' @name set_wd
#'
#' @concept r and rstudio settings
#'
#' @export
set_wd_interactive <- function() {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    invisible(rstudioapi::executeCommand("setWorkingDir"))
  }
}

#' @rdname set_wd
#' @export
set_wd_to_active_doc <- function() {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    invisible(rstudioapi::executeCommand("setWorkingDirToActiveDoc"))
  }
}

#' @rdname set_wd
#' @export
set_wd_to_files_pane <- function() {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    invisible(rstudioapi::executeCommand("setWorkingDirToFilesPane"))
  }
}

#' @rdname set_wd
#' @export
set_wd_to_project_dir <- function() {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("executeCommand")) {
    invisible(rstudioapi::executeCommand("setWorkingDirToProjectDir"))
  }
}


# Open files and Directories =================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name open_in_rstudio
#' @title Open file in RStudio
#'
#' @description
#' Function tries opening a file in RStudio.
#' @param path (sting) Path to file.
#' @param ... Further arguments to [rstudioapi::navigateToFile()].
#'
#' @concept open
#' @concept open files
#'
#' @seealso
#' - [rstudioapi::navigateToFile()],
#' - [fs::file_show()], [browseURL()],
#' - [utils::file.edit()]
#'
#' @export
open_in_rstudio <- function(path, ...) {
  rstudioapi::navigateToFile(path, ...)
  # fs::file_show(path = path, browser = "RStudio")
  # TODO (SEE ALSO): usethis::edit_file()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name open_files
#'
#' @title Open files and directories
#' @description
#' Open RStudio related files and directories.
#'
#' @seealso
#' - [fs::file_show()], [browseURL()],
#' - [rstudioapi::navigateToFile()],
#' - [utils::file.edit()]
#'
#' @concept paths and dirs

# @param path (sting) Path to file.
NULL

#' @rdname open_files
#' @export
open_r_user_dir <- function() {
  browseURL(get_path_r_user_dir())
}

#' @rdname open_files
#' @export
open_rs_config_dir <- function() {
  browseURL(get_path_rs_config_dir())
}

#' @rdname open_files
#' @export
open_rs_1.3_user_settings_dir <- function() {
  browseURL(get_path_user_settings_dir_rs_1.3())
}

#' @rdname open_files
#' @export
get_path_rs_desktop_user_settings_dir <- function() {
  get_path_rs_desktop_config_dir("monitored/user-settings")
}

#' @rdname open_files
#' @export
open_rs_desktop_user_settings_dir <- function() {
  fs::file_show(path = get_path_rs_desktop_user_settings_dir())
}


#' @rdname open_files
#'
#' @param which (character) type of settings: "current", "bio-default".
#' @inheritParams RStudio-dictionaries
#'
#' @export
#' @examples
#' \dontrun{\donttest{
#'
#' get_path_rs_user_settings()
#'
#' get_path_rs_user_settings("bio-default")
#'
#' }}
get_path_rs_user_settings <- function(which = "current", rstudio_version = "auto") {

  rs_version <- resolve_rs_version(rstudio_version)

  switch(which,
    "current"    =
      if (rs_version > "1.3") {
        get_path_user_settings_dir_rs_1.3("rstudio-prefs.json")
      } else {
        get_path_rs_desktop_config_dir("monitored/user-settings/user-settings")
      },

    "bio-default" =
      if (rs_version > "1.3") {
        system.file("rs-settings", "rstudio-prefs.json", package = "bio")
      } else {
        system.file("rs-settings", "user-settings", package = "bio")
      },

    stop("unrecognized option: ", which)
  )
}

#' @rdname open_files
#' @export
open_rs_user_settings <- function() {
  open_in_rstudio(path = get_path_rs_user_settings())
}

#' @rdname open_files
#' @export
open_rs_desktop_config_dir <- function() {
  browseURL(get_path_rs_desktop_config_dir())
}

#' @rdname open_files
#' @export
open_rs_snippets_dir <- function() {
  browseURL(get_path_rs_snippets_dir())
}

#' @rdname open_files
#' @export
open_rs_keybindings_dir <- function() {
  browseURL(get_path_rs_keybindings_dir())
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname open_files
#' @export
#' @description
#' - `get_path_r_environ()`-- gets path to file `.Renviron` with R environment
#'   variables.
#' @param scope (character) One of "user" or "project".
#'
#' @examples
#' get_path_r_environ()
#'
get_path_r_environ <- function(scope = c("user", "project")) {
  scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
}

#' @rdname open_files
#' @export
open_r_environ <- function() {
  browseURL(get_path_r_environ())
}


# str_glue_eval ==============================================================
str_glue_eval <- function(..., envir = parent.frame(), .sep = "",
  .open = "{", .close = "}", envir_eval = envir,  envir_glue = envir) {

  commands_as_text <- glue::glue(...,
    .envir = envir_glue,
    .open  = .open,
    .close = .close
  )
  eval(parse(text = commands_as_text), envir = envir_eval)
}


