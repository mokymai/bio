# General ====================================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct path and check if it exists.
#'
#' @param base (character) The base for the path name.
#' @param ... (character) Parts of the path.
#'
#' @return Path or error if the path does not exist.
#'
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
# Get path to RStudio configuration directory.

#' @name RStudio-config-dir
#' @title Directories of R and RStudio (desktop) settings, preferences, etc.
#' @description
#' - `get_path_r_user_dir()`-- gets path to the main R user directory.
#' @export
#' @examples
#' get_path_r_user_dir()
#'
# Sys.getenv("R_USER")
# fs::path_home_r()
get_path_r_user_dir <- function(...) {
  fs::path(Sys.getenv("R_USER"), ".R", ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-config-dir
#' @description
#' - `get_path_rs_config_dir()` -- gets path to RStudio configuration directory
#'   or its sub-directories.
#' @param ... (character) Parts of the path. Path to sub-directories.
#'
#' @return
#' - `get_path_rs_config_dir()` (string) path, if it exists, or error.
#' @export
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_config_dir()
#' }}

# FIXME: get_path_rs_config_dir() previously was get_rs_state_dir()
get_path_rs_config_dir  <- function(...) {
  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio"),
      "linux"   = fs::path_expand_r("~/.config/RStudio"),
      "osx"     = {
        # FIXME: Fix for Mac OS X
        # defaults read com.rstudio.desktop > ~/backup-rstudio-prefs
        warning("Mac OS X is not supported by get_path_rs_config_dir().")
        fs::path_expand_r("~/.config/RStudio")
      },
      # Otherwise:
      {
        warning("Your OS is not supported by get_path_rs_config_dir().")
        fs::path_expand_r("~/.config/RStudio")
      }
    )
  path_construct_and_check(base, ...)
}

#' @rdname RStudio-config-dir
#' @description
#' - `get_path_rs_desktop_config_dir()`` - gets path to directory (and its
#'    sub-directories) of RStudio desktop user interface settings.
#' @param .check (logical) If `TRUE`, additionally checks for path existance.
#'
#' @return (string) path to RStudio desktop user settings directory.
#'         When `.check = TRUE`, renturns error, if the path does not exist.
#' @export
#' @seealso
#' - `get_path_rs_desktop_config_dir()`:
#' https://support.rstudio.com/hc/en-us/articles/200534577-Resetting-RStudio-Desktop-s-State
#'
#' @examples
#' \dontrun{\donttest{
#' get_path_rs_desktop_config_dir()
#'
#' get_path_rs_desktop_config_dir("dictionaries")
#' }}

get_path_rs_desktop_config_dir <- function(..., .check = FALSE) {
  # base <-
  #   switch(
  #     get_os_type(),
  #     "windows" = file.path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop"),
  #     path.expand("~/.rstudio-desktop")
  #   )
  # normalizePath(file.path(base, ...))
  base <-
    switch(get_os_type(),
      "windows" = fs::path(Sys.getenv("LOCALAPPDATA"), "RStudio-Desktop"),
      # Other OS'es
      fs::path_expand_r("~/.rstudio-desktop")
    )

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
#' @export
#' @examples
#' get_path_rs_snippets_dir()
#'
get_path_rs_snippets_dir <- function() {
  get_path_r_user_dir("snippets")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name RStudio-config-dir
#' @export
#' @examples
#' get_path_rs_keybindings_dir()
#'
get_path_rs_keybindings_dir <- function() {
  get_path_r_user_dir("rstudio", "keybindings")
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
#' @title Get RStudio document IDs and paths.
#' @description Get RStudio file IDs and paths.
#'
#' @return Data frame with columns `id` (for IDs) and `path` (for file paths).
#' @export
#'
#' @examples
#' head(get_rs_file_ids_user())
get_rs_file_ids <- function() {
  dplyr::bind_rows(
    get_rs_file_ids_user(),
    get_rs_file_ids_project()
  )
}

#' @rdname get_rs_file_ids
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
#' - `set_wd()`: ... -> Choose Directory...
#'
#' @export
set_wd <- function() {
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

#' @name open_files
#' @title Open files and directories
#' @description
#' - `open_in_rs()` tries to open file in RStudio.
#' @param path (sting) Path to file.
#' @seealso
#' - [fs::file_show()],
#' - [rstudioapi::navigateToFile()],
#' - [browseURL()],
#' - [utils::file.edit()]
#' @export
open_in_rs <- function(path) {
  fs::file_show(path = path, browser = "RStudio")
}

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
get_path_rs_desktop_user_settings_dir <- function() {
  get_path_rs_desktop_config_dir("monitored/user-settings")
}

#' @rdname open_files
#' @export
open_rs_desktop_user_settings_dir <- function() {
  fs::file_show(path = get_path_rs_desktop_user_settings_dir())
}


#' @rdname open_files
#' @param (character) type of settings: "current", "bs-default".
#' @export
#' @examples
#' get_path_rs_user_settings()
#'
#' get_path_rs_user_settings("bs-default")
#'
get_path_rs_user_settings <- function(which = "current") {
  switch(which,
    "current"    =
      get_path_rs_desktop_config_dir("monitored/user-settings/user-settings"),

    "bs-default" =
      system.file("rs-settings", "user-settings", package = "bs"),

    stop("unrecognized option: ", which)
  )
}

#' @rdname open_files
#' @export
open_rs_user_settings <- function() {
    open_in_rs(path = get_path_rs_user_settings())
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


# ============================================================================
str_glue_eval <- function(..., envir = parent.frame(), .sep = "",
  .open = "{", .close = "}", envir_eval = envir,  envir_glue = envir) {

  commands_as_text <- stringr::str_glue(...,
    .envir = envir_glue,
    .open  = .open,
    .close = .close
  )
  eval(parse(text = commands_as_text), envir = envir_eval)
}


