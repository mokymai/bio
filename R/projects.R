# Get details about RStudio project from its path ============================

#' @name parse_proj_path
#' @title Parse project's path
#' @description
#' Get details about RStudio project from its path.
#'
#' - `extract_proj_name()` extracts RStudio project's name form a path to the
#'   project.
#' @param proj_path (character) Path to `*.Rproj` file.
#'
#' @return
#'  - `parse_proj_path()` returns data frame with columns
#'     `name` (with project name, i.e., folder name),
#'     `path` to *.Rproj file,
#'     `exists` flag if .Rproj file exists, and
#'     `dir_exists` flag if dir with .Rproj file exists
#'     (sometimes file is renamed and directory still exists).
#' - `extract_proj_name()` (character) a vector of extracted project names.
#'                        A project name is a directory name.
#'
#' @noRd
#'
#' @concept rstudio projects
#'
#' @examples
#' path_to_project <-  c("list/proj.Rproj", "C:/R/BS-2020/bs.Rproj",
#'     "C:/data/analysis/proj.Rproj")
#'
#' extract_proj_name(path_to_project)
#'
#' parse_proj_path(path_to_project)
NULL

#' @rdname parse_proj_path
#' @noRd
parse_proj_path <- function(proj_path) {
  tibble::tibble(
    name   = extract_proj_name(proj_path = proj_path),
    path   = proj_path,
    exists = fs::file_exists(proj_path),
    dir_exists = fs::dir_exists(fs::path_dir(proj_path))
  )
}

#' @rdname parse_proj_path
#' @noRd
extract_proj_name <- function(proj_path) {
  proj_path <- fs::path(proj_path)
  ext       <- fs::path_ext(proj_path)
  if (any(!tolower(ext) %in% c("Rproj", "rproj"))) {
    warning('The result is incorrect as the extension in some strings is not ".Rproj".')
  }
  stringr::str_replace(proj_path, "(.*/)?([^/]*?)(/[^/]*?\\.[Rr]proj$)", "\\2")
}


#' @rdname parse_proj_path
#' @noRd
#' @examples
#' highlight_proj_name("D:/bio/proj.Rproj")
#' highlight_proj_name("bio/proj.Rproj")
highlight_proj_name <- function(proj_path) {
  proj_dir <- fs::path_dir(proj_path)
  glue::glue(
    crayon::blue(sep = "",
      fs::path(
        paste0("'", fs::path_dir(proj_dir)),
        crayon::yellow(fs::path_file(proj_dir)),
        paste0(fs::path_file(proj_path), "'")
      )
    )
  )
}

# Manage RStudio projects ====================================================
#' @name projects
#' @title Manage RStudio Projects
#'
#' @description
#' - `read_projects()` - reads file with projects and list their names and paths.
#' @examples
#' \dontrun{\donttest{
#' read_projects(get_path_recent_proj_list())
#' }}
#'
#' @param file (character) Path to file with RStudio project names.
#' @param sort_by (`"name"`|`"path"`|[`FALSE`])
#'
#' @export
#'
#' @concept rstudio projects
#'
read_projects <- function(file, sort_by = FALSE) {

  if (is.null(file) || !file.exists(file)) {
    warning("File was not found: ", file)
    return(NULL)
  }

  projs   <- readr::read_lines(file, lazy = FALSE)
  projs   <- stringr::str_trim(projs)
  projs   <- stringr::str_subset(projs, "^\\s*$", negate = TRUE)
  proj_df <- parse_proj_path(projs)

  switch(
    tolower(sort_by),

    "name" = , "names" = {
      dplyr::arrange(proj_df, name)
    },

    "path" = , "paths" = {
      dplyr::arrange(proj_df, path)
    },
    # else
    proj_df
  )
}


#' @name projects
#' @noRd
#' @description
#' - `get_projs_recent()` -- lists RStudio projects from the recent project list.
#' - `get_projs_user()` -- lists RStudio projects from a user-defined list.
#' - `get_projs_all()` -- lists RStudio projects from recent project and user-defined project lists.
#' @examples
#' \dontrun{\donttest{
#' get_projs_recent()
#' get_projs_user()
#' get_projs_all()
#' }}
#'
get_projs_recent <- function(sort_by = FALSE) {
  read_projects(file = get_path_recent_proj_list(), sort_by = sort_by)
}

#' @name projects
#' @noRd
get_projs_user <- function(sort_by = FALSE) {
  read_projects(file = get_path_user_proj_list(create = TRUE), sort_by = sort_by)
}

#' @name projects
#' @noRd
get_projs_all <- function() {

  file_with_recent_list <- get_path_recent_proj_list()
  file_with_users_list  <- get_path_user_proj_list(create = TRUE)

  new_list <-
    dplyr::bind_rows(
      read_projects(file = file_with_recent_list),
      read_projects(file = file_with_users_list)
    ) %>%
    dplyr::distinct()

  new_list
}

#' @name projects
#' @noRd
#' @description
#' - `get_proj_names()` -- lists RStudio projects names (as a character
#'   vector)
#' @examples
#' \dontrun{\donttest{
#' head(get_proj_names())
#' }}
get_proj_names <- function(file = get_path_recent_proj_list(),
  sort_by = FALSE) {
  read_projects(file, sort_by = sort_by)$name
}

# Open RStudio project =======================================================
#' Open RStudio Project
#'
#' Open RStudio project by name or interactively. The projects list is read from
#' files that contain project lists.
#'
#' @param name (string|`NULL`) The name of the project or `NULL` to choose
#'       a project interactively.
#' @param proj_list (data frame) The result of [read_projects()] or `NULL`.
#' @param proj_list_path (string) The path to the file with the list of project
#'        paths. If `proj_list` is not `NULL`, then `proj_list_path` is ignored.
#' @param new_session (logical|`NULL`) should the project be opened in a new
#'        session, or should the current RStudio session switch to that project?
#'        Note that `TRUE` values are only supported with RStudio Desktop and
#'        RStudio Server Pro.
#'        If `NULL`, user will have to choose interactively.
#' @param only_available (logical) If `TRUE`, non-existing projects and projects
#'        with broken paths are removed from the list of choices.
#' @param pattern (character) regular expression to narrow down the list of
#'        possible options.
#' @param negate (logical) If `TRUE`, then the options defined by  `pattern` are
#'        excluded.
#'
#' @return Opens the indicated project.
#' @export
#'
#' @concept rstudio projects
#'
#' @seealso
#' - [update_rstudio_proj_list_user()]
#' - [rstudioapi::openProject()]
#' - [rstudioapi::initializeProject()]
#' @examples
#' \dontrun{\donttest{
#'
#' open_project()
#'
#' open_project("bio")
#'
#' open_project("R-2019-project")
#'
#' }}
#
open_project <- function(pattern = NULL,
                         new_session = if (interactive()) NULL else TRUE,
                         proj_list = NULL,
                         proj_list_path = NULL,
                         only_available = TRUE,
                         name = NULL,
                         negate = FALSE) {

  if (is.null(proj_list) && is.null(proj_list_path)) {
    # No project lists are provided
    proj_list <- get_projs_all()

  } else {
    if (!is.null(proj_list_path)) {
      if (!fs::file_exists(proj_list_path)) {
        usethis::ui_stop(
          "The file {usethis::ui_path(proj_list_path)} was not found."
        )
      }
      proj_list_2 <- read_projects(proj_list_path)
    }

    proj_list <- dplyr::bind_rows(proj_list, proj_list_2)
  }

  if (isTRUE(only_available) || only_available == "rproj") {
    proj_list <- dplyr::filter(proj_list, exists == TRUE)
  } else if (only_available %in% c("dir", "proj dir", "project dir")) {
    proj_list <- dplyr::filter(proj_list, dir_exists == TRUE)
  }

  if (!is.null(pattern)) {
    proj_list <-
      dplyr::filter(proj_list, stringr::str_detect(name, pattern, negate))
  }

  n_projs <- nrow(proj_list)

  if (n_projs < 1) {
    negated <- ifelse(negate, " negated", "")
    usethis::ui_oops(paste0(
      "No project name matches{negated} regex pattern ",
      "'{blue(pattern)}'"
    ))
    return(invisible())
  }

  if (is.null(name)) {
    if (!interactive()) {
      stop(
        "No project is chosen. Available projects:\n",
        paste(proj_list$name, collapse = "\n")
      )
    }

    cat("\nChoose the number of the project (or enter 0 to cancel): \n")

    all_names <- sort(unique(proj_list$name))
    i_name <- utils::menu(all_names)
    if (i_name == 0) {
      usethis::ui_oops("Cancelled by user")
      return(invisible())

    } else {
      name <- all_names[i_name]
    }
  }

  proj <- dplyr::filter(proj_list, name == !!name)
  n_proj <- nrow(proj)

  if (n_proj < 1) {
    stop(
      "No project named '", name, "'. ",
      "Available projects:\n", paste(proj_list$name, collapse = ", ")
    )

  } else if (n_proj > 1) {
    if (!interactive()) {
      stop(
        "Several projects match name '", name, "'. ",
        "\nUse `rstudioapi::openProject()` with one of the following paths:\n",
        paste(proj$path, collapse = "\n")
      )
    }

    usethis::ui_info(paste0(
      "\nSeveral projects match name {usethis::ui_value(name)}.\n",
      "Choose the path to the project of interest: \n"
    ))
    i_path <- utils::menu(proj$path)

    if (i_path == 0) {
      usethis::ui_oops("Cancelled by user")
      return(invisible(NULL))
    }
    proj_path <- proj$path[i_path]

  } else {
    proj_path <- proj$path
  }

  if (is.null(new_session)) {
    proj_path_i <- highlight_proj_name(proj_path)
    usethis::ui_done("{proj_path_i}\n\n")

    usethis::ui_info("Close current project:")
    i_close <- utils::menu(c("Yes, close", "No, keep open (default)"))

    if (i_close == 0) {
      usethis::ui_oops("Cancelled by user")
      return(invisible(NULL))
    }
    new_session <- ifelse(i_close == 1, FALSE, TRUE)
  }

  if (!new_session) {
    usethis::ui_oops("Closing current project...")
  }

  usethis::ui_done("Opening {blue(proj_path)}")
  rstudioapi::openProject(proj_path, newSession = new_session)
}

# @rdname open_project
# @export
open_project_from_user_list <- function(pattern = NULL, new_session = TRUE,
  only_available = TRUE, name = NULL, ...) {

  new_list <- get_projs_user()
  open_project(pattern = pattern, new_session = new_session, proj_list = new_list,
    only_available = only_available, name = name, ...)
}


# Project lists ==============================================================

#' @name project-lists
#' @title Manage Project Lists
#' @description
#' Manage project lists.
#' @concept rstudio projects

NULL

#' @rdname project-lists
#' @export
#' @description
#' - `get_path_recent_proj_list()` -- gets path to the file with the list of
#'    recent RStudio projects.
#' - `get_path_user_proj_list()` -- gets path to the file with the list of
#'    personal RStudio projects.
#' @examples
#' \dontrun{\donttest{
#' get_path_recent_proj_list()
#' get_path_user_proj_list()
#' }}
#'
get_path_recent_proj_list <- function() {
  get_path_rstudio_internal_state_dir("monitored/lists/project_mru")
}

#' @rdname project-lists
#' @param create (logical) If `TRUE` and file does not exist, the file is created.
#' @export
get_path_user_proj_list <- function(create = FALSE) {
  file_with_users_list <- fs::path(get_path_r_user_dir(), "rstudio-proj-list--user")
  if (create && !fs::file_exists(file_with_users_list)) {
    fs::dir_create(fs::path_dir(file_with_users_list))
    fs::file_create(file_with_users_list)
    ui_done("File for RStudio project list was created: {ui_path(file_with_users_list)}")
  }
  file_with_users_list
}


#' @rdname project-lists
#' @description
#' - `open_recent_proj_list()` -- opens the file with the list of
#'    recent RStudio projects.
#' @export
open_recent_proj_list <- function() {
  open_in_rstudio(path = get_path_recent_proj_list())
}

#' @rdname project-lists
#' @export
open_user_proj_list <- function() {
  open_in_rstudio(path = get_path_user_proj_list())
}

#' @rdname project-lists
#' @export
update_rstudio_proj_list_user <- function() {
  file_with_users_list <- get_path_user_proj_list(create = TRUE)
  new_list <- get_projs_all()
  readr::write_lines(new_list$path, file_with_users_list)
  ui_done(paste0(
    "User's list of RStudio projects was updated:\n",
    "{usethis::ui_path(file_with_users_list)}"
  ))
}

