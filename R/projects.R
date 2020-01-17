# RStudio projects ===========================================================
#' Extract R Project's Name
#'
#' @param path (character) Vector of character names
#'
#' @return (CHaracter) a vector of extracted character names
#' @export
#'
#' @importFrom magrittr "%>%"
#' @examples
#' path_to_project <- c("list/proj.Rproj", "C:/R/BS-2020/bs.Rproj")
#' rproj_extract_proj_name(path_to_project)
#
rproj_extract_proj_name <- function(path) {
  path <- fs::path(path)
  ext  <- fs::path_ext(path)
  if (any(!tolower(ext) %in% c("Rproj", "rproj"))) {
    warning('The result is incorrect as the extension in some strings are not ".Rproj".')
  }
  stringr::str_replace(path, "(.*/)?([^/]*?)(/[^/]*?\\.[Rr]proj$)", "\\2")
}

#' @rdname projects
#'
#' @param file (character) Path to file with RStudio projects.
#' @param sort_by (`"name"`|`"path"`|[`FALSE`])
#'
#' @export
#' @description
#' - `rproj_read_projects()` - reads file with projects and list their names and paths.
#'
rproj_read_projects <- function(file, sort_by = FALSE) {

  projs   <- readr::read_lines(file)
  projs   <- stringr::str_subset(projs, "^\\s*$", negate = TRUE)
  proj_df <- rproj_path_to_df(projs)

  switch(
    tolower(sort_by),

    "name" = , "names" = {
      dplyr::arrange(proj_df, proj_df)
    },

    "path" = , "paths" = {
      dplyr::arrange(proj_df, name)
    },
    # else
    proj_df
  )
}

#
#' @name projects
#' @title Project paths.
#'
#' @param proj (character) Path to *.Rproj file.
#'
#' @return
#'  - `rproj_path_to_df()` returns data frame with columns `name` (with project
#'     name, i.e., folder name) and `path` to *.Rproj file.
#'
#' @export
#'
#' @examples
#' rproj_path_to_df("C:/data/analysis/proj.Rproj")
rproj_path_to_df <- function(proj) {
  tibble::tibble(
    name   = rproj_extract_proj_name(path = proj),
    path   = proj,
    exists = file.exists(proj)
  )
}

#' @name projects
#' @export
#' @description
#' - `rproj_get_path_rs_recent_proj_list()` -- gets path to the file with the list of
#'    recent RStudio projects.
#' @examples
#' rproj_get_path_rs_recent_proj_list()
#'
rproj_get_path_rs_recent_proj_list <- function() {
  get_path_rs_desktop_config_dir("monitored/lists/project_mru")
}

#' @name projects
#' @export
#' @description
#' - `rproj_get_current_projects()` -- lists current RStudio projects (in a data
#'    frame).
rproj_get_current_projects <- function(sort_by = FALSE) {
  rproj_read_projects(file = rproj_get_path_rs_recent_proj_list(), sort_by = sort_by)
}

#' @name projects
#' @export
#' @description
#' - `rproj_get_proj_names()` -- lists RStudio projects names (as a character
#'   vector)
#' @examples
#' head(rproj_get_proj_names())
rproj_get_proj_names <- function(file = rproj_get_path_rs_recent_proj_list(),
  sort_by = FALSE) {
  rproj_read_projects(file, sort_by = sort_by)$name
}


#' @name projects
#' @export
#' @description
#' - `rproj_open_rs_recent_proj_list()` -- opens the file with the list of
#'    recent RStudio projects.
rproj_open_rs_recent_proj_list <- function() {
  open_in_rs(path = rproj_get_path_rs_recent_proj_list())
}


#' Open RStudio project.
#'
#' Open RStudio project by name or interactively.
#'
#' @param name (string|`NULL`) The name of the project or `NULL` to choose
#'       a project interactively.
#' @param proj_list (data frame) The result of [rproj_read_projects()] or `NULL`.
#' @param proj_list_path (string) The path to the file with the list of project
#'        paths. If `proj_list` is not `NULL`, then `proj_list_path` is ignored.
#' @param new_session (logical) should the project be opened in a new session,
#'        or should the current RStudio session switch to that project? Note
#'        that `TRUE` values are only supported with RStudio Desktop and RStudio
#'        Server Pro.
#' @param only_existing (logical) If `TRUE`, non-existing projects and projects
#'        with broken paths are removed from the list of choices.
#'
#' @return Opens the indicated project.
#' @export
#' @seealso
#' - [rstudioapi::openProject()]
#' - [rstudioapi::initializeProject()]
#' @examples
#' \donttest{\dontrun{
#'
#' rproj_open_project()
#'
#' rproj_open_project("bs")
#'
#' rproj_open_project("R-2019-project")
#'
#' }}
#
rproj_open_project <- function(name = NULL, new_session = FALSE, proj_list = NULL,
  proj_list_path = rproj_get_path_rs_recent_proj_list(), only_existing = TRUE) {

  if (is.null(proj_list)) {

    if (!fs::file_exists(proj_list_path)) {
      usethis::ui_stop("The file {usethis::ui_path(proj_list_path)} was not found.")
    }

    proj_list <- rproj_read_projects(proj_list_path)
  }

  if (isTRUE(only_existing)) {
    proj_list <- dplyr::filter(proj_list, exists == TRUE)
  }

  if (is.null(name)) {
    if (!interactive()) {
      stop(
        "No project is chosen. Available projects:\n",
        paste(proj_list$name, collapse = "\n")
      )
    }

    cat("\nChoose the name of the project: \n")

    all_names <- sort(proj_list$name)
    i_name <- utils::menu(all_names)
    if (i_name == 0) {
      message("Cancelled by user.")
      return(invisible())

    } else {
      name <- all_names[i_name]
    }
  }

  proj <- dplyr::filter(proj_list, name == !!name)
  n_proj <- nrow(proj)

  if (n_proj < 1) {
    stop(
      "No project match name '", name, "'. ",
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

    cat(sep = "", "\nSeveral projects match name '", name, "'. \n")
    cat("Choose the path to the project of interest: \n")

    i_path <- utils::menu(proj$path)
    if (i_path == 0) {
      message("Cancelled by user.")
      return(invisible())
    }
    proj_path <- proj$path[i_path]

  } else {
    proj_path <- proj$path
  }

  rstudioapi::openProject(proj_path, newSession = new_session)
}

#' @rdname rproj_open_project
#' @export
open_project <- function(...) {
  rproj_open_project(...)
}

# VG projects ================================================================
rproj_get_path_vg_proj_list <- function() {
  "D:/Dokumentai/R/bs/data-raw/project-list"
}


rproj_open_vg_proj_list <- function() {
  open_in_rs(path = rproj_get_path_vg_proj_list())
}


rproj_list_recent_n_vg_proj <- function() {

  file_recent <- rproj_get_path_rs_recent_proj_list()
  file_vg     <- rproj_get_path_vg_proj_list()

  new_list <-
    dplyr::bind_rows(
      rproj_read_projects(file = file_recent),
      rproj_read_projects(file = file_vg)
    ) %>%
    dplyr::distinct()

  new_list
}


rproj_update_vg_proj_list <- function() {
  file_vg  <- rproj_get_path_vg_proj_list()
  new_list <- rproj_list_recent_n_vg_proj()
  readr::write_lines(new_list$path, path = file_vg)
}

update_vg_proj_list <- function() {
  rproj_update_vg_proj_list()
}

rproj_open_project2 <- function(name = NULL, new_session = FALSE, ...) {
  new_list <- rproj_list_recent_n_vg_proj()
  rproj_open_project(new_session = new_session, proj_list = new_list, ...)
}

open_project2 <- function(name = NULL, new_session = FALSE, ...) {
  new_list <- rproj_list_recent_n_vg_proj()
  rproj_open_project(new_session = new_session, proj_list = new_list, ...)
}

