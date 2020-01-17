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
#' extract_proj_name(path_to_project)
#
extract_proj_name <- function(path) {
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
#' - `read_projects()` - reads file with projects and list their names and paths.
#'
read_projects <- function(file, sort_by = FALSE) {

  projs   <- readr::read_lines(file)
  projs   <- stringr::str_subset(projs, "^\\s*$", negate = TRUE)
  proj_df <- parse_proj_path(projs)

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
#'  - `parse_proj_path()` returns data frame with columns `name` (with project
#'     name, i.e., folder name) and `path` to *.Rproj file.
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' parse_proj_path("C:/data/analysis/proj.Rproj")
#' }}
parse_proj_path <- function(proj) {
  tibble::tibble(
    name   = extract_proj_name(path = proj),
    path   = proj,
    exists = file.exists(proj)
  )
}

#' @name projects
#' @export
#' @description
#' - `get_path_recent_proj_list()` -- gets path to the file with the list of
#'    recent RStudio projects.
#' @examples
#' \dontrun{\donttest{
#' get_path_recent_proj_list()
#' }}
#'
get_path_recent_proj_list <- function() {
  get_path_rs_desktop_config_dir("monitored/lists/project_mru")
}

#' @name projects
#' @export
#' @description
#' - `get_projs_recent()` -- lists recent RStudio projects (into a data
#'    frame).
get_projs_recent <- function(sort_by = FALSE) {
  read_projects(file = get_path_recent_proj_list(), sort_by = sort_by)
}

#' @name projects
#' @export
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


#' @name projects
#' @export
#' @description
#' - `open_recent_proj_list()` -- opens the file with the list of
#'    recent RStudio projects.
open_recent_proj_list <- function() {
  open_in_rs(path = get_path_recent_proj_list())
}


#' Open RStudio project.
#'
#' Open RStudio project by name or interactively.
#'
#' @param name (string|`NULL`) The name of the project or `NULL` to choose
#'       a project interactively.
#' @param proj_list (data frame) The result of [read_projects()] or `NULL`.
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
#' \dontrun{\donttest{
#'
#' open_project()
#'
#' open_project("bs")
#'
#' open_project("R-2019-project")
#'
#' }}
#
open_project <- function(name = NULL, new_session = FALSE, proj_list = NULL,
  proj_list_path = get_path_recent_proj_list(), only_existing = TRUE) {

  if (is.null(proj_list)) {

    if (!fs::file_exists(proj_list_path)) {
      usethis::ui_stop("The file {usethis::ui_path(proj_list_path)} was not found.")
    }

    proj_list <- read_projects(proj_list_path)
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


# Personal projects ==========================================================
#' @rdname open_project
#' @export
get_path_personal_proj_list <- function() {
  fs::path(get_path_r_user_dir(), "personal-project-list-r")
  # "D:/Dokumentai/R/bs/data-raw/project-list"
}

#' @rdname open_project
#' @export
open_personal_proj_list <- function() {
  open_in_rs(path = get_path_personal_proj_list())
}

#' @rdname open_project
#' @export
update_personal_proj_list <- function() {
  file_vg  <- get_path_personal_proj_list()
  new_list <- get_projs_all()
  readr::write_lines(new_list$path, path = file_vg)
}

#' @rdname open_project
#' @export
open_project_personal <- function(name = NULL, new_session = FALSE, ...) {
  new_list <- get_projs_all()
  open_project(new_session = new_session, proj_list = new_list, ...)
}

#' @rdname open_project
#' @export
get_projs_all <- function() {

  file_recent <- get_path_recent_proj_list()
  file_vg     <- get_path_personal_proj_list()

  new_list <-
    dplyr::bind_rows(
      read_projects(file = file_recent),
      read_projects(file = file_vg)
    ) %>%
    dplyr::distinct()

  new_list
}
