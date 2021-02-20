#' @name get_rstudio_file_ids
#' @title Get RStudio document IDs and paths
#' @description Get RStudio file IDs and paths.
#'
#' @return Data frame with columns `id` (for IDs) and `path` (for file paths).
#'
#' @concept paths and dirs
#' @noRd
#'
#' @examples
#' \dontrun{\donttest{
#' head(get_rstudio_file_ids_user())
#' }}
get_rstudio_file_ids <- function() {
  dplyr::bind_rows(
    get_rstudio_file_ids_user(),
    get_rstudio_file_ids_project()
  )
}

# @rdname get_rstudio_file_ids
# @concept paths and dirs
# @export
get_rstudio_file_ids_user <- function() {
  f_dir <- get_path_rstudio_internal_state_dir("notebooks/paths")

  if (!dir.exists(f_dir)) {
    return(NULL)
  }

  f_dir %>%
    readr::read_lines() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      id   = stringr::str_extract(value, '(?<=").*?(?="$)'),
      path = stringr::str_extract(value, '^.*?(?==")')
    )
}

# @rdname get_rstudio_file_ids
# @concept paths and dirs
# @export
get_rstudio_file_ids_project <- function() {
  # TODO: Not implemented yet
  NULL
}
