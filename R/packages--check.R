# ========================================================================== ~
# PACKAGES -------------------------------------------------------------------
# ========================================================================== ~

# Base of URL for files of "bio" repository on GitHub
url_bio <- function(file = NULL) {
  paste0(
    "https://raw.githubusercontent.com/mokymai/bio/master/inst/install-r/",
    file
  )
}

# Path to files of installed "bio" package on your machine
path_bio <- function(file = "") {
  system.file("install-r", file, package = "bio")
}

path_bio_rs <- function(file = "") {
  system.file("rs-settings", "", package = "bio")
}

to_str_vector <- function(str, quotes = '"', collapse = ", ") {
  paste0(quotes, str, quotes, collapse = collapse) |>
    structure(class = "glue")
}

base_r_packages <- function() {
  rownames(installed.packages(priority = "base"))
}

#' Compare Version Numbers
#'
#' @param v_installed vector with installed version numbers
#' @param v_required vector with required version numbers
#'
#' @return The same as in [utils::compareVersion()], just a vector.
#' @export
#'
#' @family R-packages-related functions
#'
#' @concept utilities
#'
#' @examples
#'
#' compare_version("2.4", "2")
#'
#' compare_version("2.3", "2.3")
#'
#' compare_version("2.3", "2.3.1")
#'
compare_version <- function(v_installed, v_required) {

  result <- numeric(length(v_installed))

  v_installed <- as.character(v_installed)
  v_required  <- as.character(v_required)

  for (i in seq_along(result)) {
    result[i] <- utils::compareVersion(v_installed[i], v_required[i])
  }
  result
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' remove_ignored_rows
#' Removes rows in column `ignore` with value `TRUE` and then removes the column
#' itself.
#' @param tbl A data frame.
#' @noRd
#' @keywords internal
remove_ignored_rows <- function(tbl) {
  ignore_col <- names(tbl) %in% c("ignore")
  subset(tbl, !sapply(as.logical(tbl$ignore), FUN = isTRUE))[, -which(ignore_col)]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check if package is installed
#'
#' @param pkgs (character) A list of installed packages.
#'
#' @return A logical vector for each input element.
#' @export
#'
#' @concept packages
#'
#' @examples
#'
#' is_pkg_installed("bio")
#'
#' is_pkg_installed(c("bio", "utils", "grugru"))
#'
is_pkg_installed <- function(pkgs) {
  pkgs %in% .packages(all.available = TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' List packages installed on this computer
#'
#' @param rm_duplicates (logical) Should duplicated names of packages be removed?
#'        If `TRUE`, when several packages are found, only the one with the
#'        highest version is returned. If `FALSE`, no packages are removed
#'        from the list.
#' @return Data frame with columns `"package"` and `"current_version"`.
#'
#' @export
#' @family R-packages-related functions
#'
#' @concept packages
#'
#' @examples
#'
#' head(get_pkgs_installed())
#'
#' nrow(get_pkgs_installed(rm_duplicates = TRUE))
#' nrow(get_pkgs_installed(rm_duplicates = FALSE))
#'
get_pkgs_installed <- function(rm_duplicates = TRUE) {
  pkgs_existing <- installed.packages()[, c("Package", "Version")]
  rownames(pkgs_existing) <- NULL
  colnames(pkgs_existing) <- c("package", "current_version")
  df <- as.data.frame(pkgs_existing, stringsAsFactors = FALSE)

  if (isTRUE(rm_duplicates)) {
    df |>
      dplyr::group_by(package) |>
      dplyr::group_modify(
        ~ dplyr::filter(.x, current_version == max(current_version))
      ) |>
      dplyr::ungroup() |>
      dplyr::distinct() |>
      as.data.frame(stringsAsFactors = FALSE)

  } else {
    df

  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get Details About Packages on CRAN
#'
#' Convenience function based on [utils::available.packages()].
#'
#' @param repos Additional repos to check.
#'
#' @return
#' Data frame with columns "package", "cran_version", "on_cran".
#'
#' @noRd
#'
#' @family R-packages-related functions
#'
#' @seealso [utils::available.packages()]
#'
#' @concept packages
#'
#' @examples
#' if (interactive()) {
#'   # NOTE: Internet connection is needed.
#'   head(get_pkgs_cran_details())
#' }
get_pkgs_cran_details <- function(repos = NULL) {
  repos <- unique(c(repos, getOption("repos")))

  cran_all <-
    data.frame(
      available.packages(repos = repos)[, c("Package", "Version")],
      on_cran = TRUE,
      stringsAsFactors = FALSE
    )
  rownames(cran_all) <- NULL
  colnames(cran_all) <- c("package", "cran_version", "on_cran")
  cran_all
}

#' Get previous package versions available on CRAN
#'
#' Function to scrape the CRAN website and retrieve archived (old) package versions
#'
#' @param package (character)
#'        Package name.
#'
#' @return Vector with version numbers (the current version is not present).
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
#' pkg_list_archived_versions("ggplot2")
#'
#' pkg_list_archived_versions("none")
pkg_list_archived_versions <- function(package) {
  # Base URL of archived packages on CRAN
  url <- "https://cran.r-project.org/src/contrib/Archive/"

  # Create the complete URL for the package
  package_url <- paste0(url, package)

  # Extract the webpage table rows containing the package versions
  rows <- try(
    suppressWarnings(readLines(package_url, warn = FALSE)),
    silent = TRUE
  )

  if (inherits(rows, "try-error")) {
    return(as.numeric_version(NULL))
  }

  # Extract the archived versions
  rows |>
    stringr::str_extract(stringr::str_glue("{package}_(.*?)[.]tar[.]gz"), 1) |>
    na.omit() |>
    as.numeric_version() |>
    sort(decreasing = TRUE)
}
