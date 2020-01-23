# ========================================================================== ~
# PACKAGES -------------------------------------------------------------------
# ========================================================================== ~

#' Compare version numbers.
#'
#' @param v_installed vector with version numbers
#' @param v_required vector with version numbers
#'
#' @return The same as in [utils::compareVersion()], just a vector.
#' @export
#' @family R-packages-related functions
#' @examples
#' compare_version("2.4", "2")
#'
#' compare_version("2.3", "2.3")
#'
#' compare_version("2.3", "2.3.1")

compare_version <- function(v_installed, v_required) {

  busena <- numeric(length(v_installed))

  v_installed <- as.character(v_installed)
  v_required  <- as.character(v_required)

  for (i in seq_along(busena)) {
    busena[i] <- utils::compareVersion(v_installed[i], v_required[i])
  }
  busena
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' remove_ignored_rows
#' Removes rows in column `ignore` with value `TRUE` and then removes the culumn
#' itself.
#' @param tbl A data frame.
#' @noRd
#' @keywords internal
remove_ignored_rows <- function(tbl) {
  ingnore_col <- names(tbl) %in% c("ignore")
  subset(tbl, !sapply(as.logical(tbl$ignore), FUN = isTRUE))[ , -which(ingnore_col)]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' List packages installed on this computer.
#'
#' @return Data frame with columns `"package"` and `"current_version"`.
#' @export
#' @family R-packages-related functions
#' @examples
#' head(get_pkgs_installed())
get_pkgs_installed <- function() {
  pkgs_existing <- installed.packages()[, c("Package", "Version")]
  rownames(pkgs_existing) <- NULL
  # colnames(pkgs_existing) <- c("paketas", "turima_versija")
  colnames(pkgs_existing) <- c("package", "current_version")
  as.data.frame(pkgs_existing, stringsAsFactors = FALSE)
}

#' List of packages of interest.
#'
#' @return Data frame with column `"package"`.
#' @family R-packages-related functions
#' @examples
#' head(get_pkgs_recommended())
get_pkgs_recommended <- function(file = "install-r/pkgs-recommended.txt") {

  ln <- readLines(file, encoding = "UTF-8")

  data.frame(
    package = ln[!(ln == "" | grepl("^#", ln))],
    stringsAsFactors = FALSE
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get required version of packages.
#'
#' Get required version of packages from a list in a file.
#'
#' @return Dataframe with columns  "package" and "required_version".
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' head(get_pkgs_req_version())
get_pkgs_req_version <- function(file = "install-r/pkgs-required-version.txt") {
  tbl <-
    read.table(file, skip = 2, header = TRUE, sep = "|", quote = "'",
      strip.white = TRUE, stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get modified package instalation code.
#'
#' Get installation code of packages that either should be installed not from
#' CRAN or a modified code shold be installed.
#'
#' @return Dataframe with columns
#'   - `"package"` (package name),
#'   - `"type"` (type of input in column `details`. Currently supported values
#'               are "code" and "github")
#'   - `"details"` (either installation code of repository name)
#'   - `"notes"`  (notes for user)
#'
#' @details
#' If the file has column `ignore` with value `TRUE`, the line is not included
#' in the output.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' head(get_pkgs_installation_details())
#'
#' head(get_pkgs_installation_code())
get_pkgs_installation_details <- function(file = "install-r/pkgs-install-from.txt") {
  tbl <- read.table(file, skip = 2, header = TRUE, sep = "|", quote = "'",
    strip.white = TRUE, stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

#' @name get_pkgs_installation_details
#' @export
get_pkgs_installation_code <- function() {

}

#' Add column with package instalation code.
#'
#' Add column with installation code for each provided package.
#'
#' @param pkgs_df Data frame with columns "paketas" (character) and "on_cran" (logical)
#'
#' @return The same data frame with additional columns
#'  - "diegimo_kodas"  -- strings with code; if a package is not on cran and its
#'  installation code is not provided, NA value is returned.
#'  - "ar_mod_kodas" -- logical, if modified code for instalation should be used.
#' @export
#'
# @examples
# my_pkgs <- head(get_pkgs_recommended())
# my_pkgs <- get_pkgs_recommended()[c(17, 1, 2, 3, 87),]
# my_pkgs$on_cran[1] <- FALSE
#
# add_pkgs_installation_code(my_pkgs)
#
add_pkgs_installation_code <- function(pkgs_df) {

  pkgs_df$..nr <- 1:nrow(pkgs_df)
  pkgs_code <- get_pkgs_installation_code()

  pkgs_df <- merge(pkgs_df, pkgs_code, all.x = TRUE, sort = FALSE)
  pkgs_df <- pkgs_df[order(pkgs_df$..nr), ]# Sort

  pkgs_df$ar_mod_kodas <- !is.na(pkgs_df$diegimo_kodas)

  cran_code <-
    paste0('install.packages("', pkgs_df$paketas, '", dependencies = TRUE)')
  # paste0('install.packages("', pkgs_df$paketas, '", dependencies = TRUE, quiet = TRUE)')


  pkgs_df$diegimo_kodas <-
    ifelse(!pkgs_df$ar_mod_kodas,
      ifelse(pkgs_df$on_cran, cran_code, NA_character_),
      trimws(pkgs_df$diegimo_kodas))

  pkgs_df$..nr <- NULL
  pkgs_df
}



#' Get infromation about recommended packages
#'
#' @return Data frame
#' @export
#'
# @examples
# rez <- get_pkgs_instalation_status()
#
# tibble::as_tibble(head(rez))
#
# cran_paketai <-
#   get_pkgs_instalation_status() %>%
#   dplyr::filter(on_cran) %>%
#   dplyr::pull(paketas) %>%
#   stringr::str_c('"',.,'"', collapse = ", ") %>%
#   structure(class = "glue")

# ne_cran_diegimo_kodas <-
#   get_pkgs_instalation_status() %>%
#   dplyr::filter(ar_mod_kodas) %>%
#   dplyr::pull(diegimo_kodas) %>%
#   structure(class = "glue")
#

# TODO: toliau netaisyta
# tmp3 <- tmp2[, c(-2, -5, -6)]
# tmp3 <- setNames(tmp3, c("Paketas", "Idiegta_versija", "Reikiama_min_versija"))

#' get_pkgs_instalation_status
#'
#' @return Data frame with ???
#' @export
#'
#' @examples
#' head(get_pkgs_instalation_status())
get_pkgs_instalation_status <- function() {
  pkgs_rec   <- get_pkgs_recommended()
  pkgs_inst  <- get_pkgs_installed()
  pkgs_req_v <- get_pkgs_req_version()

  tmp <- merge(pkgs_rec, pkgs_inst, by = "package", all.x = TRUE)
  tmp <- merge(tmp, pkgs_req_v,     by = "package", all.x = TRUE)

  tmp$needs_update <- with(tmp, compare_version(current_version, required_version) < 0)
  tmp$current_version[is.na(tmp$current_version)]    <- "[ not installed ]"
  tmp$required_version[is.na(tmp$required_version )] <- ""

  attr(tmp, "packages_to_update") <- tmp$package[tmp$needs_update]

  # tmp <- add_pkgs_installation_code(pkgs_df = tmp)
  #
  # tmp <- tmp[order(tmp$nr), ]
  # rownames(tmp) <- NULL
  tmp
  # tibble::as_tibble(tmp)
}
# get_pkgs_instalation_status <- function() {
#   tmp <- get_pkgs_recommended()
#   tmp <- merge(tmp, get_pkgs_installed(),   by = "paketas", all.x = TRUE)
#   tmp <- merge(tmp, get_pkgs_req_version(), by = "paketas", all.x = TRUE)
#
#   tmp$reikia_atnaujinti <- with(tmp, compare_version(turima_versija, reikiama_versija) < 0)
#   tmp$turima_versija[is.na(tmp$turima_versija)] <- "[ neidiegta ]"
#   tmp$reikiama_versija[is.na(tmp$reikiama_versija )] <- ""
#
#   tmp <- add_pkgs_installation_code(pkgs_df = tmp)
#
#   tmp <- tmp[order(tmp$nr), ]
#   rownames(tmp) <- NULL
#   return(tmp)
#   # tibble::as_tibble(tmp)
#
# }



