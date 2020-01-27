# ========================================================================== ~
# PACKAGES -------------------------------------------------------------------
# ========================================================================== ~

# Base of URL for files of "bio" sepository on GitHub
url_bio <- function(file = NULL) {
  paste0("https://raw.githubusercontent.com/mokymai/bio/master/install-r/", file)
}

#' Compare version numbers.
#'
#' @param v_installed vector with installed version numbers
#' @param v_required vector with required version numbers
#'
#' @return The same as in [utils::compareVersion()], just a vector.
#' @export
#' @family R-packages-related functions
#' @examples
#'
#' compare_version("2.4", "2")
#'
#' compare_version("2.3", "2.3")
#'
#' compare_version("2.3", "2.3.1")
#'
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
#' \dontrun{\donttest{
#'
#' head(get_pkgs_installed())
#'
#' }}
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
#' \dontrun{\donttest{
#'
#' head(get_pkgs_recommended())
#'
#' }}
get_pkgs_recommended <- function(file = url_bio("pkgs-recommended.txt")) {

  ln <- readLines(file, encoding = "UTF-8")

  data.frame(
    # Remove R comments and trim whitespace.
    package = trimws(gsub("#.*?$", "", ln[!(ln == "" | grepl("^#", ln))])),
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
#' \dontrun{\donttest{
#'
#' head(get_pkgs_req_version())
#'
#' }}
get_pkgs_req_version <- function(file = url_bio("pkgs-required-version.txt")) {
  tbl <-
    read.table(file, skip = 2, header = TRUE, sep = "|", na.strings = c("NA", "-"),
      strip.white = TRUE, stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get details about package on CRAN.
#'
#' Convenience function based on [base::available.packages()].
#' @return
#' Data frame with columns "package", "cran_version", "on_cran".
#'
#' @export
#' @family R-packages-related functions
#'
#' @seealso [base::available.packages()]
#'
#' @examples
#' \dontrun{\donttest{
#'
#' head(get_pkgs_cran_details())
#'
#' }}
get_pkgs_cran_details <- function() {
  cran_all <-
    data.frame(
      available.packages()[ , c("Package", "Version")],
      on_cran = TRUE,
      stringsAsFactors = FALSE
    )
  rownames(cran_all) <- NULL
  colnames(cran_all) <- c("package", "cran_version", "on_cran")
  cran_all
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get details about non-CRAN package instalation.
#'
#' Get installation code of packages that either should be installed not from
#' CRAN or a modified code shold be installed.
#'
#' @return Dataframe with columns:
#'   - `"package"` package name,
#'   - `"install_from"` source to install from. Currently supported values
#'               are "code" and "github"
#'   - `"details"` either installation code of repository name
#'   - `"notes"`  notes for user.
#'
#' @details
#' If the file has column `ignore` with value `TRUE`, the line is not included
#' in the output.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' \dontrun{\donttest{
#'
#' head(get_pkgs_non_cran_installation_details())
#'
#' head(get_pkgs_installation_code())
#'
#' }}

get_pkgs_non_cran_installation_details <- function(file = url_bio("pkgs-install-from.txt")) {

  tbl <- read.table(file, skip = 2, header = TRUE, sep = "|", strip.white = TRUE,
    na.strings = c("NA", "-"), stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get package installation status.
#'
#' @param outdated_only (logical) If `TRUE`, only packages, tat need to be
#'        installed/updated are shown.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' \dontrun{\donttest{
#'
#' get_pkgs_instalation_status()
#'
#' }}
get_pkgs_instalation_status <- function(outdated_only = TRUE) {

  pkgs_status <- get_pkgs_instalation_status_local()
  pkgs_cran   <- get_pkgs_cran_details()
  pkgs_other  <- get_pkgs_non_cran_installation_details()

  tmp <- pkgs_status
  tmp <- merge(tmp, pkgs_cran,      by = "package", all.x = TRUE)
  tmp <- merge(tmp, pkgs_other,     by = "package", all.x = TRUE)
  tmp$on_cran <- sapply(tmp$on_cran, isTRUE)
  tmp$newer_on_cran <- with(tmp, on_cran & (compare_version(current_version, cran_version) < 0))

  if (isTRUE(outdated_only)) {
    tmp_2 <- tmp[tmp$needs_update == TRUE, ]
  }

  out <- list(
    status = tmp_2,
    missing_installation_code = tmp_2[(tmp_2$on_cran == FALSE & is.na(tmp_2$install_from) == TRUE), "package"],
    needs_update             = tmp_2[tmp_2$needs_update == TRUE, "package"],
    install_from_cran        = tmp_2[tmp_2$newer_on_cran, "package"],
    install_from_github      = tmp_2[sapply(tmp_2$install_from == "github", isTRUE), "details"],
    install_from_elsewhere   = tmp_2[sapply(tmp_2$install_from == "code",   isTRUE), "details"]
  )

  # tmp_2[, c("notes_version", "notes", "cran_version", "on_cran", "install_from",
  #   "details", "newer_on_cran")] <- NULL
  # out$status <- tmp_2

  out$status <- tmp_2[, c("package", "current_version", "is_installed",
    "required_version", "needs_update", "cran_version", "newer_on_cran")]

  structure(
    out,
    class = c("pkgs_instalation_status", "list")
  )
}

print.pkgs_instalation_status <- function(x, ...) {
  print(structure(x, class = "list"))
}

#' @rdname get_pkgs_instalation_status
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' head(get_pkgs_instalation_status_local())
#'
#' }}
get_pkgs_instalation_status_local <- function(
  pkgs_rec   = get_pkgs_recommended(),
  pkgs_inst  = get_pkgs_installed(),
  pkgs_req_v = get_pkgs_req_version()
) {

  tmp <- merge(pkgs_rec, pkgs_inst, by = "package", all.x = TRUE)
  tmp <- merge(tmp, pkgs_req_v,     by = "package", all.x = TRUE)

  tmp$is_installed <- !is.na(tmp$current_version)
  tmp$needs_update <- with(tmp, compare_version(current_version, required_version) < 0)
  tmp$required_version[is.na(tmp$required_version )] <- ""
  attr(tmp, "packages_to_update") <- tmp$package[tmp$needs_update]
  tmp
}





