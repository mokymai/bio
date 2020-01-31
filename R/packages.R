# ========================================================================== ~
# PACKAGES -------------------------------------------------------------------
# ========================================================================== ~

# Base of URL for files of "bio" repository on GitHub
url_bio <- function(file = NULL) {
  paste0("https://raw.githubusercontent.com/mokymai/bio/master/install-r/", file)
}
path_bio <- function(file = NULL) {
  system.file("install-r", file, package = "bio")
}
is_url_accessible <- function(str) {
  pingr::is_online() && pingr::is_up(str)
}
# to_str_vector(LETTERS)
to_str_vector <- function(str, quotes = '"', collapse = ", ") {
  paste0(quotes, str, quotes, collapse = collapse) %>%
    structure(., class = "glue")
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
#' @param which (character) The name of the list with recommended packages.
#' @return Data frame with column `"package"`.
#' @family R-packages-related functions
#' @examples
#' \dontrun{\donttest{
#' which <- "r209"
#' head(get_pkgs_recommended("r209"))
#'
#' }}
#'
get_pkgs_recommended <- function(which, local_list = FALSE) {

  file <- get_path_pkgs_recommended(which, local_list)
  ln <- readLines(file, encoding = "UTF-8")

  data.frame(
    # Remove R comments and trim whitespace.
    package = trimws(gsub("#.*?$", "", ln[!(ln == "" | grepl("^#", ln))])),
    stringsAsFactors = FALSE
  )
}

# get_path_pkgs_recommended("r209", TRUE)
# get_path_pkgs_recommended("r209", FALSE)
get_path_pkgs_recommended <- function(which, local_list) {
  which <- tolower(which)
  base_name <- paste0("pkgs-recommended--", which, ".txt")

  if (isTRUE(local_list)) {
    file <- path_bio(base_name)
    if (!file.exists(file)) {
      stop("List '", which, "' was not found.")
    }

  } else {
    file <- url_bio(base_name)
  }
  file
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
get_pkgs_req_version <- function(local_list = FALSE) {
  file <- get_path_pkgs_req_version(local_list)
  tbl <-
    read.table(file, skip = 10, header = TRUE, sep = "|", na.strings = c("NA", "-"),
      strip.white = TRUE, stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

# (file <- get_path_pkgs_req_version(TRUE))
# rmarkdown::yaml_front_matter(file)
# get_path_pkgs_req_version(FALSE)
get_path_pkgs_req_version <- function(local_list) {
  base_name <- "pkgs-required-version.txt"

  if (isTRUE(local_list)) {
    file <- path_bio(base_name)
    if (!file.exists(file)) {
      stop("File '", base_name, "' was not found.")
    }

  } else {
    file <- url_bio(base_name)
  }
  file
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
#' Get details about non-CRAN package installation.
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
#'
#' }}

get_pkgs_non_cran_installation_details <- function(local_list = FALSE) {
    file <- get_path_pkgs_non_cran_installation_details(local_list)

  tbl <- read.table(file, skip = 10, header = TRUE, sep = "|", strip.white = TRUE,
    na.strings = c("NA", "-"), stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

get_path_pkgs_non_cran_installation_details <- function(local_list) {

  base_name <- "pkgs-install-from.txt"

  if (isTRUE(local_list)) {
    file <- path_bio(base_name)
    if (!file.exists(file)) {
      stop("File '", base_name, "' was not found.")
    }

  } else {
    file <- url_bio(base_name)
  }
  file
}

# ===========================================================================~
#' @rdname get_pkgs_installation_status
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' head(get_pkgs_installation_status_local("r209"))
#'
#' }}
get_pkgs_installation_status_local <- function(which, local_list = TRUE) {

  pkgs_inst  = get_pkgs_installed()
  pkgs_rec   = get_pkgs_recommended(local_list = local_list, which = which)
  pkgs_req_v = get_pkgs_req_version(local_list = local_list)

  tmp <- merge(pkgs_rec, pkgs_inst, by = "package", all.x = TRUE)
  tmp <- merge(tmp, pkgs_req_v,     by = "package", all.x = TRUE)

  tmp$is_installed <- !is.na(tmp$current_version)
  tmp$update_is_required <- with(tmp, compare_version(current_version, required_version) < 0)
  tmp$required_version[is.na(tmp$required_version )] <- ""
  attr(tmp, "packages_to_update") <- tmp$package[tmp$update_is_required]
  tmp
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get package installation status.
#'
#' @param show (character)
#' - If `"required"`, only packages, that need to be installed/updated are shown.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' \dontrun{\donttest{
#'
#' which <- "r209"
#'
#' get_pkgs_installation_status("r209")
#' (status_all <- get_pkgs_installation_status("r209", show = "all"))
#'
#' get_pkgs_installation_code(status_all)
#' get_pkgs_installation_code_cran(status_all)
#' get_pkgs_installation_code_github(status_all)
#' }}
get_pkgs_installation_status <- function(which, show = "required", local_list = FALSE) {

  pkgs_status <- get_pkgs_installation_status_local(which = which,
    local_list = local_list)
  pkgs_cran   <- get_pkgs_cran_details()
  pkgs_other  <- get_pkgs_non_cran_installation_details(local_list = local_list)

  tmp <- pkgs_status
  tmp <- merge(tmp, pkgs_cran,      by = "package", all.x = TRUE)
  tmp <- merge(tmp, pkgs_other,     by = "package", all.x = TRUE)
  tmp$on_cran <- sapply(tmp$on_cran, isTRUE)
  tmp$newer_on_cran <- with(tmp, on_cran & (compare_version(current_version, cran_version) < 0))

  switch(
    tolower(show),
    "required" = tmp_2 <- tmp[tmp$update_is_required == TRUE, ],

    # Otherwise
    tmp_2 <- tmp
  )

  out <- list(
    status = tmp_2,
    missing_installation_code = tmp_2[(tmp_2$on_cran == FALSE & is.na(tmp_2$install_from) == TRUE), "package"],
    pkgs_to_install_or_update = tmp_2[tmp_2$update_is_required == TRUE, "package"],
    install_from_cran         = tmp_2[tmp_2$newer_on_cran, "package"],
    install_from_github       = tmp_2[sapply(tmp_2$install_from == "github", isTRUE), "details"],
    install_from_elsewhere    = tmp_2[sapply(tmp_2$install_from == "code",   isTRUE), "details"]
  )

  # tmp_2[, c("notes_version", "notes", "cran_version", "on_cran", "install_from",
  #   "details", "newer_on_cran")] <- NULL
  # out$status <- tmp_2

  # out$status <- tmp_2[, c("package", "is_installed", "current_version",
  # "required_version", "update_is_required", "cran_version", "newer_on_cran")]

  structure(
    out,
    class = c("pkgs_installation_status", "list")
  )
}

# Print method ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param x Object of interest.
#' @param ... Arguments to other methods.
#' @rdname get_pkgs_installation_status
#' @export
print.pkgs_installation_status <- function(x, ...) {
  if (nrow(x$status) == 0) {
    cat("All required packages are installed and up-to-date.")
    return()
  }

  st <-
    x$status[ , c("package", "is_installed", "current_version",
      "required_version", "update_is_required")] # , "cran_version", "newer_on_cran"
  st$current_version  <- ifelse(is.na(st$current_version), "-", st$current_version)
  st$required_version <- ifelse(st$required_version == "", "-", st$required_version )
  rownames(st) <- NULL
  print(st)

  if (length(x$missing_installation_code) > 0) {
    warning(
      call. = FALSE,
      "Missing installation code for: ",
      paste(x$missing_installation_code, sep = ", ")
    )
  }
}

# Installation code
#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code <- function(x) {
  styler::style_text(
    c(
      get_pkgs_installation_code_cran(x),
      get_pkgs_installation_code_github(x)
    )
  )
}

#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code_cran <- function(x) {
  # Install from CRAN only if the version of package is newer on CRAN
  pkgs <- to_str_vector(x$install_from_cran, collapse = ",\n")
  if (length(pkgs) == 0) {
    return("")
  }
  res <- paste0("install.packages(", pkgs , ")")
  styler::style_text(res)
}

#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code_github <- function(x) {
  pkgs <- to_str_vector(x$install_from_github, collapse = ",\n")
  if (length(pkgs) == 0) {
    return("")
  }
  res <- paste0(
    "remotes::install_github(dependencies = TRUE, upgrade = FALSE,\n", pkgs , ")"
  )
  styler::style_text(res)
}

