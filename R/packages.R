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
#' @export
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
#' which <- "r209"
#' head(get_pkgs_installation_status_local("r209"))
#'
#' }}
get_pkgs_installation_status_local <- function(which, local_list = TRUE) {

  pkgs_inst  = get_pkgs_installed()
  pkgs_rec   = get_pkgs_recommended(local_list = local_list, which = which)
  pkgs_req_v = get_pkgs_req_version(local_list = local_list)

  pkgs_init <- merge(pkgs_rec, pkgs_inst, by = "package", all.x = TRUE)
  pkgs_init <- merge(pkgs_init, pkgs_req_v,     by = "package", all.x = TRUE)

  pkgs_init$is_installed <- !is.na(pkgs_init$current_version)

  pkgs_init$update_is_required <-
    with(pkgs_init, compare_version(current_version, required_version) < 0)

  pkgs_init$required_version[is.na(pkgs_init$required_version )] <- ""

  attr(pkgs_init, "packages_to_update") <-
    pkgs_init$package[pkgs_init$update_is_required]

  pkgs_init
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get package installation status and code.
#'
#' Get package installation status (e.g., if packages of interest are installed
#' or need to be updated) and package installation code.
#'
#' @param include (character) Which packages from the list (indicated in `which`)
#'        must be included in the results.
#'        One of:
#'        - `outdated` (default): only the packages that need are not installed
#'           or do not have a minimum required version installed.
#'        - `missing `: only the packages that are not installed to be installed.
#'        - `always `: all packages.
#'
#' @param show_status (character) Which packages should be included in the
#'        package installation status summary. One of "outdated",
#'        "missing", "always" (see `include`). Defaults to the value of `include`.
#'
#' @param install (character) Which packages should be included in the
#'        package installation code. One of "outdated", "missing", "always"
#'        (see `include`). Defaults to the value of `include`. Sets the default
#'        value for `from_cran_if`, `from_github_if`, and `from_elsewhere_if`.
#'
#' @param from_cran_if (character) Condition to filter packages that should be
#'        included in code that installs packages from CRAN. One of "outdated",
#'        "missing", "always" (see `include`) as well as "newer_on_cran" and
#'        "required" (see below). Defaults to the value of `install`.
#'        - `newer_on_cran` -- if CRAN version is newer than the installed one
#'           (even if minimum required version is installed).
#'        - `required` -- packages that do not have a minimum required version
#'          installed even if the required version is not on CRAN.
#'
#' @param from_github_if (character) Condition to filter packages that should be
#'        included in code that installs packages from GitHub. One of "outdated",
#'        "missing", "always" (see `include`). Defaults to the value of `install`.
#'
#' @param from_elsewhere_if (character) Condition to filter packages that should
#'        be included in code that installs packages from other sources. One of
#'        "outdated", "missing", "always" (see `include`). Defaults to the value
#'         of `install`.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' \dontrun{\donttest{
#'
#' which <- "r209"
#'
#' (status_out <- get_pkgs_installation_status("r209"))
#' get_pkgs_installation_code(status_out)
#'
#' (status_all <- get_pkgs_installation_status("r209", include = "always"))
#' get_pkgs_installation_code(status_all)
#'
#' (status_custom <-
#'   get_pkgs_installation_status("r209", include = "always", install = "outdated"))
#' get_pkgs_installation_code(status_custom)
#'
#' }}
get_pkgs_installation_status <- function(which, include = "outdated",
  show_status = include, install = include, from_cran_if = install,
  from_github_if = install, from_elsewhere_if = install, local_list = FALSE) {

  choices <- c("outdated", "missing", "always", "never", FALSE)

  include           <- match.arg(include, choices)
  show_status       <- match.arg(show_status, choices)
  install           <- match.arg(install, choices)
  from_cran_if      <- match.arg(from_cran_if, c(choices, "newer_on_cran", "required"))
  from_github_if    <- match.arg(from_github_if,    choices)
  from_elsewhere_if <- match.arg(from_elsewhere_if, choices)

  pkgs_init  <- get_pkgs_installation_status_local(which = which, local_list = local_list)
  pkgs_cran  <- get_pkgs_cran_details()
  pkgs_other <- get_pkgs_non_cran_installation_details(local_list = local_list)

  pkgs_init <- merge(pkgs_init, pkgs_cran,  by = "package", all.x = TRUE)
  pkgs_init <- merge(pkgs_init, pkgs_other, by = "package", all.x = TRUE)
  pkgs_init$on_cran <- sapply(pkgs_init$on_cran, isTRUE)
  pkgs_init$newer_on_cran <-
    with(pkgs_init, on_cran & (compare_version(current_version, cran_version) < 0))

  # Sort columns
  first_cols <- c("package", "is_installed", "current_version",
    "required_version", "update_is_required", "cran_version", "newer_on_cran")
  pkgs_init <-
    pkgs_init[, c(first_cols, setdiff(colnames(pkgs_init), first_cols))]

  # Filter packages of interest
  pkgs <-
    switch(
      include,
      "missing"  = pkgs_init[!pkgs_init$is_installed, ],
      "outdated" = pkgs_init[ pkgs_init$update_is_required, ],
      "always"   = pkgs_init,
      stop("Unknown value of `include`: ", include)
    )

  missing_installation_code <-
    pkgs[(pkgs$on_cran == FALSE & is.na(pkgs$install_from) == TRUE), "package"]

  pkgs_to_install_or_update <- pkgs[pkgs$update_is_required == TRUE, "package"]


  # Show status
  from_code <- sapply(pkgs$install_from == "code", isTRUE)

  show_status_cond <-
    switch(show_status,
      "outdated" =  pkgs$update_is_required,
      "missing"  = !pkgs$is_installed,
      "always"   =  rep(TRUE, nrow(pkgs)),
      "newer" = ,
      "FALSE"    = rep(TRUE, nrow(pkgs)),
      stop("Unknown value of `show_status`: ", show_status)
    )

  status_df <- pkgs[show_status_cond, ]


  # From CRAN code
  from_cran_cond <-
    switch(from_cran_if,
      "newer_on_cran" = pkgs$newer_on_cran,
      "outdated"      = pkgs$newer_on_cran &  pkgs$update_is_required,
      "required"      = pkgs$on_cran       &  pkgs$update_is_required,
      "missing"       = pkgs$on_cran       & !pkgs$is_installed,
      "always"        = pkgs$on_cran,
      "newer"         = ,
      "FALSE"         = rep(FALSE, nrow(pkgs)),
      stop("Unknown value of `from_cran_if`: ", from_cran_if)
    )

  install_from_cran  <- pkgs[from_cran_cond, "package"]

  # From GitHub code
  on_github <- sapply(pkgs$install_from == "github", isTRUE)

  from_github_cond <-
    switch(from_github_if,
      "outdated" = on_github &  pkgs$update_is_required,
      "missing"  = on_github & !pkgs$is_installed,
      "always"   = on_github,
      "newer"    = ,
      "FALSE"    = rep(FALSE, nrow(pkgs)),
      stop("Unknown value of `from_github_if`: ", from_github_if)
    )

  install_from_github <- pkgs[from_github_cond, "details"]

  # Custom instalation code
  from_code <- sapply(pkgs$install_from == "code", isTRUE)

  from_code_cond <-
    switch(from_elsewhere_if,
      "outdated" = from_code &  pkgs$update_is_required,
      "missing"  = from_code & !pkgs$is_installed,
      "always"   = from_code,
      "newer"    = ,
      "FALSE"    = rep(FALSE, nrow(pkgs)),
      stop("Unknown value of `from_elsewhere_if`: ", from_elsewhere_if)
    )

  install_from_elsewhere <- pkgs[from_code_cond, "details"]

  # Unknown options of `install_from`
  # TODO: this part of code is not finished:
  install_from_unknown <-
    unique(pkgs$install_from[!pkgs$install_from %in% c("github", "code", NA)])

  # Output structure
  out <- list(
    status                    = status_df,
    missing_installation_code = missing_installation_code,
    pkgs_to_install_or_update = pkgs_to_install_or_update,
    install_from_cran         = install_from_cran,
    install_from_github       = install_from_github,
    install_from_elsewhere    = install_from_elsewhere,
    install_from_unknown      = install_from_unknown
  )

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
  print(tibble::as_tibble(st), n = Inf, width = Inf, n_extra = Inf)

}

#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code <- function(x) {
  UseMethod("get_pkgs_installation_code")
}

# Installation code
#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code.pkgs_installation_status <- function(x) {
  res <- styler::style_text(
    c(
      get_pkgs_installation_code_cran(x),
      get_pkgs_installation_code_github(x),
      get_pkgs_installation_code_other(x)
    )
  )

  # Warn if code is missing
  if (length(x$missing_installation_code) > 0) {
    warning(
      call. = FALSE,
      "Missing installation code for: ",
      paste(x$missing_installation_code, sep = ", ")
    )
  }

  # Output
  res

}

#' @rdname get_pkgs_installation_status
#  @export
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
#  @export
get_pkgs_installation_code_github <- function(x) {
  pkgs <- to_str_vector(x$install_from_github, collapse = ",\n")
  if (length(pkgs) == 0) {
    return("")
  }
  res <- paste0(
    "remotes::install_github(\n", pkgs , ",\n dependencies = TRUE, upgrade = FALSE)"
  )
  styler::style_text(res)
}

#' @rdname get_pkgs_installation_status
#  @export
get_pkgs_installation_code_other <- function(x) {
  if (length(x$install_from_elsewhere) == 0) {
    return("")
  }

  styler::style_text(x$install_from_elsewhere)
}

