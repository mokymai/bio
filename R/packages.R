# ========================================================================== ~
# PACKAGES -------------------------------------------------------------------
# ========================================================================== ~

# Base of URL for files of "bio" repository on GitHub
url_bio <- function(file = NULL) {
  paste0("https://raw.githubusercontent.com/mokymai/bio/master/inst/install-r/", file)
}

# Path to files of installed "bio" package on your machine
path_bio <- function(file = "") {
  system.file("install-r", file, package = "bio")
}

path_bio_rs <- function(file = "") {
  system.file("rs-settings", "", package = "bio")
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
#' Check if packae is installed
#'
#' @param pkgs (character) A list of installed packages.
#'
#' @return A logical vector for each input element.
#' @export
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
#' List packages installed on this computer.
#'
#' @return Data frame with columns `"package"` and `"current_version"`.
#' @export
#' @family R-packages-related functions
#' @examples
#'
#' head(get_pkgs_installed())
#'
get_pkgs_installed <- function() {
  pkgs_existing <- installed.packages()[, c("Package", "Version")]
  rownames(pkgs_existing) <- NULL
  # colnames(pkgs_existing) <- c("paketas", "turima_versija")
  colnames(pkgs_existing) <- c("package", "current_version")
  as.data.frame(pkgs_existing, stringsAsFactors = FALSE)
}


#' List of packages of interest.
#'
#' @inheritParams get_pkgs_installation_status
#'
#' @return Data frame with column `"package"`.
#' @export
#' @family R-packages-related functions
#' @examples
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' # Here it is used for testing purposes only.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_recommended("mini"))
#'
get_pkgs_recommended <- function(list_name,
  use_local_list = getOption("bio.use_local_list", FALSE)) {

  file <- get_path_pkgs_recommended(list_name, use_local_list)

  tryCatch({ln <- readLines(file, encoding = "UTF-8")},
    # error = function(e) {
    #   if (!pingr::is_online()) {
    #     usethis::ui_stop(paste0(
    #       "No internet connection. The online version of list ",
    #       "{usethis::ui_value(list_name)} cannot be accessed. "))
    #   } else {
    #     usethis::ui_stop(paste0(
    #       "It seems that list {usethis::ui_value(list_name)} is not present ",
    #       "online or cannot be accessed. Check if the spelling is correct."))
    #   }
    #   return()
    # },
    warning = function(w) {

      if (!pingr::is_online()) {
        usethis::ui_stop(paste0(
          "No internet connection, thus the online version of list ",
          "{usethis::ui_value(list_name)} cannot be accessed. ")
        )

      } else if (stringr::str_detect(w$message, "'404 Not Found'")) {
        usethis::ui_stop(paste0(
          "It seems that there is no online version of list ",
          "{usethis::ui_value(list_name)} or it cannot be accessed. ",
          "\nCheck if the list name is correct.")
        )

      } else {
        usethis::ui_stop(w$message)
      }

    }
  )

  data.frame(
    # Remove R comments and trim whitespace.
    package = trimws(gsub("#.*?$", "", ln[!(ln == "" | grepl("^#", ln))])),
    stringsAsFactors = FALSE
  )
}

# get_path_pkgs_recommended("gmc-r209", TRUE)
# get_path_pkgs_recommended("gmc-r209", FALSE)
get_path_pkgs_recommended <- function(list_name, use_local_list) {
  list_name <- tolower(list_name)
  base_name <- paste0("pkgs-recommended--", list_name, ".txt")

  if (isTRUE(use_local_list)) {
    file <- path_bio(base_name)
    if (!file.exists(file)) {
      usethis::ui_stop(paste0(
        "List {usethis::ui_value(list_name)} was not found on your computer. ",
        "Check if the list name is correct. "
      ))
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
#' @inheritParams get_pkgs_installation_status
#'
#' @return Dataframe with columns  "package" and "required_version".
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_req_version())
#'
get_pkgs_req_version <- function(use_local_list = getOption("bio.use_local_list", FALSE)) {
  file <- get_path_pkgs_req_version(use_local_list)
  tbl <-
    read.table(file, skip = 10, header = TRUE, sep = "|", na.strings = c("NA", "-"),
      strip.white = TRUE, stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}

# (file <- get_path_pkgs_req_version(TRUE))
# rmarkdown::yaml_front_matter(file)
# get_path_pkgs_req_version(FALSE)
get_path_pkgs_req_version <- function(use_local_list) {
  base_name <- "pkgs-required-version.txt"

  if (isTRUE(use_local_list)) {
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
#' Convenience function based on [utils::available.packages()].
#' @return
#' Data frame with columns "package", "cran_version", "on_cran".
#'
#' @export
#' @family R-packages-related functions
#'
#' @seealso [utils::available.packages()]
#'
#' @examples
#' \dontrun{\donttest{
#'
#' # NOTE: Internet connection is needed.
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
#' @inheritParams get_pkgs_installation_status
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
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_non_cran_installation_details())

get_pkgs_non_cran_installation_details <- function(
  use_local_list = getOption("bio.use_local_list", FALSE)) {
  file <- get_path_pkgs_non_cran_installation_details(use_local_list)

  tbl <- read.table(file, skip = 10, header = TRUE, sep = "|", strip.white = TRUE,
    na.strings = c("NA", "-"), stringsAsFactors = FALSE)

  remove_ignored_rows(tbl)
}


get_path_pkgs_non_cran_installation_details <- function(use_local_list) {

  base_name <- "pkgs-install-from.txt"

  if (isTRUE(use_local_list)) {
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
# Instalation status (local) -------------------------------------------------

#' @rdname get_pkgs_installation_status
#' @export
#'
#' @examples
#' head(get_pkgs_installation_status_local("mini"))

get_pkgs_installation_status_local <- function(list_name,
  use_local_list = getOption("bio.use_local_list", TRUE)) {

  pkgs_rec   <- get_pkgs_recommended(use_local_list = use_local_list, list_name = list_name)
  pkgs_inst  <- get_pkgs_installed()
  pkgs_req_v <- get_pkgs_req_version(use_local_list = use_local_list)

  pkgs_init <- dplyr::left_join(pkgs_rec,   pkgs_inst, by = "package")
  pkgs_init <- dplyr::left_join(pkgs_init, pkgs_req_v, by = "package")

  pkgs_init$is_installed <- !is.na(pkgs_init$current_version)

  pkgs_init$update_is_required <-
    with(pkgs_init, compare_version(current_version, required_version) < 0)

  pkgs_init$required_version[is.na(pkgs_init$required_version )] <- ""

  attr(pkgs_init, "packages_to_update") <-
    pkgs_init$package[pkgs_init$update_is_required]

  pkgs_init
}

# Instalation status ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name get_pkgs_installation_status
#'
#' @title Get package installation status and code.
#' @description
#' Get package installation status (e.g., if packages of interest are installed
#' or need to be updated) and package installation code.
#'
#' @param list_name (character) The name of the list with required R packages.
#'        E.g., "mini", "Rcmdr", etc.
#'
#' @param include (character) Which packages from the list (indicated in `list_name`)
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
#'        value for `cran`, `github`, and `elsewhere`.
#'
#' @param cran (character) Condition to filter packages that should be
#'        included in code that installs packages from CRAN. One of "outdated",
#'        "missing", "always" (see `include`) as well as "newer_on_cran" and
#'        "required" (see below). Defaults to the value of `install`.
#'        - `newer_on_cran` -- if CRAN version is newer than the installed one
#'           (even if minimum required version is installed).
#'        - `required` -- packages that do not have a minimum required version
#'          installed even if the required version is not on CRAN.
#'
#' @param github (character) Condition to filter packages that should be
#'        included in code that installs packages from GitHub. One of "outdated",
#'        "missing", "always" (see `include`). Defaults to the value of `install`.
#'
#' @param elsewhere (character) Condition to filter packages that should
#'        be included in code that installs packages from other sources. One of
#'        "outdated", "missing", "always" (see `include`). Defaults to the value
#'         of `install`.
#'
#' @param use_local_list (logical) If `TRUE`, the list, which is in the folder
#'         of package \pkg{bio} ("local list"), is used. If `FALSE`, the lists
#'         on "GitHub" repository of the package is used. It is recommended
#'         using the online version of the list, as it may contain more recent
#'         changes.
#'
#' @export
#' @family R-packages-related functions
#'
#' @examples
#' \dontrun{\donttest{
#'
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' options(bio.use_local_list = TRUE)
#' list_name <- "mini"
#'
#' (status_out <- get_pkgs_installation_status("mini"))
#' get_pkgs_installation_code(status_out)
#'
#' (status_all <- get_pkgs_installation_status("mini", include = "always"))
#' get_pkgs_installation_code(status_all)
#'
#' (status_custom <-
#'   get_pkgs_installation_status("mini", include = "always", install = "outdated"))
#' get_pkgs_installation_code(status_custom)
#'
#' }}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# list_name <- "gmc-r209"
#
# include <- show_status <- install <- cran <- github <- elsewhere <- "always"
#
# include <- show_status <- install <- cran <- github <- elsewhere <- "outdated"
#
# use_local_list <- TRUE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ========================================================================== ~
get_pkgs_installation_status <- function(list_name, include = "outdated",
  show_status = include, install = include, cran = install,
  github = install, elsewhere = install,
  use_local_list = getOption("bio.use_local_list", FALSE)) {

  choices <- c(TRUE, "always", "outdated", "missing", "never", FALSE)

  include     <- match.arg(as.character(include),     choices)
  show_status <- match.arg(as.character(show_status), choices)
  install     <- match.arg(as.character(install),     choices)
  cran        <- match.arg(as.character(cran),      c(choices, "newer_on_cran", "required"))
  github      <- match.arg(as.character(github),      choices)
  elsewhere   <- match.arg(as.character(elsewhere),   choices)

  status_0  <- get_pkgs_installation_status_local(list_name = list_name,
    use_local_list = use_local_list)
  pkgs_cran  <- get_pkgs_cran_details()
  pkgs_other <- get_pkgs_non_cran_installation_details(use_local_list = use_local_list)

  first_cols <- c("package", "is_installed", "current_version",
    "required_version", "update_is_required", "cran_version", "newer_on_cran")

  status <-
    status_0 %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(pkgs_cran,  by = "package") %>%
    dplyr::left_join(pkgs_other, by = "package") %>%
    dplyr::mutate(
      on_cran = purrr::map_lgl(on_cran, isTRUE),
      newer_on_cran = on_cran & (compare_version(current_version, cran_version) < 0),
      missing_installation_code = (on_cran == FALSE & is.na(install_from) == TRUE)
    ) %>%
    dplyr::select(dplyr::one_of(first_cols), dplyr::everything())

  # Unknown options of `install_from`
  # TODO: this part of code is not finished:
  install_from_unknown <-
    unique(status$install_from[!status$install_from %in% c("github", "code", NA)])


  # missing_installation_code = status[status$missing_installation_code, ]$package
  # n_to_install_or_update    = sum(status$update_is_required)
  # pkgs_to_install_or_update = status[any_to_install_or_update, ]$package

  # Output structure
  out <- list(
    list_name    = list_name,      # stirng
    status       = status,         # data frame
    show_status  = show_status,
    install_from = tibble::tibble(cran, github, elsewhere),
    missing_installation_code = status[status$missing_installation_code, ]$package,
    n_to_install_or_update    = sum(status$update_is_required)

    # missing_installation_code = missing_installation_code,
    # pkgs_to_install_or_update = pkgs_to_install_or_update,
    #
    # install_from_cran         = install_from_cran,
    # install_from_github       = install_from_github,
    # install_from_elsewhere    = install_from_elsewhere,
    # install_from_unknown      = install_from_unknown
  )

  structure(
    out,
    class = c("pkgs_installation_status", "list")
  )
}

# =~~~ methods ---------------------------------------------------------------


# Print method ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param x Object of interest.
#' @param ... Arguments to other methods.
#' @rdname get_pkgs_installation_status
#' @export
print.pkgs_installation_status <- function(x, show_status = x$show_status, ...) {

  list_name <- usethis::ui_value(x$list_name)
  st <- x$status
  n     <- nrow(st)
  n_old <- x$n_to_install_or_update
  if (n_old == 0) {

    msg <-
      if (n == 1) {
        pkg <- crayon::green(st$package)
        "The required version of package {pkg} (from list {list_name}) is installed."

      } else {
        paste0(
          "The required versions of all {crayon::green(n)} packages ",
          "(from list {list_name}) are already installed."
        )
      }

    usethis::ui_done(msg)

  } else {
    # For singular or plural in English language.
    if (n_old == 1) {
      s <- ""
    } else {
      s <- "s"
    }
    msg <-
      if (n == 1) {
        pkg <- crayon::red(st$package)
        paste0(
          "Package {pkg} (from list {list_name}) should be ",
          "{crayon::red('installed')} or {crayon::red('updated')}."
        )

      } else {
        paste0(
          "List {list_name} contains {crayon::red(n_old)} package{s} (out of {n}) ",
          "that should be {crayon::red('installed')} or {crayon::red('updated')}."
        )
      }
    usethis::ui_todo(msg)
  }


  # Show status
  pkg_to_show <-
    switch(as.character(show_status),
      "TRUE"     = ,
      "always"   =  rep(TRUE, nrow(st)),
      "outdated" =  st$update_is_required,
      "missing"  = !st$is_installed,
      "never"    = ,
      "FALSE"    = rep(TRUE, nrow(st)),
      stop("Unknown value of `show_status`: ", show_status)
    )


  if (any(pkg_to_show)) {
    st <-
      st[pkg_to_show , c("package", "is_installed", "current_version",
        "required_version", "update_is_required")] # , "cran_version", "newer_on_cran"
    st$current_version  <- ifelse(is.na(st$current_version), "-", st$current_version)
    st$required_version <- ifelse(st$required_version == "", "-", st$required_version )
    rownames(st) <- NULL
    print(tibble::as_tibble(st), n = Inf, width = Inf, n_extra = Inf)
  }
}


process_pkgs_to_install <- function(x, cran = x$install_from$cran,
  github = x$install_from$github, elsewhere = x$install_from$elsewhere
) {

  st <- x$status

  # From CRAN code
  from_cran_cond <-
    switch(as.character(cran),
      "TRUE"          = ,
      "always"        = st$on_cran,
      "newer_on_cran" = st$newer_on_cran,
      "outdated"      = st$newer_on_cran &  st$update_is_required,
      "required"      = st$on_cran       &  st$update_is_required,
      "missing"       = st$on_cran       & !st$is_installed,
      "never"         = ,
      "FALSE"         = rep(FALSE, nrow(st)),
      stop("Unknown value of `cran`: ", x$install_from$cran)
    )

  # From GitHub code
  on_github <- purrr::map_lgl(st$install_from == "github", isTRUE)

  from_github_cond <-
    switch(as.character(github),
      "TRUE"     = ,
      "always"   = on_github,
      "outdated" = on_github &  st$update_is_required,
      "missing"  = on_github & !st$is_installed,
      "never"    = ,
      "FALSE"    = rep(FALSE, nrow(st)),
      stop("Unknown value of `github`: ", github)
    )

  # Custom instalation code
  from_code <- purrr::map_lgl(st$install_from == "code", isTRUE)

  from_code_cond <-
    switch(as.character(elsewhere),
      "TRUE"     = ,
      "always"   = from_code,
      "outdated" = from_code &  st$update_is_required,
      "missing"  = from_code & !st$is_installed,
      "never"    = ,
      "FALSE"    = rep(FALSE, nrow(st)),
      stop("Unknown value of `elsewhere`: ", elsewhere)
    )

  c(
    x,
    list(
      install_from_cran      = st[from_cran_cond,   ]$package,
      install_from_github    = st[from_github_cond, ]$details,
      install_from_elsewhere = st[from_code_cond,   ]$details
    )
  )
}

#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code <- function(x, ...) {
  UseMethod("get_pkgs_installation_code")
}

# Installation code
#' @rdname get_pkgs_installation_status
#' @export
get_pkgs_installation_code.pkgs_installation_status <- function(x, ...,
  to_clipboard = FALSE) {

  x <- process_pkgs_to_install(x, ...)

  # Warn if there are packages with no source of installation
  pkgs_miss_code <- x$missing_installation_code

  if (length(pkgs_miss_code) > 0) {
    usethis::ui_warn(paste0(
      "Installation code is missing for packages: \n",
      paste0("{crayon::yellow('", pkgs_miss_code, "')}", collapse = ", "),
      "The packages might be recently removed from CRAN."
    ))
  }

  # Print installation code, if present
  res <-
    c(
      get_pkgs_installation_code_cran(x),
      get_pkgs_installation_code_github(x),
      get_pkgs_installation_code_other(x)
    )
  res <- res[!res %in% ""]

  if (length(res) == 0) {
    usethis::ui_info("No installation code was generated.")
    return(invisible(res))
  }

  res <- styler::style_text(res)

  if (isTRUE(to_clipboard)) {
    clipr::write_clip(res, object_type = "character")

    cat("\n")
    usethis::ui_done("Installation code was copied to the clipboard.")
    usethis::ui_info("Use Ctrl+V (on Mac: Cmd+V) to paste it.")
    usethis::ui_todo(
      "Before installation, close RStudio project and/or restart R session."
    )
    return(invisible(res))

  } else {
    return(res)

  }
}

#  @rdname get_pkgs_installation_status
#  @export
get_pkgs_installation_code_cran <- function(x) {
  # Install from CRAN only if the version of package is newer on CRAN
  pkgs_vec <- x$install_from_cran
  if (length(pkgs_vec) == 0) {
    return("")
  }
  pkgs <- to_str_vector(pkgs_vec, collapse = ",\n")
  res <- paste0("install.packages(", pkgs , ")")
  styler::style_text(res)
}

#  @rdname get_pkgs_installation_status
#  @export
get_pkgs_installation_code_github <- function(x) {
  pkgs_vec <- x$install_from_github

  if (length(pkgs_vec) == 0) {
    return("")
  }

  pkgs <- to_str_vector(pkgs_vec, collapse = ",\n")

  if (length(pkgs_vec) > 1) {
    pkgs <- paste0("c(\n", pkgs ,")")
  }

  res <- paste0(
    "remotes::install_github(\n", pkgs, ",\n",
    "dependencies = TRUE, upgrade = FALSE)"
  )
  styler::style_text(res)
}

#  @rdname get_pkgs_installation_status
#  @export
get_pkgs_installation_code_other <- function(x) {
  codes_vec <- x$install_from_elsewhere
  if (length(codes_vec) == 0) {
    return("")
  }

  styler::style_text(codes_vec)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# [!] Check installed packages ===============================================
# # @rdname get_pkgs_installation_status
#  @export
#' Check installed packages
#'
#' A user-fiendly version of a function to check if required R packages are
#' installed and have minimum required versions.
#'
#' @inheritParams get_pkgs_installation_status
#' @param ... Further arguments to [get_pkgs_installation_status()].
#'
#' @return Function invisibly returns object with package installation status.
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' check_installed_packages("mini", use_local_list = TRUE)
#'
#' check_installed_packages("mini", include = "always", use_local_list = TRUE)
#' check_installed_packages("mini", include = "always", install = "outdated",
#'  github = "always", use_local_list = TRUE)
#' }}
check_installed_packages <- function(list_name,
  use_local_list = getOption("bio.use_local_list", FALSE), ...) {
  status <-
    get_pkgs_installation_status(list_name, use_local_list = use_local_list,
      ...)

  status_name <- make_unique_obj_names("status")
  code_name   <- stringr::str_glue(
    "bio::get_pkgs_installation_code({status_name}, to_clipboard = TRUE)")

  print(status)

  if (status$n_to_install_or_update > 0) {
    assign(status_name, status, envir = .GlobalEnv)
    cat("\n")
    usethis::ui_info("To print these results again, type {usethis::ui_field(status_name)} ")
    usethis::ui_todo("To get package installation code, type:\n{usethis::ui_field(code_name)} ")
    rstudioapi::sendToConsole(code_name, execute = FALSE)
  }

  invisible(status)
}
# Optimize order of packages to install ======================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Optimize order of packages to install.
#'
#' Helper function that takes a vector of package names and suggests how to
#' sort the packages in order not to repeat installation of the same
#' package if it can be installed as a dependence of some other package.
#'
#'
#' @param pkgs_vec (character) Vector of package names.
#' @param recursive_dependencies (logical) If `TRUE`, recursive dependencies are
#'        also checked.
#'        **NOTE:** `recursive_dependencies = TRUE`requires internet connection.
#'
#' @return
#' A list with suggestion, which packages should be moved.
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' suggest_optimized_order_of_packages(c("stringr", "stringi", "glue", "readr"))
#' }}
suggest_optimized_order_of_packages <- function(pkgs_vec,
  recursive_dependencies = FALSE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # library(tidyverse)

  list_after <- function(.which, list) {
    rev(rev(list)[1:which(rev(list) == .which)])
  }

  list_before <- function(.which, list) {
    list[1:which(list == .which)]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  deps <-
    tools::package_dependencies(
      pkgs_vec,
      which   = c("Depends", "Imports"),
      reverse = FALSE,
      recursive = recursive_dependencies
    )

  rev_deps <-
    tools::package_dependencies(
      pkgs_vec,
      which   = c("Depends", "Imports"),
      reverse = TRUE,
      recursive = recursive_dependencies
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  move_after <-
    purrr::imap(deps, ~ stringr::str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move after): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")


  move_before <-
    purrr::imap(rev_deps, ~ stringr::str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move before): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")

  list(move_before = move_before, move_after = move_after)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIXME: this function does not work yet!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' optimize_order_to_install
#'
#' Helpeer function that suggest how to optimize order of packages in the
#' vector of packages in order not to repeat installation of
#' the same packages.
#'
#' recursive_dependencies = TRUE requires internet connection.
#'
#' @param pkgs_vec ???
#' @param recursive_dependencies (logical) ???
#'
#' @return ???
#'
#' @noRd
#'
#' @examples
#'
#' \dontrun{\donttest{
#'
#' }}
optimize_order_to_install <- function(pkgs_vec,
  recursive_dependencies = TRUE) {

  stop("This function is not implemented yet!!!")

  pkgs_vec_orig <- pkgs_vec
  # pkgs_vec <- pkgs_vec_orig
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # library(tidyverse)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list_after <- function(.which, list) {
    rev(rev(list)[0:(which(rev(list) == .which) - 1)])
  }
  # list_before <- function(.which, list) {
  #     list[(0:(which(list == .which) - 1))[-1]]
  # }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  str_move_before <- function(x, .what, .move_before) {
    checkmate::assert_string(.what)
    checkmate::assert_string(.move_before)
    checkmate::assert_choice(.what, x)
    checkmate::assert_choice(.move_before, x)

    i <- seq_along(x)

    i_from <- i[x == .what]         # larger number
    i_to   <- i[x == .move_before]  # smaller number

    # If no need to re-arrenge
    if (i_from < i_to) {
      return(x)
    }

    i_new <- i[i > i_to & i <= i_from]
    i[i_new] <- i_new - 1
    i[i_to]  <- i_from

    x[i]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  str_move_after <- function(x, .what, .move_after) {
    checkmate::assert_string(.what)
    checkmate::assert_string(.move_after)
    checkmate::assert_choice(.what, x)
    checkmate::assert_choice(.move_after, x)

    i <- seq_along(x)

    i_from <- i[x == .what]        # smaller number
    i_to   <- i[x == .move_after]  # larger number

    # If no need to re-arrange
    if (i_to < i_from) {
      return(x)
    }

    i_new <- i[i >= i_from & i < i_to]
    i[i_new] <- i_new + 1
    i[i_to]  <- i_from

    x[i]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # deps <-
  #     tools::package_dependencies(
  #         pkgs_vec,
  #         reverse = FALSE,
  #         which = "all",
  #         recursive = recursive_dependencies
  #     )

  # pkgs_vec <- x

  base_pkgs <- c("R", rownames(installed.packages(priority = "base")))

  get_deps <- function(x) {
    # NOTE: all packages must be installed
    ind <- is_pkg_installed(x)

    if (any(!ind)) {
      warning("These package are not installed and were removed from the list:\n",
        paste(x[!ind], sep = ", "))
    }

    x <- x[ind]

    x %>%
      purrr::map(
        ~ system.file(package = ., "DESCRIPTION") %>%
          desc::desc_get_deps() %>%
          dplyr::pull(package) %>%
          setdiff(base_pkgs)
      ) %>%
      purrr::set_names(x) %>%
      # imap(~ str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>%
      purrr::imap(~ stringr::str_c(.x[.x %in% x])) %>%
      purrr::imap_dfr(~tibble::tibble(
        pkg = .y,
        deps = list(.x),
        n_deps = length(.x)
      )) %>%
      dplyr::arrange(n_deps != 0) %>%
      dplyr::select(-n_deps)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  deps <- get_deps(x = pkgs_vec)

  # TODO: Does not work from here

  # x <- deps
  get_n_deps_below <- function(x) {
    lst <- x$pkg
    xx <-
      x %>%
      dplyr::mutate(
        deps_below = purrr::map2(deps, pkg,
          ~ stringr::str_c(.x[.x %in% list_after(.y, list = lst)])
        ),
        n_deps = purrr::map_dbl(deps_below, length),
        last_dep = purrr::map_dbl(deps_below, ~max(which(lst %in% .)))
      ) %>%
      dplyr::arrange(n_deps != 0)

    lst2 <- xx$pkg
    xxx <-
      xx %>%
      dplyr::mutate()
  }



  if (nrow(deps) > 0) {
    x <- pkgs_vec

    # tbl <- tibble()

    for (i in 1:nrow(deps)) {
      # x_old <- x
      x <- str_move_before(x, .what = deps$what[i], .move_before = deps$move_before[i])
      # tbl <- bind_rows(
      # tbl,
      # tibble(i = i,
      #        identical = identical(x_old, x),
      #        what = obj$what[i],
      #        move_before = obj$move_before[i],
      #        x_old = list(x_old))
      # )
    }
    return(x)

  } else {
    return(pkgs_vec)
  }

  # Test output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # all(x %in% pkgs_vec)
  # all(pkgs_vec %in% x)
  # all.equal(pkgs_vec, x)
  # tibble(before = sort(pkgs_vec), after = sort(x), match = before == after) %>%
  #     print(n = Inf)
  # tibble(before = pkgs_vec, after = x, match = before == after) %>%
  #     print(n = Inf)
  #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # move_after <-
  #     imap(deps, ~ str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>%
  #     discard(~length(.) == 0) %>%
  #     imap_dfr(~tibble(what = .y, move_after = str_c(.x, collapse = ","))) %>%
  #     separate_rows(move_after)

  # move_before <-
  #     imap(rev_deps, ~ str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])) %>%
  #     discard(~length(.) == 0) %>%
  #     imap_dfr(~tibble(what = .y, move_before = str_c(.x, collapse = ","))) %>%
  #     separate_rows(move_before)

  # list(move_before = move_before, move_after = move_after)
}
