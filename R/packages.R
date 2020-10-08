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

# Returns more meaningful error message, if GitHub does not allow to connect.
# Return a string of text or error message

# download_from_github_with_curl <- function(file) {
#
#   res <- curl::curl_fetch_memory(file)
#
#   if (res$status_code >= 300) {
#     stop(remotes:::github_error(res))
#
#   } else {
#     rawToChar(res$content)
#   }
# }

# to_str_vector(LETTERS)
to_str_vector <- function(str, quotes = '"', collapse = ", ") {
  paste0(quotes, str, quotes, collapse = collapse) %>%
    structure(., class = "glue")
}

base_r_packages <- function() {
  rownames(installed.packages(priority = "base"))
}

#' Compare version numbers
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
  # colnames(pkgs_existing) <- c("paketas", "turima_versija")
  colnames(pkgs_existing) <- c("package", "current_version")
  df <- as.data.frame(pkgs_existing, stringsAsFactors = FALSE)

  if (isTRUE(rm_duplicates)) {
    df %>%
      dplyr::group_by(package) %>%
      dplyr::group_modify(
        ~ dplyr::filter(., current_version == max(current_version))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      as.data.frame(stringsAsFactors = FALSE)

  } else {
    df

  }
}


#' List of packages of interest
#'
#' @inheritParams get_pkgs_installation_status
#' @param show_message (logical)
#'        If `TRUE`, a message with chosen list is printed.
#'
#' @return Data frame with column `"package"`.
#' @export
#' @family R-packages-related functions
#'
#' @concept packages
#'
#' @examples
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' # Here it is used for testing purposes only.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_recommended("mini"))
#'
get_pkgs_recommended <- function(list_name,
                                 use_local_list = getOption("bio.use_local_list", FALSE),
                                 show_message = FALSE) {

  checkmate::assert_flag(show_message)
  list_name <- tolower(list_name)
  list_name_blue <- usethis::ui_value(list_name)
  file <- get_path_pkgs_recommended(list_name, use_local_list)

  tryCatch(
    {
      ln <- readLines(file, encoding = "UTF-8")
    },
    # error = function(e) {
    #   if (!pingr::is_online()) {
    #     usethis::ui_stop(paste0(
    #       "No internet connection. The online version of list ",
    #       "{list_name_blue} cannot be accessed. "))
    #   } else {
    #     usethis::ui_stop(paste0(
    #       "It seems that list {list_name_blue} is not present ",
    #       "online or cannot be accessed. Check if the spelling is correct."))
    #   }
    #   return()
    # },
    warning = function(w) {
      if (!pingr::is_online()) {
        usethis::ui_stop(paste0(
          "No internet connection, thus the online version of list ",
          "{list_name_blue} cannot be accessed. "
        ))
      } else if (stringr::str_detect(w$message, "'404 Not Found'")) {
        usethis::ui_stop(paste0(
          "It seems that there is no online version of list ",
          "{list_name_blue} or it cannot be accessed. ",
          "\nCheck if the list name is correct. ",
          "Did you mean one of: \n{usethis::ui_value(bio::get_pkg_lists_local())}, ..."
        ))
      } else {
        usethis::ui_stop(w$message)
      }
    }
  )

  if (show_message) {
    usethis::ui_info("Reading list {list_name_blue}. ")
  }

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
        "List {list_name_blue} was not found on your computer. \n",
        "Check if the list name is correct. ",
        "Did you mean one of: \n{usethis::ui_value(bio::get_pkg_lists_local())}, ..."
      ))
    }

  } else {
    file <- url_bio(base_name)
  }

  file
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get required version of packages
#'
#' Get required version of packages from a list in a file.
#'
#' @inheritParams get_pkgs_installation_status
#'
#' @return Dataframe with columns  "package" and "required_version".
#' @export
#' @family R-packages-related functions
#'
#' @concept packages
#'
#' @examples
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_req_version())
#'
get_pkgs_req_version <- function(
  use_local_list = getOption("bio.use_local_list", FALSE)) {

  file <- get_path_pkgs_req_version(use_local_list)
  # text <- download_from_github_with_curl(file)
  tbl <- read.table(file, skip = 10, header = TRUE, sep = "|",
    na.strings = c("NA", "-"), strip.white = TRUE, stringsAsFactors = FALSE)

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
#' Get details about package on CRAN
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
#' @concept packages
#'
#' @examples
#' \dontrun{\donttest{
#'
#' # NOTE: Internet connection is needed.
#' head(get_pkgs_cran_details())
#'
#' }}
get_pkgs_cran_details <- function() {
  repos <- unique(c("https://mokymai.github.io/download/" , getOption("repos")))

  cran_all <-
    data.frame(
      available.packages(repos = repos)[ , c("Package", "Version")],
      on_cran = TRUE,
      stringsAsFactors = FALSE
    )
  rownames(cran_all) <- NULL
  colnames(cran_all) <- c("package", "cran_version", "on_cran")
  cran_all
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get details about non-CRAN package installation
#'
#' Get installation code of packages that either should be installed not from
#' CRAN or a modified code should be installed.
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
#' @concept packages
#'
#' @examples
#' # NOTE: It is not recommended to use the local lists as they might be out of date.
#' options(bio.use_local_list = TRUE)
#'
#' head(get_pkgs_non_cran_installation_details())

get_pkgs_non_cran_installation_details <- function(
  use_local_list = getOption("bio.use_local_list", FALSE)) {

  file <- get_path_pkgs_non_cran_installation_details(use_local_list)
  # text <- download_from_github_with_curl(file)
  tbl <- read.table(file, skip = 10, header = TRUE, sep = "|",
    strip.white = TRUE, na.strings = c("NA", "-"), stringsAsFactors = FALSE)

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
#' @concept packages
#'
#' @examples
#' head(get_pkgs_installation_status_local("mini"))

get_pkgs_installation_status_local <- function(list_name,
  use_local_list = getOption("bio.use_local_list", TRUE)) {

  pkgs_rec   <- get_pkgs_recommended(use_local_list = use_local_list,
    list_name = list_name, show_message = TRUE)
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
#
#' @name get_pkgs_installation_status
#'
#' @title Get package installation status and code
#' @description
#' Get package installation status (e.g., if packages of interest are installed
#' or need to be updated) and package installation code.
#'
#' @param list_name (character) The name of the list with required R packages.
#'        E.g., "mini", "Rcmdr", "Rcmdr-biostat", "bio", etc.
#'
#' @param include (character) Which packages from the list (indicated in `list_name`)
#'        must be included in the results.
#'        One of:
#'
#' - `"always"` or `TRUE`: all packages;
#' - `"newer_on_cran"` -- only the packages that are `"outdated"` or have
#'   newer version on CRAN. For arguments `github` and `elsewhere`,
#'    value `"newer_on_cran"` is replaced with `"outdated"`.
#' - `"outdated"` (default): only the packages that are not installed
#'    or do not have a minimum required version installed.
#' - `"missing"`: only the packages that are not installed.
#' - `"never"` or `FALSE`: none.
#'
#' @param show_status (character) Which packages should be included in the
#'        package installation status summary.
#'        See options of `include`.
#'        Defaults to the value of `include`.
#'
#' @param install (character) Which packages should be included in the
#'        package installation code.
#'        See options of `include`.
#'        Defaults to the value of `include`.
#'        Sets the default value for `cran`, `github`, and `elsewhere`.
#'
#' @param cran (character) Condition to filter packages that should be
#'        included in code that installs packages from CRAN.
#'        See options of `include` plus value `"required"`.
#'        Defaults to the value of `install`.
#'
#' - `"required"` -- packages that do not have a minimum required version
#'   installed even if the required version is not on CRAN.
#'
#' @param github (character) Condition to filter packages that should be
#'        included in code that installs packages from GitHub.
#'        See options of `include` plus value `"required"`.
#'        Defaults to the value of `install`.
#'
#' @param elsewhere (character) Condition to filter packages that should
#'        be included in code that installs packages from other sources.
#'        See options of `include` plus value `"required"`.
#'        Defaults to the value of `install`.
#'
#' @param use_local_list (logical) If `TRUE`, the list, which is locally
#'        installed in the folder of package \pkg{bio} ("local list"), is used.
#'         If `FALSE`, the list on "GitHub" repository of the package is used.
#'         It is recommended  using the online version of the list, as it may
#'         contain more recent changes.
#'         Optiom bight be set globally by, e.g.,
#'          `options(bio.use_local_list = TRUE)`.
#'
#' @export
#' @family R-packages-related functions
#'
#' @concept packages
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
#' # Package "remembers" the last created 'pkgs_installation_status' object
#' get_pkgs_installation_status("snippets")
#' get_last_pkgs_installation_status()
#' get_pkgs_installation_code()
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
get_pkgs_installation_status <- function(list_name = NULL, include = "outdated",
  show_status = include, install = include, cran = install,
  github = install, elsewhere = install,
  use_local_list = getOption("bio.use_local_list", FALSE)) {

  if (is.null(list_name)) {
    ui_stop(paste0(
      "The {green('list_name')} is missing. ",
      "Currently available lists include: ",
      "{usethis::ui_value(bio::get_pkg_lists_local())}, ..."
    ))
  }

  choices <-
    c(TRUE, "always", "newer_on_cran", "outdated", "missing", "never", FALSE)

  include     <- match.arg(as.character(include),     choices)
  show_status <- match.arg(as.character(show_status), choices)
  install     <- match.arg(as.character(install),     choices)
  cran        <- match.arg(as.character(cran),      c(choices, "required"))
  github      <- match.arg(as.character(github),      choices)
  elsewhere   <- match.arg(as.character(elsewhere),   choices)

  github      <- ifelse(github    == "newer_on_cran", "outdated", github)
  elsewhere   <- ifelse(elsewhere == "newer_on_cran", "outdated", elsewhere)

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
    n_to_install_or_update    = sum(status$update_is_required),
    n_newer_on_cran           = sum(status$newer_on_cran)

    # missing_installation_code = missing_installation_code,
    # pkgs_to_install_or_update = pkgs_to_install_or_update,
    #
    # install_from_cran         = install_from_cran,
    # install_from_github       = install_from_github,
    # install_from_elsewhere    = install_from_elsewhere,
    # install_from_unknown      = install_from_unknown
  )

  out <- structure(
    out,
    class = c("pkgs_installation_status", "list")
  )

  bio_envir$last_installation_status <- out
  out
}

# =~~~ methods ---------------------------------------------------------------


#' @rdname get_pkgs_installation_status
#' @export
get_pkg_lists_local <- function() {
  path_bio() %>%
    fs::dir_ls(regexp = "pkgs-recommended") %>%
    stringr::str_extract("(?<=pkgs-recommended--).*?(?=.txt$)")
}

#' @rdname get_pkgs_installation_status
#' @export
get_last_pkgs_installation_status <- function() {
  bio_envir$last_installation_status
}


# Print method ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_pkgs_installation_status
#' @param x Object of interest.
#' @param ... Arguments to other methods.
#' @export
print.pkgs_installation_status <- function(x, show_status = x$show_status, ...) {

  list_name <- ui_value(x$list_name)
  st <- x$status
  n     <- nrow(st)
  n_old <- x$n_to_install_or_update
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Show status
  pkg_to_show <-
    switch(as.character(show_status),
      "TRUE"          = ,
      "always"        =  rep(TRUE, nrow(st)),
      "newer_on_cran" =  st$update_is_required | st$newer_on_cran,
      "outdated"      =  st$update_is_required,
      "missing"       = !st$is_installed,
      "never"         = ,
      "FALSE"         = rep(FALSE, nrow(st)),
      stop("Unknown value of `show_status`: ", show_status)
    )


  if (any(pkg_to_show)) {
    st2 <-
      st[pkg_to_show , c("package", "is_installed", "current_version",
        "required_version", "cran_version", "update_is_required")] # , "cran_version", "newer_on_cran"
    st2$current_version  <- ifelse(is.na(st2$current_version), "-", st2$current_version)
    st2$required_version <- ifelse(st2$required_version == "", "-", st2$required_version )
    rownames(st2) <- NULL
    colnames(st2) <- c("package", "is_installed", "v_current", "v_required",
      "v_cran", "update_is_required")
    cat("\n")
    ui_info("{silver('Abbreviations:')} {yellow('v \u2014 version')}\n\n")
    print(tibble::as_tibble(st2), n = Inf, width = Inf, n_extra = Inf)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("\n")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (n_old == 0) {

    msg <-
      if (n == 1) {
        pkg <- green(st$package)
        "Minimal required version of package {pkg} (from list {list_name}) is installed."

      } else {
        paste0(
          "Minimal required versions of all {green(n)} packages ",
          "(from list {list_name}) are already installed."
        )
      }

    ui_done(msg)

  } else {
    # For singular or plural in English language.
    if (n_old == 1) {
      s <- ""
    } else {
      s <- "s"
    }
    msg <-
      if (n == 1) {
        pkg <- red(st$package)
        paste0(
          "Package {pkg} (from list {list_name}) should be ",
          "{red('installed')} or {red('updated')}."
        )

      } else {
        paste0(
          "List {list_name} contains {red(n_old)} package{s} (out of {n}) ",
          "that should be {red('installed')} or {red('updated')}."
        )
      }
    ui_oops(msg)
  }

  if (x$n_newer_on_cran > 0) {
    n_cran <- yellow(x$n_newer_on_cran)
    if (x$n_newer_on_cran == 1) {
      ui_info(paste0(
        "Note: {n_cran} package has newer version on CRAN. ",
        # "You may update it, if needed."
        ""
        )
      )

    } else {
      ui_info(paste0(
        "Note: {n_cran} packages have newer versions on CRAN. ",
        # "You may update them, if needed."
        ""
        )
      )
    }
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


# Installation code
#' @rdname get_pkgs_installation_status
#' @inheritParams update_pkg_bio
#' @export
#' @param to_clipboard (logical) If `TRUE`, the code is copied to clipboard and
#'        returned only invisibly.
get_pkgs_installation_code <- function(x = NULL, ..., to_clipboard = FALSE,
  upgrade = TRUE) {

  if (is.null(x)) {
    x <- get_last_pkgs_installation_status()
  }

  if (is.null(x)) {
    ui_stop(paste0(
      "Incorrect value of {ui_value('x')} in ",
      "{ui_code('get_pkgs_installation_code()')}.\n",
      "You should should do one of the following: \n",
      " - run either {ui_code('get_pkgs_installation_status()')} or ",
      "{ui_code('check_packages_by_topic()')} before this function; \n",
      " - provide an object of class {ui_field('pkgs_installation_status')}."
    ))
  }
  checkmate::assert_class(x, "pkgs_installation_status")
  checkmate::assert_flag(to_clipboard)

  x <- process_pkgs_to_install(x, ...)

  # Warn if there are packages with no source of installation
  pkgs_miss_code <- x$missing_installation_code

  if (length(pkgs_miss_code) > 0) {

    r_installed <- getRversion()
    r_available <- get_available_r_version()

    if (length(pkgs_miss_code) > 1) {
      # Plural
      s    <- "s"
      ss   <- ""
      each <- "each"

    } else {
      # Singular
      s    <- ""
      ss   <- "s"
      each <- "the"
    }

    status_msg <-
      if (r_installed < r_available) {
        glue::glue(
          "Either the package{s} require{ss} a newer version of R ",
          "(installed {yellow(r_installed)}, ",
          "available {green(r_available)}) ",
          "or the package{s} might be recently removed from CRAN. "
        )

      } else {
        glue::glue(
          "The package{s} might be recently removed from CRAN. "
        )
      }

    usethis::ui_warn(paste0(
      "Installation code is missing for package{s}: \n",
      paste0("{yellow('", pkgs_miss_code, "')}", collapse = ", "), ". \n",
      "{status_msg}",
      "\n",
      "Check the status of {each} package at \n",
      "{yellow('https://CRAN.R-project.org/package=')}",
      "{blue('[package\\'s name]')} "
    ))
  }

  # Print installation code, if present
  res <-
    c(
      get_pkgs_installation_code_cran(x),
      get_pkgs_installation_code_github(x, upgrade = upgrade),
      get_pkgs_installation_code_other(x)
    )

  res <- res[!res %in% ""]

  if (length(res) == 0) {
    usethis::ui_info("No installation code was generated.")
    return(invisible(res))
  }

  # if (Sys.getenv("GITHUB_PAT") == "") {
  #   # usethis::browse_github_pat()
  #   # Sys.setenv(GITHUB_PAT = "write your PAT here, if you have it")
  # }

  res <- c(glue::glue(
    '
    # To read more on the used options, run code:
    # help("options") # Opens help on options
    options(
      repos = "https://cran.rstudio.com/",
      pkgType = "{ifelse(get_os_type() == "linux", "source", "both")}",
      install.packages.check.source = "yes",
      install.packages.compile.from.source = "always"
    )

    # For installation from GitHub
    # Read more at: https://remotes.r-lib.org/#environment-variables
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

    '),
    res
  )
  # , Ncpus = max(1, parallel::detectCores() - 1)
  res <- styler::style_text(res)

  if (isTRUE(to_clipboard)) {
    clipr::write_clip(res, object_type = "character")

    cat("\n")
    usethis::ui_done("Installation code was copied to the clipboard.")

    if (get_os_type() == "mac") {
      # Mac
      usethis::ui_info("Use {yellow('Cmd+V')} to paste it.")

    } else {
      # Windows / Linux
      usethis::ui_info("Use {yellow('Ctrl+V')} to paste it.")
    }

    usethis::ui_todo(paste0(
      "But before the installation, {underline('close')} RStudio ",
      "{underline('project')} and/or {underline('restart R session')}.")
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

  if (length(pkgs_vec) > 1) {
    pkgs <- paste0("c(\n", pkgs ,")")
  }

  if (requireNamespace("remotes", quietly = TRUE)) {
    res <- paste0("remotes::install_cran(", pkgs , ")")

  } else {
    res <- paste0("install.packages(", pkgs , ")")
  }

  styler::style_text(res)
}

#  @rdname get_pkgs_installation_status
#  @export
get_pkgs_installation_code_github <- function(x, upgrade = TRUE) {

  upgrade <- chk_arg_upgrade(upgrade)
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
    glue::glue("dependencies = TRUE, upgrade = {upgrade})")
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

#' Check installed packages by topic
#'
#' A user-fiendly version of a function to check if required R packages are
#' installed and have minimum required versions.
#'
#' @inheritParams get_pkgs_installation_status
#' @inheritParams get_pkgs_installation_code
#' @param ... Further arguments to [get_pkgs_installation_status()].
#'
#' @return Function invisibly returns object with package installation status.
#' @export
#'
#' @concept packages
#'
#' @examples
#' \dontrun{\donttest{
#' check_packages_by_topic("mini", use_local_list = TRUE)
#'
#' check_packages_by_topic("mini", include = "always", use_local_list = TRUE)
#' check_packages_by_topic("mini", include = "always", install = "outdated",
#'  github = "always", use_local_list = TRUE)
#' }}

# Sys.getenv("R_REMOTES_UPGRADE")
check_packages_by_topic <- function(list_name = NULL,
  use_local_list = getOption("bio.use_local_list", FALSE), upgrade = TRUE,
  ...) {

  status <-
    get_pkgs_installation_status(list_name, use_local_list = use_local_list, ...)

  upgrade_str <-
    if (isTRUE(upgrade)) {
      ", upgrade = TRUE"
    } else {
      ""
    }

  code_str <- glue::glue(
    "bio::get_pkgs_installation_code(to_clipboard = TRUE{upgrade_str})"
  )

  print(status)

  if (status$n_to_install_or_update > 0) {
    assign("last_installation_status", status, envir = bio_envir)

    cat("\n")
    usethis::ui_todo(paste0(
      "To get package installation code, type:\n{usethis::ui_field(code_str)} ")
      )
    cat("\n")

    if (rstudioapi::isAvailable("0.99.787")) {
      rstudioapi::sendToConsole(code_str, execute = FALSE)
    }
  }

  invisible(status)
}


# Optimize order of packages to install ======================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Optimize order of packages to install
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
#' @concept packages
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
    purrr::imap(
      deps,
      ~ stringr::str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])
    ) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move after): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")


  move_before <-
    purrr::imap(
      rev_deps,
      ~ stringr::str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])
    ) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move before): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")

  pkgs_base <- pkgs_vec[pkgs_vec %in% base_r_packages()]

  list(move_before = move_before, move_after = move_after, base_packages = pkgs_base)

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

# List packages --------------------------------------------------------------
#' List packages used in file(s)
#'
#' These functions search for `library(pkg)`, `require(pkg)` or `pkg::function()`
#' and return package names from these expressions.
#'
#' @param path Path to directory with R and R Markdown files. Defaults to `"."`.
#' @param files Path to R or Rmd files
#' @param regexp Regular expression to filter file names. Defaults to R and Rmd
#'        files.
#' @param ... Further arguments to [fs::dir_ls].
#'
#' @return Character vector with package names.
#' @export
list_pkgs_used_in_dir <- function(path = ".", regexp = "(?i)[.](rmd|r)$", ...) {
  path %>%
  fs::dir_ls(regexp = "(?i)[.](rmd|r)$") %>%
    list_pkgs_used_in_files()
}


#' @rdname list_pkgs_used_in_dir
#' @export
list_pkgs_used_in_files <- function(files) {
  files %>%
  purrr::map(~readr::read_file(.)) %>%
  stringr::str_extract_all(
    "(?<=(library|require)\\()(.*?)(?=\\))|(?<=\\s|\n)[a-zA-Z.]*?(?=::)"
  ) %>%
  purrr::reduce(c) %>%
  unique()
}
