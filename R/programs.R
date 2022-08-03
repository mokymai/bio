# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get User-Related Information
#'
#' Get user-related information.
#'
#' @concept check
#' @concept check-user-info
#'
#' @export
#' @examples
#' \dontrun{\donttest{
#' check_user_info()
#' }}
#
# @concept utilities
check_user_info <- function() {

  os_info <-
    c(
      "Operating system   " = sessionInfo()$running,
      "Platform "           = sessionInfo()$platform,
      Sys.getenv(c(
        "LOGNAME", "USERNAME", "USERPROFILE", "HOME", "R_USER", "R_HOME", "R_LIBS_USER"))
    ) %>%
    as.data.frame()

  os_info$. = fs::path(os_info$.)
  os_info <- setNames(os_info, c("  "))

  print(os_info, right = FALSE)
  cat("\n")

  invisible(os_info)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check Required Programs
#'
#' Check absence/presence and (in some cases) versions of required programs.
#'
#' @param skip_online_check (logical) If `TRUE`, the numbers of newest available
#'       stable programs are downloaded, when internet connection is connected.
#' @param type (character) Which programs should be checked? Options:
#'        `main`, `all`, `gmc-bs`, `gmc-r`.
#'
#' @return
#' Invisible `NULL`.
#' The results of program checking are printed.
#'
#' @export
#' @concept check
#' @concept check-programs
#'
#' @examples
#'
#' \dontrun{\donttest{
#'
#' check_installed_programs()
#'
#' check_installed_programs("all")
#'
#' }}
check_installed_programs <- function(type = "main", skip_online_check = FALSE) {

  type_lwr <- tolower(type)

  if (!type_lwr %in% c("main", "all", "gmc-bs", "gmc-r")) {
    ui_warn("Unknown value of type = '{type}'")
  }


  if (!skip_online_check) {
    skip_online_check <- check_internet_connection()
  }

  v_req <- get_prgm_req_version(local_list = skip_online_check)

  # R
  check_r_version(v_recommended = v_req$R, skip_online_check = skip_online_check)

  # RStudio
  check_rs_version(v_recommended = v_req$RStudio, skip_online_check = skip_online_check)

  # R Build Tools (on Windows, they are called 'Rtools')
  tool_name <-
    if (get_os_type() == "windows") {
      "Rtools"
    } else {
      "Compiler (R Build Tools)"
    }
  # TODO (SEE ALSO): rstudioapi::buildToolsCheck(); pkgbuild::has_build_tools()
  # FIXME: Might not work properly if RStudio is not running
  check_tool_installed(tool_name, rstudioapi::buildToolsCheck())


  # XQuartz (on Mac)
  if (type_lwr %in% c("all", "gmc-bs")) {
    # xQuartz (on Mac, OS X)
    if (get_os_type() == "mac") {
      # FIXME: on stack overflow, it writes that this functon might hang R session
      # if XQuartz is missing.
      # https://stackoverflow.com/questions/37438773/
      check_program_installed("XQuartz", is_xquartz_installed())
    }
  }

  # Git
  if (type_lwr %in% c("all", "gmc-r")) {
    check_program_installed("Git",  is_git_installed())
  }

  # Meld
  if (type_lwr %in% c("all")) {
    # FIXME: Use better algorithm to check if Meld is installed.
    try({
      check_program_installed("Meld", is_meld_installed())
    }, silent = TRUE)
  }
  invisible(NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~ ======================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_path_program_req_version <- function(local_list) {
  base_name <- "programs-required-version.txt"

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_prgm_req_version <- function(local_list = getOption("bio.local_list", FALSE)) {

  file <- get_path_program_req_version(local_list)

  # text <- download_from_github_with_curl(file)

  tbl <- read.table(file, skip = 10, header = TRUE, sep = "|",
      na.strings = c("NA", "-"), strip.white = TRUE, stringsAsFactors = FALSE)

  tbl <- remove_ignored_rows(tbl)
  as.list(setNames(tbl$required_version, tbl$program))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_r_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (force || pingr::is_online()) {
    c(
      # "https://cran.r-project.org/src/base/R-3",
      "https://cran.r-project.org/src/base/R-4"
    ) %>%
      purrr::map(readr::read_lines) %>%
      purrr::reduce(c) %>%
      stringr::str_extract("(?<=R-).\\d*[.].\\d*[.]\\d*(?=.tar.gz)") %>%
      .[!is.na(.)] %>%
      numeric_version() %>%
      max()

  } else {
    ui_warn(paste(
      "To get the newest availableR version, network connection is required.",
      "You are offline. "
    ))
    NULL
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_rs_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (force || pingr::is_online()) {

    "https://rstudio.com/products/rstudio/download/" %>%
      readr::read_lines() %>%
      stringr::str_extract("(?<=RStudio-).*?(?=.exe)") %>%
      .[!is.na(.)] %>%
      numeric_version() %>%
      max()

  } else {
    ui_warn(paste(
      "To get the newest available RStudio version,",
      "network connection is required. You are offline. "
    ))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_internet_connection <- function() {
  if (pingr::is_online()) {
    FALSE # Online

  } else {
    ui_warn(paste0(
      "To get the newest available versions, network connection is required. ",
      "You are offline. "
    ))
    TRUE # Offline
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_program_version  <- function(program = "", r_installed = "", v_recommended = "",
  r_available = NULL) {

  v_recommended <- numeric_version(v_recommended)
  r_installed   <- numeric_version(r_installed)

  if (!is.null(r_available)) {

    if (r_installed < r_available) {
      av_color <- green

    } else {
      av_color <- yellow
    }

    if_available <- glue::glue(", available {av_color(r_available)}")

  } else {
    if_available <- ""
  }

  if (r_installed < v_recommended) {
    print_fun <- ui_todo
    v_color   <- red
    r_color   <- green
    install_status <- "should be updated"

  } else {
    print_fun <- ui_done
    v_color   <- green
    r_color   <- yellow
    install_status <- "is installed"
  }

  print_fun(paste0(
    "Program {blue(program)} ({v_color(r_installed)}) {install_status} ",
    "(recommended {r_color(v_recommended)}{if_available})."
  ))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_r_version <- function(v_recommended = "4.2.0", skip_online_check = FALSE) {

  check_program_version(
    program = 'R',
    r_installed = getRversion(),
    r_available = get_available_r_version(skip = skip_online_check),
    v_recommended = v_recommended
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_rs_version <- function(v_recommended = "2022.2.3.492", skip_online_check = FALSE) {

  if (!rstudioapi::isAvailable()) {
    ui_oops("Program {red('RStudio')} is not installed or is not running. ")

  } else {
    check_program_version(
      program = 'RStudio',
      r_installed = rstudioapi::versionInfo()$version,
      r_available =
        tryCatch(
          get_available_rs_version(skip = skip_online_check),
          error = function(e) {
            warning(e)
            NULL
          }
      ),
      v_recommended = v_recommended
    )
  }
  try({
    if (is_32bit_os()) {
      ui_info(stringr::str_c(
        "For 32-bit operating systems, the newest available RStudio version ",
        "is {yellow('1.1.463')}."
      ))
    }
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_xquartz_installed  <- function(variables) {
  isTRUE(unname(capabilities("aqua")))
}

# Check if Git is installed
#
# @return logical value.
# @export
# @concept programs
#
# @examples
# is_git_installed()
is_git_installed <- function() {
  # suppressWarnings(
  #   system("git --version", show.output.on.console = FALSE) == 0
  # )
  tryCatch(
    {
      system2("git", "--version", stdout = TRUE, stderr = TRUE)
      # If no error occurs in system2(), TRUE is returned.
      TRUE
    },

    error = function(e) {
      FALSE
    }
  )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# program   - string
# condition - logical
# string    - what
check_program_installed <- function(program = "", condition = NULL,
                                    what = "Program") {

  if (condition) {
    ui_done("{what} {blue(program)} is installed.")

  } else {
    ui_oops("{what} {red(program)} is ether missing, not detected or configured incorrectly.")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_tool_installed <- function(name = "", condition = NULL) {
  check_program_installed(name, condition, what = "Tool")
}

