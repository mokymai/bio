
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check the versions of required programs
#'
#' @param skip_online_check (logical) If `TRUE`, the numbers of newest available
#'       stable programs are downloaded, when internet connection is connected.
#' @param which (character) Which programs should be checked? Options:
#'        `main`, `all`, `BS-2020`.
#'
#' @return
#' The results of version checking is printed.
#'
#' @export
#' @concept programs
#'
#' @examples
#'
#' \dontrun{\donttest{
#'
#' check_user_info()
#'
#' check_installed_programs()
#'
#' check_installed_programs("all")
#'
#' check_installed_programs("BS-2020")
#'
#' }}
check_installed_programs <- function(which = "main", skip_online_check = FALSE) {

  # if (user_info) {
  #   check_user_info()
  # }

  if (!skip_online_check) {
    skip_online_check <- check_internet_connection()
  }

  v_req <- get_prgm_req_version(use_local_list = skip_online_check)

  # R
  check_r_version(v_recommended = v_req$R, skip_online_check = skip_online_check)

  # RStudio
  check_rs_version(v_recommended = v_req$RStudio, skip_online_check = skip_online_check)

  # R Build Tools (on Windows, they are called 'Rtools')
  if (get_os_type() == "windows") {
    # TODO (SEE ALSO): rstudioapi::buildToolsCheck()
    check_program_installed("Rtools", pkgbuild::has_build_tools())
    # check_program_installed("Rtools", is_rtools_installed())
  } else {
    check_program_installed("'R Build Tools'", pkgbuild::has_build_tools())
  }


  # xQuartz (on Mac, OS X)
  if (get_os_type() == "mac") {
    # FIXME: on stack overflow it writes, that this functon might hang R session
    # if XQuartz is missing.
    # https://stackoverflow.com/questions/37438773/is-it-possible-to-check-if-a-graphics-device-is-available-without-calling-dev-ne
    check_program_installed("XQuartz", is_xquartz_installed())
  }

  # Additional software
  switch(tolower(which),
    "main" = {
      NULL
    },

    "all" = {
      check_program_installed("Atom", is_atom_installed())
      check_program_installed("Git",  is_git_installed())
      # FIXME: Use better algorithm to check if Meld is installed.
      try({
        check_program_installed("Meld", is_meld_installed())
      }, silent = TRUE)
    },

    "bs-2020" = {
      check_program_installed("Atom", is_atom_installed())
    },

    ui_warn("Unknown value '{which}'")
  )

  invisible()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname check_installed_programs
#' @export
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

# ~~~~~~~~~~~~~~~~~~~~~ ======================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_path_program_req_version <- function(use_local_list) {
  base_name <- "programs-required-version.txt"

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_prgm_req_version <- function(use_local_list = getOption("bio.use_local_list", FALSE)) {

  file <- get_path_program_req_version(use_local_list)

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
      tidyr::replace_na(0) %>%
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
    suppressWarnings({
      "https://rstudio.com/products/rstudio/download/" %>%
        purrr::map(readr::read_lines) %>%
        stringr::str_extract("(?<=RStudio-).*?(?=.exe)")
    }) %>%
      numeric_version()


  } else {
    ui_warn(
      "To get the newest available RStudio version, network connection is required. You are offline. "
    )
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_internet_connection <- function() {
  if (pingr::is_online()) {
    FALSE # Skip

  } else {
    ui_warn(paste0(
      "To get the newest available versions, network connection is required. ",
      "You are offline. "
    ))
    TRUE # Skip
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
    "(recommended {r_color(v_recommended)}{if_available})"
  ))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_r_version <- function(v_recommended = "4.0.2", skip_online_check = FALSE) {

  check_program_version(
    program = 'R',
    r_installed = getRversion(),
    r_available = get_available_r_version(skip = skip_online_check),
    v_recommended = v_recommended
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_rs_version <- function(v_recommended = "1.3.1073", skip_online_check = FALSE) {

  if (!rstudioapi::isAvailable()) {
    ui_oops("Program {red('RStudio')} is not installed or is not running. ")

  } else {
    check_program_version(
      program = 'RStudio',
      r_installed = rstudioapi::versionInfo()$version,
      r_available = get_available_rs_version(skip = skip_online_check),
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_program_installed <- function(program = "", condition = NULL,
                                    what = "Program") {

  if (condition) {
    ui_done("{what} {blue(program)} is installed.")

  } else {
    ui_oops("{what} {red(program)} is ether missing, not detected or not configured incorrectly.")
  }

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_tool_installed <- function(name = "", condition = NULL) {

  if (condition) {
    ui_done("Tool {blue(name)} is installed.")

  } else {
    ui_oops("Tool {red(name)} is ether missing, not detected or not configured incorrectly.")
  }

}

