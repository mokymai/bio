
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_r_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (force || pingr::is_online()) {
    c(
      "https://cran.r-project.org/src/base/R-3",
      "https://cran.r-project.org/src/base/R-4"
    ) %>%
      purrr::map(readr::read_lines) %>%
      purrr::reduce(c) %>%
      stringr::str_extract("(?<=R-).\\d*[.].\\d*[.]\\d*(?=.tar.gz)") %>%
      tidyr::replace_na(0) %>%
      numeric_version() %>%
      max()

  } else {
    ui_warn(
      "To get the newest availableR version, network connection is required. You are offline. "
    )
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

    if (r_installed <= r_available) {
      av_color <- yellow

    } else {
      av_color <- green
    }

    if_available <- stringr::str_glue(", available {av_color(r_available)}")

  } else {
    if_available <- ""
  }

  if (r_installed < v_recommended) {
    print_fun <- ui_todo
    v_color   <- red
    r_color   <- green

  } else {
    print_fun <- ui_done
    v_color   <- green
    r_color   <- yellow
  }

  print_fun(paste0(
    "Program {blue(program)} ({v_color(r_installed)}) is installed ",
    "(recommended {r_color(v_recommended)}{if_available})"
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_r_version <- function(v_recommended = "3.6.2", skip_online_check = FALSE) {

  check_program_version(
    program = 'R',
    r_installed = getRversion(),
    r_available = get_available_r_version(skip = skip_online_check),
    v_recommended = v_recommended
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_rs_version <- function(v_recommended = "1.2.5033", skip_online_check = FALSE) {

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
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_program_installed <- function(program = "", condition = NULL) {

  if (condition) {
    ui_done("Program {blue(program)} is installed.")

  } else {
    ui_oops("Program {red(program)} is not found or not configured correctly.")
  }

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check the versions of required programs.
#'
#' @param skip_online_check (logical) If `TRUE`, the numbers of newest available
#'       stable programs are downloaded, when internet connection is connected.
#' @param which (character) Which programs should be checked? Options:
#'        `all` (default), `BS-2020`, `mini`.
#'
#' @return
#' The results of version checking is printed.
#'
#' @export
#'
#' @examples
#' check_installed_programs(skip_online_check = TRUE)
#'
#' check_installed_programs("mini", skip_online_check = TRUE)
#'
#' check_installed_programs("BS-2020", skip_online_check = TRUE)
#'
check_installed_programs <- function(which = "all", skip_online_check = FALSE) {
  if (!skip_online_check) {
    skip_online_check <- check_internet_connection()
  }

  check_r_version(skip  = skip_online_check)
  check_rs_version(skip = skip_online_check)

  switch(tolower(which),
    "mini" = {

    },

    "all" = {
      check_program_installed("Atom", is_atom_installed())
      check_program_installed("Git",  is_git_installed())
      check_program_installed("Meld", is_meld_installed())
    },

    "bs-2020" = {
      check_program_installed("Atom", is_atom_installed())
    },
    ui_warn("Unknown value '{which}'")

  )

  invisible()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

