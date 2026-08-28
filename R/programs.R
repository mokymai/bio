# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get user-related information
#'
#' Prints user and system metadata useful for troubleshooting local R setups.
#'
#' @concept check
#' @concept check-user-info
#' @export
#' @examples
#' if (interactive()) {
#'   check_user_info()
#' }
check_user_info <- function() {
  os_info <- c(
    "Operating system" = sessionInfo()$running,
    "Platform" = sessionInfo()$platform,
    Sys.getenv(c(
      "LOGNAME",
      "USERNAME",
      "USERPROFILE",
      "HOME",
      "R_USER",
      "R_HOME",
      "R_LIBS_USER"
    ))
  )

  os_info <- tibble::enframe(os_info, name = "Setting", value = "Value")
  os_info$Value <- fs::path(os_info$Value)

  print(os_info, right = FALSE)
  cat("\n")

  invisible(os_info)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check installed programs and available versions
#'
#' Checks whether key tools are installed and, when online, reports the newest
#' available versions for R, RStudio, and Quarto.
#'
#' @param type Character scalar selecting the tool group to check. Supported
#'   values are `"main"`, `"all"`, `"dev"`, `"gmc-bs"`, and `"gmc-r"`.
#' @param skip_online_check Logical. If `TRUE`, skips internet checks and does not
#'   attempt to fetch the newest available versions.
#'
#' @return Invisibly returns `NULL`. The results are printed to the console.
#' @export
#' @concept check
#' @concept check-programs
#' @examples
#' if (interactive()) {
#'   check_installed_programs()
#'   check_installed_programs("all")
#' }
check_installed_programs <- function(type = "main", skip_online_check = FALSE) {
  type_lwr <- tolower(type)

  if (!type_lwr %in% c("main", "dev", "all", "gmc-bs", "gmc-r")) {
    ui_warn("Unknown value of type = '{type}'")
    return(invisible(NULL))
  }

  if (!skip_online_check) {
    skip_online_check <- check_internet_connection()
  }

  # R
  check_r_version(skip_online_check = skip_online_check)

  # RStudio
  check_rs_version(skip_online_check = skip_online_check)

  # Quarto
  check_quarto_version(skip_online_check = skip_online_check)

  # R Build Tools (on Windows, they are called 'Rtools')
  tool_name <-  if (get_os_type() == "windows") "Rtools" else "R Build Tools"

  if (type_lwr %in% c("all", "dev")) {
    check_tool_installed(
      tool_name,
      if (rstudioapi::isAvailable()) {
        rstudioapi::buildToolsCheck()
      } else {
        pkgbuild::has_build_tools()
      }
    )
  }

  # XQuartz (on Mac)
  # NOTE: on stack overflow, it writes that this functon might hang R session
  # if XQuartz is missing.
  # https://stackoverflow.com/questions/37438773/
  if (type_lwr %in% c("all", "gmc-bs") && get_os_type() == "mac") {
    check_program_installed("XQuartz", is_xquartz_installed())
  }

  # Git
  if (type_lwr %in% c("all", "gmc-r")) {
    check_program_installed("Git", is_git_installed())
  }

  # Meld
  if (type_lwr %in% c("all")) {
    try(
      {
        check_program_installed("Meld", is_meld_installed())
      },
      silent = TRUE)
  }

  invisible(NULL)
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
    ) |>
      purrr::map(readr::read_lines) |>
      purrr::reduce(c) |>
      stringr::str_extract("(?<=R-).\\d*[.].\\d*[.]\\d*(?=.tar.gz)") |>
      purrr::discard(is.na) |>
      as.numeric_version() |>
      max()
  } else {
    msg_offline(get_what = "R version")
    NULL
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_rs_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (force || pingr::is_online()) {
    "https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads" |>
      readr::read_lines() |>
      stringr::str_extract("(?<=RStudio-)\\d{4}[.].*?(?=.exe)") |>
      purrr::discard(is.na) |>
      as.numeric_version() |>
      max()
  } else {
    msg_offline(get_what = "RStudio version")
    NULL
  }
}

get_available_quarto_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (force || pingr::is_online()) {
    url <- "https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest"
    rel <- jsonlite::fromJSON(url)
    sub("^v", "", rel$tag_name) |>
      as.numeric_version()
  } else {
    msg_offline(get_what = "Quarto version")
    NULL
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
msg_offline <- function(get_what = "versions") {
  cli::cli_warn(c(
    "To get the newest available {get_what}, network connection is required.",
    "x" = "This computer is offline. "
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_internet_connection <- function(get_what = "versions") {
  if (pingr::is_online()) {
    FALSE # Online

  } else {
    msg_offline(get_what = get_what)
    TRUE # Offline
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Validate that a function argument is a single value.
#'
#' This helper keeps internal checks explicit and consistent when a function is
#' intentionally scalar-only.
#'
#' @param x Value to validate.
#' @param arg_name Name of the argument for the error message.
#' @param allow_null Whether `NULL` is allowed.
#' @param allow_na Whether `NA` is allowed.
#'
#' @examples
#' bio:::assert_single_value("R", "name", allow_null = FALSE, allow_na = FALSE)
#' try(bio:::assert_single_value(c("R", "S"), "name", allow_null = FALSE, allow_na = FALSE))
assert_single_value <- function(x, arg_name, allow_null = TRUE, allow_na = TRUE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop(sprintf("`%s` must be a single value.", arg_name), call. = FALSE)
  }

  if (length(x) != 1L) {
    stop(sprintf("`%s` must be a single value.", arg_name), call. = FALSE)
  }

  if (length(x) == 1L && is.na(x) && !allow_na) {
    stop(sprintf("`%s` must not be NA.", arg_name), call. = FALSE)
  }

  invisible(TRUE)
}

#' Print a version-status summary for a program.
#'
#' Formats the installed and latest available version for an application or tool
#' and reports whether the program is installed and whether an upgrade is known.
#'
#' @param name Name of the program.
#' @param v_installed Installed version value, or `NULL` when missing.
#' @param v_available Newest available version value, or `NULL` when unavailable.
#' @param type Label used for the output, typically `"Program"` or `"Tool"`.
#'
#' @examples
#' if (interactive()) {
#'   bio:::print_program_version_info("R", getRversion(), "4.5.0")
#'   bio:::print_program_version_info("R", NULL, "4.5.0")
#' }
print_program_version_info <- function(name = "", v_installed = NULL,
  v_available = NULL, type = "Program") {
  assert_single_value(name, "name", allow_null = FALSE, allow_na = FALSE)
  assert_single_value(type, "type", allow_null = FALSE, allow_na = FALSE)
  assert_single_value(v_installed, "v_installed", allow_null = TRUE, allow_na = TRUE)
  assert_single_value(v_available, "v_available", allow_null = TRUE, allow_na = TRUE)

  scalar_na <- function(x) {
    is.null(x) || length(x) == 0L || is.na(x) || !nzchar(as.character(x))
  }

  v_installed <- if (scalar_na(v_installed)) NA_character_ else as.character(v_installed)
  v_available <- if (scalar_na(v_available)) NA_character_ else as.character(v_available)

  not_installed <- is.na(v_installed)
  not_available <- is.na(v_available)

  if (not_installed) {
    status <- "is not found"
    ui_fun <- ui_oops
    n_color <- red
    version <- ""
    v_installed_num <- NULL
  } else {
    status <- "is installed"
    ui_fun <- ui_done
    n_color <- blue
    v_installed_num <- as.numeric_version(v_installed)
    v_color <- if (not_available) {
      green
    } else {
      v_available_num <- as.numeric_version(v_available)
      if (v_installed_num < v_available_num) {
        yellow
      } else {
        green
      }
    }
    version <- glue::glue("({v_color(v_installed_num)}) ")
  }

  available <-
    if (not_available) {
      ""
    } else {
      available_version <- as.numeric_version(v_available)
      if (not_installed) {
        available_color <- green
      } else {
        available_color <- if (v_installed_num <= available_version) {
          cli::col_grey
        } else {
          green
        }
      }
      glue::glue(" ({available_color(available_version)} is available online)")
    }
  ui_fun("{type} {n_color(name)} {version}{status}{available}.")

  invisible(NULL)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_r_version <- function(skip_online_check = FALSE) {

  print_program_version_info(
    name = "R",
    v_installed = getRversion(),
    v_available = get_available_r_version(skip = skip_online_check)
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_quarto_version <- function(skip_online_check = FALSE) {
  print_program_version_info(
    name = "Quarto",
    v_installed = quarto::quarto_version(),
    v_available = get_available_quarto_version(skip = skip_online_check),
    type = "Tool"
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# `rstudioapi::isAvailable()` only detects a *running* RStudio session, so it
# is always FALSE when R is invoked from a terminal (e.g. Git Bash). Fall back
# to detecting an installed-but-not-running copy via known install locations.
check_rs_version <- function(skip_online_check = FALSE) {
  is_running <- rstudioapi::isAvailable()

  v_installed <- if (is_running) {
    rstudioapi::versionInfo()$version
  } else {
    get_installed_rstudio_version()
  }

  if (is.null(v_installed) && !is_running && !is_rstudio_installed()) {
    ui_oops("Program {red('RStudio')} is not installed or is not running. ")

  } else {
    print_program_version_info(
      name = "RStudio",
      v_installed = v_installed,
      v_available =
        tryCatch(
          get_available_rs_version(skip = skip_online_check),
          error = function(e) {
            warning(e)
            NULL
          }
        )
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
#' Locate the RStudio Desktop installation directory
#'
#' Searches common per-OS install locations (and, on Windows, the registry)
#' for an installed copy of RStudio Desktop. Does not require RStudio to be
#' running.
#'
#' @return A length-1 character string with the install directory, or `NULL`
#'   if no installation was found.
find_rstudio_install_dir <- function() {
  os <- get_os_type()

  candidates <- switch(
    os,
    "windows" = c(
      file.path(Sys.getenv("PROGRAMFILES"), "RStudio"),
      file.path(Sys.getenv("PROGRAMFILES(X86)"), "RStudio"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio"),
      file.path(Sys.getenv("LOCALAPPDATA"), "RStudio")
    ),
    "mac" = "/Applications/RStudio.app",
    "linux" = c(
      "/usr/lib/rstudio",
      "/usr/lib/rstudio-server",
      "/usr/local/lib/rstudio",
      "/opt/rstudio"
    ),
    character(0)
  )

  if (identical(os, "windows")) {
    install_path <- tryCatch(
      utils::readRegistry("SOFTWARE\\RStudio", hive = "HLM", maxdepth = 2)$InstallPath,
      error = function(e) NULL
    )
    candidates <- c(install_path, candidates)
  }

  candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
  existing <- candidates[dir.exists(candidates)]

  if (length(existing) == 0) {
    return(NULL)
  }

  existing[[1]]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check whether RStudio Desktop is installed, even if it is not running
#'
#' @return `TRUE`/`FALSE`.
is_rstudio_installed <- function() {
  if (rstudioapi::isAvailable()) {
    return(TRUE)
  }

  if (!is.null(find_rstudio_install_dir())) {
    return(TRUE)
  }

  exe <- if (get_os_type() == "windows") "rstudio.exe" else "rstudio"
  nzchar(Sys.which(exe))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the version of an installed (but not necessarily running) RStudio
#'
#' On Windows, prefers the `Version` value written to the registry by the
#' installer (the `VERSION` file on disk can hold an unrelated Electron shell
#' build number on newer RStudio releases). Falls back to parsing the
#' `VERSION` file that RStudio Desktop places in its install directory.
#'
#' @return A [numeric_version()] object, or `NULL` if it could not be
#'   determined.
get_installed_rstudio_version <- function() {
  if (identical(get_os_type(), "windows")) {
    v_registry <- get_rstudio_version_from_registry()
    if (!is.null(v_registry)) {
      return(v_registry)
    }
  }

  install_dir <- find_rstudio_install_dir()

  if (is.null(install_dir)) {
    return(NULL)
  }

  version_file <- if (get_os_type() == "mac") {
    file.path(install_dir, "Contents", "Resources", "VERSION")
  } else {
    file.path(install_dir, "VERSION")
  }

  if (!file.exists(version_file)) {
    return(NULL)
  }

  version_txt <- paste(readLines(version_file, warn = FALSE), collapse = " ")
  parse_rstudio_version_string(version_txt)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the RStudio version reported by the Windows installer registry entry
#'
#' @return A [numeric_version()] object, or `NULL` if not found.
get_rstudio_version_from_registry <- function() {
  reg_paths <- c(
    "SOFTWARE\\RStudio",
    "SOFTWARE\\WOW6432Node\\RStudio",
    "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\RStudio",
    "SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\RStudio"
  )

  for (reg_path in reg_paths) {
    for (hive in c("HLM", "HCU")) {
      key <- tryCatch(
        utils::readRegistry(reg_path, hive = hive, maxdepth = 2),
        error = function(e) NULL
      )

      value <- key$Version
      if (is.null(value)) {
        value <- key$DisplayVersion
      }

      if (is.null(value) || !nzchar(value)) {
        next
      }

      parsed <- parse_rstudio_version_string(value)
      if (!is.null(parsed)) {
        return(parsed)
      }
    }
  }

  NULL
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse an RStudio calendar-version string into a [numeric_version()]
#'
#' RStudio version strings look like `"2026.08.2+200"`. `numeric_version()`
#' accepts `-` (like `.`) as a component separator but not `+`, so `+` is
#' normalized to `-` to keep the build number as a 4th version component.
#'
#' @param x Character scalar containing (or surrounded by) a version string.
#' @return A [numeric_version()] object, or `NULL` if `x` has no match.
parse_rstudio_version_string <- function(x) {
  version_txt <- stringr::str_extract(x, "\\d{4}[.]\\d+[.]\\d+([+-]\\d+)?")

  if (is.na(version_txt)) {
    return(NULL)
  }

  version_txt <- sub("[+]", "-", version_txt)

  tryCatch(as.numeric_version(version_txt), error = function(e) NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_default_path_to_meld <- function() {
  path_to <-  switch(
    get_os_type(),

    "windows" =
      suppressWarnings({
        out <- system2("where", "meld", stdout = TRUE)
        if (is.null(attr(out, "status"))) {
          out
        } else {
          # Default directory
          "C:/Program Files (x86)/Meld/meld.exe"
        }
      }),
    "linux"   = "/usr/bin/meld",
    "mac"     = "/usr/bin/meld",

    # NOTE: Might not work if Meld is not installed
    suppressWarnings({
      out <- system2("which", "meld", stdout = TRUE)
      if (is.null(attr(out, "status"))) {
        out
      } else {
        ""
      }
    })
  )

  fs::path(path_to)
}

is_meld_installed <- function(path_to_meld = get_default_path_to_meld()) {
  file.exists(path_to_meld)
}

is_git_installed <- function() {
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

is_xquartz_installed  <- function(variables) {
  isTRUE(unname(capabilities("aqua")))
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
    ui_oops("{what} {red(program)} is not detected.")
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_tool_installed <- function(name = "", condition = NULL) {
  check_program_installed(name, condition, what = "Tool")
}
