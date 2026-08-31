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
      },
      # No single "latest" online Rtools version to compare against: CRAN Rtools
      # toolchains (e.g., Rtools 4.5) span multiple R minor releases (e.g., R 4.5/4.6).
      v_installed = get_installed_rtools_version()
    )
  }

  # XQuartz (on Mac)
  # NOTE: on stack overflow, it writes that this functon might hang R session
  # if XQuartz is missing.
  # https://stackoverflow.com/questions/37438773/
  if (type_lwr %in% c("all", "gmc-bs") && get_os_type() == "mac") {
    check_program_installed(
      "XQuartz",
      is_xquartz_installed(),
      v_installed = get_installed_xquartz_version()
    )
  }

  # Git
  if (type_lwr %in% c("all", "gmc-r")) {
    check_program_installed(
      "Git",
      is_git_installed(),
      v_installed = get_installed_git_version(),
      v_available = get_available_git_version(skip = skip_online_check)
    )
  }

  # Meld
  if (type_lwr %in% c("all")) {
    try(
      {
        check_program_installed(
          "Meld",
          is_meld_installed(),
          v_installed = get_installed_meld_version(),
          v_available = get_available_meld_version(skip = skip_online_check)
        )
      },
      silent = TRUE)
  }

  invisible(NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
can_check_available_version <- function(force = FALSE, get_what = "versions") {
  if (isTRUE(force)) {
    return(TRUE)
  }

  online <- tryCatch(
    pingr::is_online(),
    error = function(error) {
      cli::cli_warn(c(
        "Could not check network availability for {get_what}.",
        "i" = conditionMessage(error)
      ))
      NA
    }
  )

  if (is.na(online)) {
    return(FALSE)
  }

  if (!isTRUE(online)) {
    msg_offline(get_what = get_what)
    return(FALSE)
  }

  TRUE
}

fetch_available_version <- function(get_what, fetch) {
  tryCatch(
    {
      candidates <- fetch()
      candidates <- as.character(candidates)
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

      if (length(candidates) == 0L) {
        stop("The endpoint returned no recognizable version.")
      }

      max(as.numeric_version(candidates))
    },
    error = function(error) {
      cli::cli_warn(c(
        "Could not get the newest available {get_what}.",
        "i" = conditionMessage(error)
      ))
      NULL
    }
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_r_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (!can_check_available_version(force, "R version")) {
    return(NULL)
  }

  fetch_available_version("R version", function() {
    c("https://cran.r-project.org/src/base/R-4") |>
      purrr::map(readr::read_lines) |>
      purrr::reduce(c) |>
      stringr::str_extract("(?<=R-)\\d+[.]\\d+[.]\\d+(?=.tar.gz)")
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_available_rs_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (!can_check_available_version(force, "RStudio version")) {
    return(NULL)
  }

  fetch_available_version("RStudio version", function() {
    "https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads" |>
      readr::read_lines() |>
      stringr::str_extract("(?<=RStudio-)\\d{4}[.].*?(?=.exe)") |>
      purrr::discard(is.na)
  })
}

get_available_quarto_version <- function(force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (!can_check_available_version(force, "Quarto version")) {
    return(NULL)
  }

  fetch_available_version("Quarto version", function() {
    url <- "https://api.github.com/repos/quarto-dev/quarto-cli/releases/latest"
    rel <- jsonlite::fromJSON(url)
    sub("^v", "", rel$tag_name)
  })
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
# Registry hives to check for an RStudio install/version: per-user ("just
# me" installer mode) first, then system-wide ("all users", requires admin
# rights). Shared by `find_rstudio_install_dir()` and
# `get_rstudio_version_from_registry()` so a future hive addition only has
# to happen in one place.
rstudio_registry_hives <- function() c("HCU", "HLM")

rstudio_registry_paths <- function() {
  c(
    "SOFTWARE\\RStudio",
    "SOFTWARE\\WOW6432Node\\RStudio",
    "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\RStudio",
    "SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\RStudio"
  )
}

# Read a single registry key, returning `NULL` (never erroring) if missing,
# unreadable, or not on Windows.
read_registry_key_safely <- function(reg_path, hive) {
  if (!identical(get_os_type(), "windows")) {
    return(NULL)
  }

  tryCatch(
    utils::readRegistry(reg_path, hive = hive, maxdepth = 2),
    error = function(e) NULL
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Locate the RStudio Desktop installation directory
#'
#' Searches common per-OS install locations (and, on Windows, the registry)
#' for an installed copy of RStudio Desktop. Does not require RStudio to be
#' running. Looks for both a system-wide ("all users") and a per-user
#' ("just me") install, preferring the per-user one if both are found (see
#' [get_rstudio_install_scope()]).
#'
#' @return A length-1 character string with the install directory, or `NULL`
#'   if no installation was found.
find_rstudio_install_dir <- function() {
  os <- get_os_type()

  candidates <- switch(
    os,
    "windows" = c(
      # Per-user ("just me") install locations first.
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio"),
      # System-wide ("all users") install locations.
      file.path(Sys.getenv("PROGRAMFILES"), "RStudio"),
      file.path(Sys.getenv("PROGRAMFILES(X86)"), "RStudio")
    ),
    "mac" = c(
      path.expand("~/Applications/RStudio.app"), # per-user
      "/Applications/RStudio.app" # system-wide
    ),
    "linux" = c(
      "/usr/lib/rstudio",
      # "/usr/lib/rstudio-server", # (RStudio Server, not Desktop)
      "/usr/local/lib/rstudio",
      "/opt/rstudio"
    ),
    character(0)
  )

  if (identical(os, "windows")) {
    registry_paths <- character()
    for (hive in rstudio_registry_hives()) {
      for (reg_path in rstudio_registry_paths()) {
        key <- read_registry_key_safely(reg_path, hive)
        loc <- if (!is.null(key$InstallLocation) && nzchar(key$InstallLocation)) {
          key$InstallLocation
        } else if (!is.null(key$InstallPath) && nzchar(key$InstallPath)) {
          key$InstallPath
        } else {
          NA_character_
        }
        if (!is.na(loc)) {
          registry_paths <- c(registry_paths, loc)
        }
      }
    }
    candidates <- c(registry_paths, candidates)
  }

  candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
  existing <- candidates[dir.exists(candidates)]

  if (length(existing) == 0) {
    return(NULL)
  }

  existing[[1]]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Classify a resolved RStudio install directory as "system" (all-users) or
# "user" (per-user) scope, based on well-known per-user vs. system parent
# directories. Pure string-matching, no I/O.
classify_rstudio_install_scope <- function(install_dir) {
  if (is.null(install_dir) || is.na(install_dir) || !nzchar(install_dir)) {
    return(NA_character_)
  }

  user_roots <- c(Sys.getenv("LOCALAPPDATA"), path.expand("~"))
  user_roots <- user_roots[nzchar(user_roots)]

  normalize_path <- function(path) {
    path |>
      gsub("\\\\", "/", x = _) |>
      sub("/+$", "", x = _)
  }

  install_dir_lower <- tolower(normalize_path(install_dir))
  is_under_user_root <- any(vapply(
    user_roots,
    function(root) {
      root_lower <- tolower(normalize_path(root))
      identical(install_dir_lower, root_lower) ||
        startsWith(install_dir_lower, paste0(root_lower, "/"))
    },
    logical(1)
  ))

  if (is_under_user_root) "user" else "system"
}

#' Determine whether the local RStudio Desktop install is system-wide or per-user
#'
#' RStudio Desktop's Windows installer supports an "all users" (system-wide,
#' admin rights required) and a "just me" (per-user) install mode; macOS
#' installs are similarly either shared (`/Applications`) or per-user
#' (`~/Applications`). This distinction affects only where the RStudio
#' *application files* live (used e.g. to find the bundled
#' `user-prefs-schema.json`) — it does **not** affect where RStudio stores
#' preferences, keybindings, or other per-user state, which always lives
#' under the current OS user's profile regardless of install scope (see
#' [get_path_rstudio_config_dir()]).
#'
#' @param install_dir Character scalar, the resolved RStudio install
#'   directory. Defaults to [find_rstudio_install_dir()].
#' @return `"system"`, `"user"`, or `NA_character_` if the install location
#'   is unknown (e.g. RStudio isn't installed).
#' @export
#' @examples
#' if (interactive()) {
#'   get_rstudio_install_scope()
#' }
get_rstudio_install_scope <- function(install_dir = find_rstudio_install_dir()) {
  classify_rstudio_install_scope(install_dir)
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
  for (reg_path in rstudio_registry_paths()) {
    for (hive in rstudio_registry_hives()) {
      key <- read_registry_key_safely(reg_path, hive)

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

get_installed_meld_version <- function(path_to_meld = get_default_path_to_meld()) {
  if (!file.exists(path_to_meld)) {
    return(NULL)
  }

  extract_first_version(
    tryCatch(
      system2(path_to_meld, "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) NULL
    )
  )
}

get_available_meld_version <- function(force = FALSE, skip = FALSE) {
  # GNOME/meld publishes tags, not GitHub Releases, so the "latest release"
  # API 404s here. There is no reliable single "latest" version to fetch.
  NULL
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

get_installed_git_version <- function() {
  extract_first_version(
    tryCatch(
      system2("git", "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) NULL
    )
  )
}

get_available_git_version <- function(force = FALSE, skip = FALSE) {
  # Git for Windows releases track the Windows build; on Mac/Linux, Git is
  # usually managed by the OS/Xcode CLT/package manager, so there is no single
  # comparable "latest" version.
  if (!identical(get_os_type(), "windows")) {
    return(NULL)
  }

  get_available_version_from_github_release(
    "git-for-windows/git",
    force = force,
    skip = skip
  )
}

is_xquartz_installed  <- function() {
  isTRUE(unname(capabilities("aqua")))
}

get_installed_xquartz_version <- function() {
  if (!identical(get_os_type(), "mac")) {
    return(NULL)
  }

  plist <- "/Applications/Utilities/XQuartz.app/Contents/Info"
  if (!file.exists(paste0(plist, ".plist"))) {
    return(NULL)
  }

  out <- tryCatch(
    system2(
      "defaults",
      c("read", plist, "CFBundleShortVersionString"),
      stdout = TRUE, stderr = TRUE
    ),
    error = function(e) NULL
  )

  extract_first_version(out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the version of an installed Rtools toolchain on Windows
#'
#' Evaluates the active toolchain selected by `pkgbuild::rtools_path()` for the
#' running R session, or searches environment variables (`RTOOLS*_HOME`), the
#' registry, and `C:\rtools*` directories for the highest installed version.
#' Rtools toolchains are released per compiler toolchain update (e.g. Rtools 4.5
#' supports R 4.5.x and 4.6.x), so there is no strict 1-to-1 major.minor version
#' match requirement.
#'
#' @return A [numeric_version()] object, or `NULL` if not on Windows or not found.
#' @keywords internal
get_installed_rtools_version <- function() {
  if (!identical(get_os_type(), "windows")) {
    return(NULL)
  }

  # 1) pkgbuild's active toolchain detection for the running R session
  path <- tryCatch(pkgbuild::rtools_path(), error = function(e) NULL)
  if (!is.null(path) && any(nzchar(path))) {
    v <- rtools_code_to_version(
      stringr::str_extract(path[[1]], "(?i)(?<=rtools)\\d{2,3}")
    )
    if (!is.null(v)) {
      return(v)
    }
  }

  versions <- numeric_version(character(0))

  # 2) Env vars set by Rtools installers (e.g. "RTOOLS45_HOME", "RTOOLS44_HOME")
  env_names <- grep("^RTOOLS\\d+_HOME$", names(Sys.getenv()), value = TRUE)
  for (env_name in env_names) {
    v <- rtools_code_to_version(sub("^RTOOLS(\\d+)_HOME$", "\\1", env_name))
    if (!is.null(v)) {
      versions <- c(versions, v)
    }
  }

  # 3) Registry entries written by Rtools installers
  reg_paths <- c("SOFTWARE\\R-core\\Rtools", "SOFTWARE\\WOW6432Node\\R-core\\Rtools")
  for (reg_path in reg_paths) {
    for (hive in rstudio_registry_hives()) {
      key <- read_registry_key_safely(reg_path, hive)
      if (!is.null(key)) {
        subkey_names <- names(key)[vapply(key, is.list, logical(1))]
        parsed_v <- suppressWarnings(as.numeric_version(subkey_names))
        parsed_v <- parsed_v[!is.na(parsed_v)]
        if (length(parsed_v) > 0) {
          versions <- c(versions, parsed_v)
        }
      }
    }
  }

  # 4) Common install locations by naming convention (e.g. "C:/rtools45")
  candidates <- Sys.glob("C:/rtools*")
  codes <- stringr::str_extract(basename(candidates), "(?i)(?<=rtools)\\d{2,3}")
  glob_versions <- Filter(Negate(is.null), lapply(codes, rtools_code_to_version))
  if (length(glob_versions) > 0) {
    versions <- c(versions, do.call(c, glob_versions))
  }

  if (length(versions) == 0) {
    return(NULL)
  }

  max(unique(versions))
}

# Rtools folder/registry codes are 2-3 digit strings like "44" -> version "4.4"
rtools_code_to_version <- function(code) {
  if (is.null(code) || is.na(code) || !grepl("^\\d{2,3}$", code)) {
    return(NULL)
  }

  version_txt <- paste0(
    substr(code, 1, nchar(code) - 1), ".", substr(code, nchar(code), nchar(code))
  )

  tryCatch(as.numeric_version(version_txt), error = function(e) NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract the first "x.y[.z]"-style version number found in `x` as a
# numeric_version(), or NULL if no match was found.
extract_first_version <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  version_txt <- stringr::str_extract(paste(x, collapse = " "), "\\d+[.]\\d+([.]\\d+)?")
  if (is.na(version_txt)) {
    return(NULL)
  }

  tryCatch(as.numeric_version(version_txt), error = function(e) NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get the latest GitHub release tag for `repo` ("owner/name") as a
# numeric_version(), or NULL if unavailable/offline.
get_available_version_from_github_release <- function(repo, force = FALSE, skip = FALSE) {
  if (isTRUE(skip)) {
    return(NULL)
  }

  if (!force && !pingr::is_online()) {
    msg_offline(get_what = paste(repo, "version"))
    return(NULL)
  }

  tryCatch(
    suppressWarnings({
      url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")
      rel <- jsonlite::fromJSON(url)
      extract_first_version(rel$tag_name)
    }),
    error = function(e) NULL
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# program   - string
# condition - logical
# what      - string ("Program" or "Tool")
# v_installed/v_available - optional numeric_version()s; when v_installed is
# known, show the version-comparison line instead of the plain install status.
check_program_installed <- function(program = "", condition = NULL,
  what = "Program", v_installed = NULL, v_available = NULL) {

  if (isTRUE(condition) && !is.null(v_installed)) {
    print_program_version_info(
      name = program,
      v_installed = v_installed,
      v_available = v_available,
      type = what
    )
    return(invisible(NULL))
  }

  if (condition) {
    ui_done("{what} {blue(program)} is installed.")

  } else {
    ui_oops("{what} {red(program)} is not detected.")
  }

  invisible(NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_tool_installed <- function(name = "", condition = NULL,
  v_installed = NULL, v_available = NULL) {
  check_program_installed(
    name, condition,
    what = "Tool", v_installed = v_installed, v_available = v_available
  )
}
