test_that("get_os_type returns a normalized single-value OS label", {
  os <- get_os_type()

  expect_type(os, "character")
  expect_length(os, 1L)
  expect_true(nzchar(os))
})

test_that("architecture helpers return scalar booleans", {
  expect_type(is_32bit_os(), "logical")
  expect_type(is_64bit_os(), "logical")
  expect_length(is_32bit_os(), 1L)
  expect_length(is_64bit_os(), 1L)
})

test_that("program checks do not require obsolete version metadata", {
  expect_invisible(check_installed_programs(type = "main", skip_online_check = TRUE))
  expect_no_error(check_r_version(skip_online_check = TRUE))
  expect_no_error(check_quarto_version(skip_online_check = TRUE))
})

test_that("print_program_version_info handles all scalar edge cases", {
  expect_no_error(print_program_version_info("ToolX", NULL, "1.2.3"))
  out_missing <- capture_messages(print_program_version_info("ToolX", NULL, "1.2.3"))
  out_missing <- gsub("\\x1b\\[[0-9;]*m", "", out_missing)
  expect_true(any(grepl("ToolX", out_missing, fixed = TRUE)))
  expect_true(any(grepl("not found", out_missing, fixed = TRUE)))
  expect_true(any(grepl("available", out_missing, fixed = TRUE)))

  out_installed <- capture_messages(print_program_version_info("R", "4.5.0", "4.5.1"))
  out_installed <- gsub("\\x1b\\[[0-9;]*m", "", out_installed)
  expect_true(any(grepl("R", out_installed, fixed = TRUE)))
  expect_true(any(grepl("is installed", out_installed, fixed = TRUE)))
  expect_true(any(grepl("available", out_installed, fixed = TRUE)))

  out_installed_no_avail <- capture_messages(print_program_version_info("R", "4.5.0", NULL))
  out_installed_no_avail <- gsub("\\x1b\\[[0-9;]*m", "", out_installed_no_avail)
  expect_true(any(grepl("R", out_installed_no_avail, fixed = TRUE)))
  expect_true(any(grepl("is installed", out_installed_no_avail, fixed = TRUE)))
  expect_false(any(grepl("available", out_installed_no_avail, fixed = TRUE)))

  out_equal <- capture_messages(print_program_version_info("R", "4.5.2", "4.5.2"))
  out_equal <- gsub("\\x1b\\[[0-9;]*m", "", out_equal)
  expect_true(any(grepl("4.5.2", out_equal, fixed = TRUE)))
  expect_true(any(grepl("available online", out_equal, fixed = TRUE)))

  out_newer <- capture_messages(print_program_version_info("R", "4.5.0", "4.5.1"))
  out_newer <- gsub("\\x1b\\[[0-9;]*m", "", out_newer)
  expect_true(any(grepl("4.5.1", out_newer, fixed = TRUE)))

  out_older <- capture_messages(print_program_version_info("R", "4.5.3", "4.5.2"))
  out_older <- gsub("\\x1b\\[[0-9;]*m", "", out_older)
  expect_true(any(grepl("4.5.2", out_older, fixed = TRUE)))
})

test_that("assert_single_value rejects non-scalar inputs", {
  expect_no_error(assert_single_value("R", "name", allow_null = FALSE, allow_na = FALSE))
  expect_error(assert_single_value(c("R", "S"), "name", allow_null = FALSE, allow_na = FALSE), "single value")
  expect_error(assert_single_value(NA, "name", allow_null = FALSE, allow_na = FALSE), "must not be NA")
})

test_that("print_program_version_info rejects non-scalar version input", {
  expect_error(print_program_version_info("ToolX", c(NA, NA), c(NA, "1.2.3")), "single value")
  expect_error(print_program_version_info("R", c("4.5.0", "4.5.1"), c("4.5.1", "4.5.2")), "single value")
  expect_error(print_program_version_info("R", c("4.5.0", NA), "4.5.2"), "single value")
  expect_no_error(print_program_version_info("R", NA, "4.5.2"))
})

test_that("reset helpers are scalar-safe and quiet without the IP gate", {
  expect_true(restriction_status(ignore_ip = TRUE))
  expect_false(restriction_status(ignore_ip = FALSE))
  expect_false(restriction_status(ignore_ip = NA))

  with_safe_local_env(function(env) {
    env$x <- 1
    env$y <- 2

    expect_identical(clear_r_workspace(env), env)
    expect_false(exists("x", envir = env))
    expect_false(exists("y", envir = env))
  })

  with_mocked_rstudio_api({
    expect_invisible(rstudio_activate_console())
    expect_invisible(rstudio_clear_console_ask())
    expect_invisible(rstudio_reset_layout("left"))
    expect_invisible(rstudio_reset_layout("right"))
  })

  expect_error(rstudio_reset_layout("middle"), "should be one of")
})

test_that("RStudio restart and reload helpers send the expected commands", {
  commands <- character()
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    executeCommand = function(command, ...) {
      commands <<- c(commands, command)
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(rstudio_restart_r())
  expect_invisible(restart_r())
  expect_invisible(rstudio_reload_ui())
  expect_invisible(restart_rstudio())
  expect_identical(commands, c("restartR", "restartR", "reloadUi", "reloadUi"))
})

test_that("RStudio restart and reload helpers are quiet without RStudio", {
  commands <- character()
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    executeCommand = function(command, ...) {
      commands <<- c(commands, command)
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(rstudio_restart_r())
  expect_invisible(restart_r())
  expect_invisible(rstudio_reload_ui())
  expect_invisible(restart_rstudio())
  expect_identical(commands, character())
})

test_that("check_user_info() returns stable troubleshooting metadata", {
  output <- capture.output(info <- check_user_info())

  expect_s3_class(info, "tbl_df")
  expect_named(info, c("Setting", "Value"))
  expect_true(all(c("Operating system", "Platform", "R_HOME") %in% info$Setting))
  expect_true(any(grepl("Operating system", output, fixed = TRUE)))
})

test_that("pkg_list_archived_versions() parses pages and handles failures offline", {
  testthat::local_mocked_bindings(
    readLines = function(...) c(
      "href=\"demo_1.2.0.tar.gz\"",
      "href=\"demo_1.10.0.tar.gz\"",
      "unrelated text"
    ),
    .package = "base"
  )
  expect_identical(
    as.character(pkg_list_archived_versions("demo")),
    c("1.10.0", "1.2.0")
  )

  testthat::local_mocked_bindings(
    readLines = function(...) stop("offline"),
    .package = "base"
  )
  expect_identical(pkg_list_archived_versions("demo"), as.numeric_version(NULL))
})

test_that("classify_rstudio_install_scope() distinguishes user vs system installs", {
  expect_identical(classify_rstudio_install_scope(NULL), NA_character_)
  expect_identical(classify_rstudio_install_scope(NA_character_), NA_character_)
  expect_identical(classify_rstudio_install_scope(""), NA_character_)

  withr::local_envvar(c(LOCALAPPDATA = "C:/Users/test/AppData/Local"))
  expect_identical(
    classify_rstudio_install_scope("C:\\Users\\test\\AppData\\Local\\Programs\\RStudio"),
    "user"
  )
  expect_identical(
    classify_rstudio_install_scope("C:/Users/test/AppData/Locality/RStudio"),
    "system"
  )

  home_path <- file.path(path.expand("~"), "Applications", "RStudio.app")
  expect_identical(classify_rstudio_install_scope(home_path), "user")

  expect_identical(classify_rstudio_install_scope("C:/Program Files/RStudio"), "system")

  expect_identical(classify_rstudio_install_scope("/usr/lib/rstudio"), "system")
})

test_that("get_rstudio_install_scope() delegates to classify_rstudio_install_scope()", {
  expect_identical(get_rstudio_install_scope(NULL), NA_character_)
  expect_identical(
    get_rstudio_install_scope(file.path(Sys.getenv("PROGRAMFILES"), "RStudio")),
    "system"
  )
})

test_that("get_path_rstudio_config_dir() uses user-scoped paths and overrides", {
  fake_root <- withr::local_tempdir()
  fake_appdata <- fs::path(fake_root, "AppData", "Roaming")
  testthat::local_mocked_bindings(get_os_type = function() "windows")
  withr::local_envvar(c(
    APPDATA = fake_appdata,
    XDG_CONFIG_HOME = "",
    RSTUDIO_CONFIG_HOME = ""
  ))
  expect_identical(
    get_path_rstudio_config_dir("dictionaries"),
    fs::path(fake_appdata, "RStudio", "dictionaries")
  )

  withr::local_envvar(c(
    XDG_CONFIG_HOME = fs::path(fake_root, ".config"),
    RSTUDIO_CONFIG_HOME = fs::path(fake_root, ".rstudio-config")
  ))
  expect_identical(
    get_path_rstudio_config_dir(),
    fs::path(fake_root, ".rstudio-config")
  )
})

test_that("get_path_rstudio_internal_state_dir() uses current RStudio paths", {
  withr::local_envvar(c(LOCALAPPDATA = "C:/Users/test/AppData/Local"))

  testthat::local_mocked_bindings(get_os_type = function() "windows")
  expect_identical(
    get_path_rstudio_internal_state_dir(),
    fs::path("C:/Users/test/AppData/Local", "RStudio")
  )

  testthat::local_mocked_bindings(get_os_type = function() "linux")
  expect_identical(
    get_path_rstudio_internal_state_dir(),
    fs::path_expand_r("~/.local/share/rstudio")
  )

  testthat::local_mocked_bindings(get_os_type = function() "mac")
  expect_identical(
    get_path_rstudio_internal_state_dir(),
    fs::path_expand_r("~/.local/share/rstudio")
  )
})

test_that("exported path helpers compose user and RStudio paths", {
  fake_root <- withr::local_tempdir()
  fake_config <- fs::path(fake_root, "rstudio-config")
  fake_state <- fs::path(fake_root, "rstudio-state")
  fake_environ <- fs::path(fake_root, ".Renviron")
  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) fs::path(fake_config, ...),
    get_path_rstudio_internal_state_dir = function(...) fs::path(fake_state, ...),
    .package = "bio"
  )
  withr::local_envvar(c(R_ENVIRON_USER = fake_environ))

  expect_identical(
    get_path_rstudio_keybindings_dir(),
    fs::path(fake_config, "keybindings")
  )
  expect_identical(
    get_path_recent_proj_list(),
    fs::path(fake_state, "monitored/lists/project_mru")
  )
  expect_identical(get_path_r_environ(), fake_environ)
  expect_identical(
    get_path_desktop("notes.txt"),
    fs::path(fs::path_expand("~/Desktop"), "notes.txt")
  )
})

test_that("find_rstudio_install_dir() finds a synthetic per-user install", {
  fake_root <- withr::local_tempdir()
  fake_local_appdata <- file.path(fake_root, "LocalAppData")
  fake_install_dir <- file.path(fake_local_appdata, "Programs", "RStudio")
  dir.create(fake_install_dir, recursive = TRUE)

  withr::local_envvar(c(
    LOCALAPPDATA = fake_local_appdata,
    "PROGRAMFILES(X86)" = file.path(fake_root, "does-not-exist-x86"),
    PROGRAMFILES = file.path(fake_root, "does-not-exist")
  ))
  testthat::local_mocked_bindings(get_os_type = function() "windows")
  testthat::local_mocked_bindings(
    read_registry_key_safely = function(reg_path, hive) NULL
  )

  expect_identical(find_rstudio_install_dir(), fake_install_dir)
  expect_identical(get_rstudio_install_scope(), "user")
})

test_that("find_rstudio_install_dir() finds a synthetic system-wide install", {
  # Note: doesn't also assert `get_rstudio_install_scope()` here, because
  # `withr::local_tempdir()` always lives under the real user's home
  # directory, which `classify_rstudio_install_scope()` treats as "user"
  # scope regardless of the (fake) PROGRAMFILES override below. Scope
  # classification is covered directly with realistic absolute paths in
  # "classify_rstudio_install_scope() distinguishes user vs system installs".
  fake_root <- withr::local_tempdir()
  fake_program_files <- file.path(fake_root, "ProgramFiles")
  fake_install_dir <- file.path(fake_program_files, "RStudio")
  dir.create(fake_install_dir, recursive = TRUE)

  withr::local_envvar(c(
    LOCALAPPDATA = file.path(fake_root, "does-not-exist-local"),
    "PROGRAMFILES(X86)" = file.path(fake_root, "does-not-exist-x86"),
    PROGRAMFILES = fake_program_files
  ))
  testthat::local_mocked_bindings(get_os_type = function() "windows")
  testthat::local_mocked_bindings(
    read_registry_key_safely = function(reg_path, hive) NULL
  )

  expect_identical(find_rstudio_install_dir(), fake_install_dir)
})

test_that("read_registry_key_safely() is a no-op off Windows", {
  testthat::local_mocked_bindings(get_os_type = function() "linux")
  expect_null(read_registry_key_safely("SOFTWARE\\RStudio", "HCU"))
})
