test_that("directory opening wrappers request their expected paths", {
  opened_paths <- character()
  testthat::local_mocked_bindings(
    get_path_desktop = function(...) "desktop-path",
    get_path_rstudio_config_dir = function(...) "config-path",
    get_path_rstudio_internal_state_dir = function(...) "state-path",
    get_path_rstudio_keybindings_dir = function(...) "keybindings-path",
    get_path_r_environ = function(...) "environ-path",
    open_path = function(path) {
      opened_paths <<- c(opened_paths, path)
      invisible(TRUE)
    },
    .package = "bio"
  )

  expect_invisible(open_desktop())
  expect_invisible(open_rstudio_config_dir())
  expect_invisible(open_rstudio_internal_state_dir())
  expect_invisible(open_rstudio_keybindings_dir())
  expect_invisible(open_r_environ())
  expect_identical(
    opened_paths,
    c("desktop-path", "config-path", "state-path", "keybindings-path", "environ-path")
  )
})

test_that("file opening wrappers delegate their resolved targets", {
  opened_in_rstudio <- character()
  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") paste0("prefs-", which),
    get_path_recent_proj_list = function() "recent-projects",
    get_path_user_proj_list = function(...) "user-projects",
    open_in_rstudio = function(path, ...) {
      opened_in_rstudio <<- c(opened_in_rstudio, path)
      invisible(NULL)
    },
    .package = "bio"
  )

  expect_invisible(open_rstudio_config_file("bio-default"))
  expect_invisible(open_recent_proj_list())
  expect_invisible(open_user_proj_list())
  expect_identical(opened_in_rstudio, c("prefs-bio-default", "recent-projects", "user-projects"))
})

test_that("open_in_rstudio() uses RStudio or the file-show fallback", {
  navigated_path <- NULL
  shown_path <- NULL
  shown_browser <- NULL
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    hasFun = function(...) TRUE,
    navigateToFile = function(path, ...) {
      navigated_path <<- path
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(open_in_rstudio("live-file.R"))
  expect_identical(navigated_path, "live-file.R")

  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    file_show = function(path, browser) {
      shown_path <<- path
      shown_browser <<- browser
      invisible(NULL)
    },
    .package = "fs"
  )

  expect_invisible(open_in_rstudio("fallback-file.R"))
  expect_identical(shown_path, "fallback-file.R")
  expect_identical(shown_browser, "RStudio")
})

test_that("dictionary directory openers request the documented locations", {
  config_paths <- character()
  shown_paths <- character()
  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) {
      config_paths <<- c(config_paths, file.path(...))
      "system-dictionaries"
    },
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    userDictionariesPath = function() "user-dictionaries",
    dictionariesPath = function() "internal-dictionaries",
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    file_show = function(path, ...) {
      shown_paths <<- c(shown_paths, path)
      invisible(NULL)
    },
    .package = "fs"
  )

  expect_invisible(open_rstudio_system_dictionaries_dir())
  expect_invisible(open_rstudio_user_dictionaries_dir())
  expect_invisible(open_rstudio_internal_dictionaries_dir())
  expect_identical(config_paths, "dictionaries/languages-system")
  expect_identical(
    shown_paths,
    c("system-dictionaries", "user-dictionaries", "internal-dictionaries")
  )
})
