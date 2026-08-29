test_that("keybinding defaults are documented and valid", {
  expect_identical(keybindings_defaults, c("bio-default", "rstudio-default"))
  expect_error(
    rstudio_reset_keybindings(),
    "argument 'to' is missing",
    fixed = FALSE
  )
  expect_error(
    rstudio_reset_keybindings("unknown-preset"),
    "Unknown type of keybindings",
    fixed = FALSE
  )
})

test_that("user setting preset names are stable", {
  expect_identical(
    user_setting_set_names,
    c("bio-default", "bio-dark-blue", "bio-black", "rstudio-default")
  )

  expect_error(
    rstudio_reset_user_settings(),
    "argument 'to' is missing",
    fixed = FALSE
  )

  expect_error(
    rstudio_reset_user_settings("no-such-preset"),
    "Must be element of set",
    fixed = FALSE
  )
})

test_that("dictionary deletion handles non-interactive and declined requests", {
  dic_dir <- withr::local_tempdir()
  writeLines("dictionary", file.path(dic_dir, "test.dic"))
  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) dic_dir,
    .package = "bio"
  )

  expect_invisible(rstudio_delete_spellcheck_dictionaries(ask = FALSE))
  expect_false(fs::dir_exists(dic_dir))

  fs::dir_create(dic_dir)
  testthat::local_mocked_bindings(
    ui_nope = function(...) TRUE,
    .package = "usethis"
  )
  expect_invisible(suppressWarnings(rstudio_delete_spellcheck_dictionaries(ask = TRUE)))
  expect_true(fs::dir_exists(dic_dir))
})

test_that("dictionary installer delegates only in a supported RStudio session", {
  dic_dir <- fs::path(withr::local_tempdir(), "dictionaries")
  received_target <- NULL
  received_secure <- NULL
  downloader_name <- ".rs.downloadAllDictionaries"
  had_downloader <- exists(downloader_name, envir = globalenv(), inherits = FALSE)
  old_downloader <- get0(downloader_name, envir = globalenv(), inherits = FALSE)
  on.exit({
    if (had_downloader) {
      assign(downloader_name, old_downloader, envir = globalenv())
    } else {
      rm(list = downloader_name, envir = globalenv())
    }
  }, add = TRUE)
  assign(downloader_name, function(targetDir, secure) {
    received_target <<- targetDir
    received_secure <<- secure
    TRUE
  }, envir = globalenv())

  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) dic_dir,
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    .package = "rstudioapi"
  )

  expect_true(rstudio_install_spellcheck_dictionaries(secure = FALSE))
  expect_identical(received_target, dic_dir)
  expect_false(received_secure)

  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  expect_false(rstudio_install_spellcheck_dictionaries())
})

test_that("declined user-settings reset does not modify preferences", {
  preference_file <- withr::local_tempfile(fileext = ".json")
  writeLines('{"editor_theme":"Textmate (default)"}', preference_file)
  testthat::local_mocked_bindings(
    rstudio_clear_console_ask = function(...) invisible(NULL),
    get_path_rstudio_config_file = function(which = "current") preference_file,
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    showQuestion = function(...) TRUE,
    .package = "rstudioapi"
  )

  expect_invisible(suppressWarnings(rstudio_reset_user_settings("bio-default", ask = TRUE)))
  expect_true(fs::file_exists(preference_file))
  expect_identical(readLines(preference_file), '{"editor_theme":"Textmate (default)"}')
})

test_that("reset helpers hold the expected scalar contracts", {
  expect_true(restriction_status(ignore_ip = TRUE))
  expect_false(restriction_status(ignore_ip = FALSE))
  expect_false(restriction_status(ignore_ip = NA))

  with_safe_local_env(function(env) {
    env$x <- 1

    expect_identical(clear_r_workspace(env), env)
    expect_false(exists("x", envir = env))
  })

  with_mocked_rstudio_api({
    expect_invisible(rstudio_activate_console())
    expect_invisible(rstudio_clear_console_ask())
    expect_invisible(rstudio_reset_layout("left"))
    expect_invisible(rstudio_reset_layout("right"))
  })

  expect_error(rstudio_reset_layout("middle"), "should be one of")
})

test_that("clear_r_history() delegates to RStudio in a live session", {
  commands <- character()
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    executeCommand = function(command, ...) {
      commands <<- c(commands, command)
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(clear_r_history(backup = TRUE))
  expect_identical(commands, c("saveHistory", "clearHistory"))
})

test_that("compare settings helper is callable with valid presets", {
  expect_error(
    rstudio_compare_user_settings(to = "bad-name"),
    "should be one of",
    fixed = FALSE
  )

  expect_error(
    rstudio_compare_user_settings(to = "bio-default", source = "bad-source"),
    "should be one of",
    fixed = FALSE
  )

  expect_error(
    rstudio_compare_user_settings(to = "bio-default", output = "bad-output"),
    "should be one of",
    fixed = FALSE
  )

  expect_true(is.character(match.arg("bio-default", c("bio-default", "rstudio-default"))))
})

test_that("summarize_pref_diff() classifies matches, diffs, and nested keys", {
  default_prefs <- list(a = 1, b = 2, nested = list(x = 1, y = 2))
  current_prefs <- list(a = 1, b = 99, nested = list(x = 1), extra = "surprise")

  diff_df <- summarize_pref_diff(default_prefs, current_prefs)

  by_path <- stats::setNames(diff_df$status, diff_df$path)
  expect_identical(by_path[["a"]], "identical")
  expect_identical(by_path[["b"]], "different")
  expect_identical(by_path[["nested$x"]], "identical")
  expect_identical(by_path[["nested$y"]], "missing_in_current")
  expect_identical(by_path[["extra"]], "missing_in_default")
})

test_that("summarize_pref_diff() handles empty input without erroring", {
  diff_df <- summarize_pref_diff(list(), list())
  expect_equal(nrow(diff_df), 0L)
  expect_named(diff_df, c("path", "status", "default", "current"))
})

test_that("format_pref_value() renders scalars, vectors, lists, and NULL", {
  expect_true(is.na(format_pref_value(NULL)))
  expect_equal(format_pref_value("Chat"), "Chat")
  expect_equal(format_pref_value(c("a", "b")), "a, b")
  expect_equal(format_pref_value(list(x = 1, y = 2)), "<list, length 2>")
})

test_that("fill_missing_defaults() recurses and never adds unwanted keys", {
  wanted <- list(a = 1, panes = list(x = 1, y = 2, z = 3))
  defaults <- list(a = 99, panes = list(x = 100, y = 200, z = 300), unrelated = "nope")
  current <- list(panes = list(x = 1))

  filled <- fill_missing_defaults(current, defaults, wanted)

  expect_equal(attr(filled, "n_filled"), 3L)
  expect_false("unrelated" %in% names(filled))
  expect_equal(filled$a, 99)
  expect_equal(filled$panes, list(x = 1, y = 200, z = 300))
})

test_that("fill_missing_defaults() leaves already-set values untouched", {
  wanted <- list(a = 1)
  defaults <- list(a = 99)
  current <- list(a = 1)

  filled <- fill_missing_defaults(current, defaults, wanted)

  expect_equal(attr(filled, "n_filled"), 0L)
  expect_equal(filled$a, 1)
})

test_that("print_pref_diff_summary() reports counts and, with details, key lists", {
  diff_df <- summarize_pref_diff(
    list(a = 1, b = 2, c = 3),
    list(a = 1, b = 99)
  )

  minimal_out <- capture_all_output(
    print_pref_diff_summary(diff_df, "bio-default", "current", details = FALSE)
  )
  expect_true(any(grepl("difference\\(s\\) found\\.", minimal_out)))
  expect_false(any(grepl("- b ", minimal_out, fixed = TRUE)))

  detailed_out <- capture_all_output(
    print_pref_diff_summary(diff_df, "bio-default", "current", details = TRUE)
  )
  expect_true(any(grepl("- b ", detailed_out, fixed = TRUE)))
  expect_true(any(grepl("- c ", detailed_out, fixed = TRUE)))
})

test_that("print_pref_diff_summary() reports a clean match with no differences", {
  diff_df <- summarize_pref_diff(list(a = 1), list(a = 1))
  out <- capture_all_output(
    print_pref_diff_summary(diff_df, "bio-default", "current")
  )
  expect_true(any(grepl("All settings match\\.", out)))
})

test_that("read_pref_file() reads and normalizes a JSON preferences file", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(editor_theme = "Cobalt", n = 1L), tmp, auto_unbox = TRUE)

  prefs <- read_pref_file(tmp)

  expect_equal(prefs$editor_theme, "Cobalt")
  expect_equal(prefs$n, 1) # integers are normalized to double
})

test_that("rstudio_compare_user_settings(source = 'live') works with a mocked RStudio session", {
  tmp <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(editor_theme = "Cobalt"), tmp, auto_unbox = TRUE)

  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") tmp,
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    readRStudioPreference = function(name, default) {
      if (identical(name, "editor_theme")) "Cobalt" else default
    },
    .package = "rstudioapi"
  )

  out <- capture_all_output(
    result <- rstudio_compare_user_settings(to = "bio-default", source = "live", output = "minimal")
  )
  expect_true(any(grepl("settings match", out)))
  expect_s3_class(result, "data.frame")
})

test_that("rstudio_compare_user_settings(source = 'live') fails gracefully without RStudio", {
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )

  expect_null(
    rstudio_compare_user_settings(to = "bio-default", source = "live", output = "minimal")
  )
})

test_that("RStudio preference values are normalized for comparison", {
  expect_equal(
    normalize_rstudio_preference_value(list("Chat")),
    "Chat"
  )
  expect_equal(
    normalize_rstudio_preference_value(list("tmux", "screen")),
    c("tmux", "screen")
  )
  expect_equal(
    normalize_rstudio_preference_value(list()),
    character()
  )
  expect_equal(
    normalize_rstudio_preference_value(list(panes = list("Source"))),
    list(panes = "Source")
  )
  expect_equal(
    normalize_rstudio_preference_value(1L),
    1
  )
})
