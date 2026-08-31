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
  ui_messages <- character()
  downloader_name <- ".rs.downloadAllDictionaries"
  had_downloader <- exists(downloader_name, envir = globalenv(), inherits = FALSE)
  old_downloader <- get0(downloader_name, envir = globalenv(), inherits = FALSE)
  on.exit(
    {
      if (had_downloader) {
        assign(downloader_name, old_downloader, envir = globalenv())
      } else {
        rm(list = downloader_name, envir = globalenv())
      }
    },
    add = TRUE)
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
    ui_info = function(message, ...) ui_messages <<- c(ui_messages, "info"),
    ui_done = function(message, ...) ui_messages <<- c(ui_messages, "done"),
    ui_warn = function(message, ...) ui_messages <<- c(ui_messages, "warn"),
    .package = "usethis"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    .package = "rstudioapi"
  )

  expect_invisible(expect_true(rstudio_install_spellcheck_dictionaries(secure = FALSE)))
  expect_identical(ui_messages, c("info", "done"))
  expect_identical(received_target, dic_dir)
  expect_false(received_secure)

  ui_messages <- character()
  expect_invisible(expect_true(rstudio_download_spellcheck_dictionaries(secure = TRUE)))
  expect_identical(ui_messages, c("info", "done"))
  expect_true(received_secure)

  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-zip-data", destfile)
      0L
    },
    unzip = function(zipfile, exdir, list = FALSE, ...) {
      if (list) {
        return(data.frame(Name = c("lt_LT.aff", "lt_LT.dic")))
      }
      writeLines("lt_LT", file.path(exdir, "lt_LT.aff"))
      writeLines("lt_LT", file.path(exdir, "lt_LT.dic"))
      character(0)
    },
    .package = "utils"
  )
  ui_messages <- character()
  expect_invisible(expect_true(rstudio_install_spellcheck_dictionaries()))
  expect_identical(ui_messages, c("info", "done"))

  testthat::local_mocked_bindings(
    download.file = function(...) stop("download failed"),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    .download_dictionary_archive_with_curl = function(...) FALSE,
    .package = "bio"
  )
  ui_messages <- character()
  expect_invisible(expect_false(rstudio_install_spellcheck_dictionaries()))
  expect_identical(ui_messages, c("info", "warn"))
})

test_that("dictionary installation fails when extraction only warns", {
  dic_dir <- fs::path(withr::local_tempdir(), "dictionaries")
  ui_messages <- character()

  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) dic_dir,
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    ui_info = function(message, ...) ui_messages <<- c(ui_messages, "info"),
    ui_done = function(message, ...) ui_messages <<- c(ui_messages, "done"),
    ui_warn = function(message, ...) ui_messages <<- c(ui_messages, "warn"),
    .package = "usethis"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-zip-data", destfile)
      0L
    },
    unzip = function(zipfile, exdir, list = FALSE, ...) {
      if (list) {
        return(data.frame(Name = c("lt_LT.aff", "lt_LT.dic")))
      }
      warning("error 1 in extracting from zip file")
      character(0)
    },
    .package = "utils"
  )

  expect_invisible(expect_false(rstudio_install_spellcheck_dictionaries()))
  expect_identical(ui_messages, c("info", "warn"))
  expect_false(any(grepl("lt_LT", dir(dic_dir), fixed = TRUE)))
})

test_that("dictionary installation fails when extraction drops required files", {
  dic_dir <- fs::path(withr::local_tempdir(), "dictionaries")
  ui_messages <- character()

  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) dic_dir,
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    ui_info = function(message, ...) ui_messages <<- c(ui_messages, "info"),
    ui_done = function(message, ...) ui_messages <<- c(ui_messages, "done"),
    ui_warn = function(message, ...) ui_messages <<- c(ui_messages, "warn"),
    .package = "usethis"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-zip-data", destfile)
      0L
    },
    unzip = function(zipfile, exdir, list = FALSE, ...) {
      if (list) {
        return(data.frame(Name = c("lt_LT.aff", "lt_LT.dic")))
      }
      # Silent partial extraction: only one of the two required files lands.
      writeLines("lt_LT", file.path(exdir, "lt_LT.dic"))
      character(0)
    },
    .package = "utils"
  )

  expect_invisible(expect_false(rstudio_install_spellcheck_dictionaries()))
  expect_identical(ui_messages, c("info", "warn"))
})

test_that("dictionary archive download retries interrupted transfers", {
  archive_path <- withr::local_tempfile(fileext = ".zip")
  attempts <- 0L

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      attempts <<- attempts + 1L
      writeLines(if (attempts < 3L) "partial" else "complete", destfile)
      if (attempts < 3L) 1L else 0L
    },
    unzip = function(zipfile, list = FALSE, ...) {
      if (list && identical(readLines(zipfile), "complete")) {
        return(data.frame(Name = c("lt_LT.aff", "lt_LT.dic")))
      }
      stop("invalid archive")
    },
    .package = "utils"
  )

  expect_true(bio:::.download_dictionary_archive("https://example.test/dictionaries.zip", archive_path))
  expect_identical(attempts, 3L)
})

test_that("default configuration reports headless dictionary failures accurately", {
  testthat::local_mocked_bindings(
    get_path_rstudio_config_dir = function(...) dict_dir,
    rstudio_download_spellcheck_dictionaries = function(...) FALSE,
    rstudio_reset_user_settings = function(...) invisible(TRUE),
    rstudio_reset_keybindings = function(...) invisible(TRUE),
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    install_snippets_from_package = function(...) invisible(TRUE),
    .package = "snippets"
  )
  testthat::local_mocked_bindings(
    install_tinytex = function(...) invisible(TRUE),
    .package = "tinytex"
  )

  result <- suppressMessages(rstudio_configure_defaults())

  expect_false(result$ok[result$step == "dictionaries"])
  expect_match(
    result$message[result$step == "dictionaries"],
    "dictionaries were not installed"
  )
  expect_false(grepl(
    "running RStudio session",
    result$message[result$step == "dictionaries"],
    fixed = TRUE
  ))
})

test_that("keybinding resets update only a temporary keybindings directory", {
  keybindings_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
  dict_dir <- withr::local_tempdir()

    get_path_rstudio_keybindings_dir = function() keybindings_dir,
    .package = "bio"
  )

  expect_invisible(rstudio_reset_keybindings("bio-default", backup = FALSE))
  expect_true(length(fs::dir_ls(keybindings_dir, regexp = "[.]json$")) > 0L)

  expect_invisible(rstudio_reset_keybindings("rstudio-default", backup = FALSE))
  expect_length(fs::dir_ls(keybindings_dir, regexp = "[.]json$"), 0L)
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

test_that("console clearing follows both RStudio question responses", {
  commands <- character()
  answer <- TRUE
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    showQuestion = function(...) answer,
    executeCommand = function(command, ...) {
      commands <<- c(commands, command)
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  # showQuestion() returns TRUE for the first button, which is "No" here.
  expect_invisible(rstudio_clear_console_ask())
  expect_identical(commands, character())

  answer <- FALSE
  expect_invisible(rstudio_clear_console_ask())
  expect_identical(commands, "consoleClear")
})

test_that("preference reset restores the original file after a later failure", {
  root <- withr::local_tempdir()
  current <- fs::path(root, "rstudio-prefs.json")
  rstudio_default <- fs::path(root, "rstudio-default.json")
  bio_default <- fs::path(root, "bio-default.json")
  original <- charToRaw('{"editor_theme":"Original"}\n')
  writeBin(original, current)
  writeLines('{"editor_theme":"Textmate (default)"}', rstudio_default)
  writeLines('{"save_workspace":"never"}', bio_default)
  calls <- 0L

  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") {
      switch(which,
        current = current,
        `rstudio-default` = rstudio_default,
        bio = bio_default
      )
    },
    rstudio_set_preferences = function(file) {
      calls <<- calls + 1L
      writeLines(sprintf('{"partial":%d}', calls), current)
      if (calls == 2L) stop("second preset failed")
      TRUE
    },
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )

  expect_error(
    rstudio_reset_user_settings("bio-default", backup = FALSE, ask = FALSE),
    "second preset failed"
  )
  expect_identical(readBin(current, "raw", n = file.info(current)$size), original)
})

test_that("preference reset removes partial output when no original existed", {
  root <- withr::local_tempdir()
  current <- fs::path(root, "rstudio-prefs.json")
  malformed <- fs::path(root, "rstudio-default.json")
  writeLines("{not-json", malformed)

  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") {
      switch(which, current = current, `rstudio-default` = malformed)
    },
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )

  expect_error(
    rstudio_reset_user_settings("rstudio-default", backup = FALSE, ask = FALSE)
  )
  expect_false(fs::file_exists(current))
})

test_that("headless preference reset applies both presets", {
  root <- withr::local_tempdir()
  current <- fs::path(root, "rstudio-prefs.json")
  rstudio_default <- fs::path(root, "rstudio-default.json")
  bio_default <- fs::path(root, "bio-default.json")
  writeLines('{"editor_theme":"Textmate (default)","remove_me":true}', current)
  writeLines('{"editor_theme":"Textmate (default)"}', rstudio_default)
  writeLines('{"save_workspace":"never"}', bio_default)

  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") {
      switch(which,
        current = current,
        `rstudio-default` = rstudio_default,
        bio = bio_default
      )
    },
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    dir_create = function(...) invisible(NULL),
    .package = "fs"
  )

  expect_invisible(rstudio_reset_user_settings("bio-default", backup = FALSE, ask = FALSE))
  result <- jsonlite::fromJSON(current, simplifyVector = FALSE)
  expect_identical(result$editor_theme, "Textmate (default)")
  expect_identical(result$save_workspace, "never")
  expect_false("remove_me" %in% names(result))
})

test_that("a live session leaves the preference file to RStudio", {
  root <- withr::local_tempdir()
  current <- fs::path(root, "rstudio-prefs.json")
  rstudio_default <- fs::path(root, "rstudio-default.json")
  bio_default <- fs::path(root, "bio-default.json")
  writeLines('{"editor_theme":"Original"}', current)
  writeLines('{"editor_theme":"Textmate (default)"}', rstudio_default)
  writeLines('{"save_workspace":"never"}', bio_default)
  applied <- character()

  testthat::local_mocked_bindings(
    get_path_rstudio_config_file = function(which = "current") {
      switch(which,
        current = current,
        `rstudio-default` = rstudio_default,
        bio = bio_default
      )
    },
    rstudio_set_preferences = function(file) {
      applied <<- c(applied, fs::path_file(file))
      TRUE
    },
    .package = "bio"
  )
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    applyTheme = function(...) invisible(NULL),
    executeCommand = function(...) invisible(NULL),
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    dir_create = function(...) invisible(NULL),
    .package = "fs"
  )

  expect_invisible(rstudio_reset_user_settings("bio-default", backup = FALSE, ask = FALSE))

  expect_identical(applied, c("rstudio-default.json", "bio-default.json"))
  expect_true(fs::file_exists(current))
  expect_identical(readLines(current), '{"editor_theme":"Original"}')
  expect_length(fs::dir_ls(root, regexp = "rstudio-prefs-"), 0L)
})

test_that("a rejected preference key does not discard the whole preset", {
  preset <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"editor_theme":"Textmate (default)","unknown_key":true,"save_workspace":"never"}',
    preset
  )
  written <- character()

  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    writeRStudioPreference = function(name, value) {
      if (identical(name, "unknown_key")) stop("Unknown preference name")
      written <<- c(written, name)
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_warning(
    expect_true(rstudio_set_preferences(preset)),
    "unknown_key"
  )
  expect_identical(written, c("editor_theme", "save_workspace"))
})

test_that("preference writes retry with the type RStudio asks for", {
  preset <- withr::local_tempfile(fileext = ".json")
  writeLines('{"num_spaces_for_tab":2}', preset)
  attempts <- list()

  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    writeRStudioPreference = function(name, value) {
      attempts[[length(attempts) + 1L]] <<- value
      if (length(attempts) == 1L) stop("expected <Integer>")
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_true(rstudio_set_preferences(preset))
  expect_length(attempts, 2L)
  expect_true(is.integer(attempts[[2]]))
})

test_that("a malformed preset still aborts and triggers rollback", {
  preset <- withr::local_tempfile(fileext = ".json")
  writeLines("{not-json", preset)

  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    .package = "rstudioapi"
  )

  expect_error(rstudio_set_preferences(preset))
})

test_that("reset steps report warnings and errors in their summaries", {
  expect_warning(
    warning_result <- suppressMessages(run_reset_step("Warn", warning("careful"))),
    "Warn: careful"
  )
  error_result <- suppressMessages(run_reset_step("Fail", stop("boom")))

  expect_true(warning_result$ok)
  expect_true(is.na(warning_result$message))
  expect_false(error_result$ok)
  expect_identical(error_result$message, "boom")

  summary <- suppressMessages(summarize_reset_steps(list(
    warning = warning_result,
    error = error_result
  )))
  expect_identical(summary$ok, c(TRUE, FALSE))
  expect_identical(summary$message, c(NA_character_, "boom"))
})

test_that("preference transaction rolls back a false result", {
  current <- withr::local_tempfile(fileext = ".json")
  original <- charToRaw('{"value":"original"}\n')
  writeBin(original, current)

  result <- bio:::with_preference_file_rollback(current, {
    writeLines('{"value":"partial"}', current)
    FALSE
  })

  expect_false(result)
  expect_identical(readBin(current, "raw", n = file.info(current)$size), original)
})

test_that("all bundled RStudio settings assets contain valid JSON objects", {
  files <- fs::dir_ls(path_bio_rs(), regexp = "[.]json$")
  expect_length(files, 4L)

  assets <- lapply(files, jsonlite::fromJSON, simplifyVector = FALSE)
  expect_true(all(vapply(assets, is.list, logical(1))))
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

test_that("session reset honors safety gates without running commands", {
  command_count <- 0L
  testthat::local_mocked_bindings(
    executeCommand = function(...) {
      command_count <<- command_count + 1L
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(suppressMessages(rstudio_reset_session_state(ignore_ip = FALSE)))
  expect_identical(command_count, 0L)

  testthat::local_mocked_bindings(
    isAvailable = function(...) FALSE,
    .package = "rstudioapi"
  )
  expect_invisible(suppressMessages(rstudio_reset_session_state(ignore_ip = TRUE)))
  expect_identical(command_count, 0L)
})

test_that("session reset runs mocked steps with history last", {
  events <- character()
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)
  writeLines("temporary history", ".Rhistory")

  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    executeCommand = function(command, ...) {
      events <<- c(events, command)
      invisible(NULL)
    },
    applyTheme = function(theme, ...) {
      events <<- c(events, paste0("theme:", theme))
      invisible(NULL)
    },
    .package = "rstudioapi"
  )
  testthat::local_mocked_bindings(
    clear_r_workspace = function(...) {
      events <<- c(events, "clearWorkspace")
      invisible(NULL)
    },
    rstudio_reset_layout = function(...) {
      events <<- c(events, "resetLayout")
      invisible(NULL)
    },
    rstudio_clear_history = function(backup = FALSE) {
      if (!identical(backup, FALSE)) stop("history backup must be disabled")
      events <<- c(events, "clearHistory")
      invisible(NULL)
    },
    .package = "bio"
  )

  result <- suppressMessages(rstudio_reset_session_state(ignore_ip = TRUE))

  expect_identical(
    result$step,
    c(
      "working_dir", "recent_files", "plots", "help", "viewer",
      "recent_projects", "workspace", "layout", "theme", "documents",
      "terminals", "console", "history"
    )
  )
  expect_true(all(result$ok))
  expect_identical(tail(events, 1L), "clearHistory")
  expect_false(fs::file_exists(".Rhistory"))
})

test_that("combined reset honors its guard and delegates both phases", {
  calls <- character()
  testthat::local_mocked_bindings(
    rstudio_configure_defaults = function(force_update_dictionaries = FALSE) {
      calls <<- c(calls, paste0("configure:", force_update_dictionaries))
      data.frame(step = "configure", ok = TRUE, message = NA_character_)
    },
    rstudio_reset_session_state = function(...) {
      calls <<- c(calls, "session")
      data.frame(step = "session", ok = TRUE, message = NA_character_)
    },
    .package = "bio"
  )

  expect_invisible(suppressMessages(rstudio_reset_gmc(ignore_ip = FALSE)))
  expect_identical(calls, character())

  result <- suppressMessages(rstudio_reset_gmc(
    ignore_ip = TRUE,
    force_update_dictionaries = TRUE
  ))

  expect_identical(calls, c("configure:TRUE", "session"))
  expect_identical(result$configure$step, "configure")
  expect_identical(result$session_state$step, "session")
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
  expect_false("extra" %in% diff_df$path)
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
