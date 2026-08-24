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
