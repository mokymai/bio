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

test_that("compare settings helper is callable with valid presets", {
  expect_error(
    rstudio_compare_user_settings(to = "bad-name"),
    "should be one of",
    fixed = FALSE
  )

  expect_true(is.character(match.arg("bio-default", c("bio-default", "rstudio-default"))))
})
