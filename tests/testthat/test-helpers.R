test_that("argument formatting helpers preserve supported upgrade values", {
  expect_identical(str_to_quotes("ask"), '"ask"')
  expect_true(str_to_quotes(TRUE))
  expect_identical(chk_arg_upgrade("never"), '"never"')
  expect_false(chk_arg_upgrade(FALSE))
  expect_error(chk_arg_upgrade("sometimes"), "Must be element of set")

  expect_identical(get_upgrade_str("default"), "")
  expect_identical(get_upgrade_str("always"), ', upgrade = "always"')
  expect_identical(get_upgrade_str(FALSE), ", upgrade = FALSE")
})

test_that("str_glue_eval evaluates generated code in the requested environment", {
  target <- new.env(parent = baseenv())
  target$value <- 2

  result <- str_glue_eval(
    "value + {increment}",
    envir_glue = list2env(list(increment = 3), parent = emptyenv()),
    envir_eval = target
  )

  expect_identical(result, 5)
})

test_that("public dictionary installer aliases remain exported", {
  exports <- getNamespaceExports("bio")

  expect_true("rstudio_install_spellcheck_dictionaries" %in% exports)
  expect_true("rstudio_download_spellcheck_dictionaries" %in% exports)
  expect_false(".is_valid_dictionary_archive" %in% exports)
  expect_identical(
    rstudio_download_spellcheck_dictionaries,
    rstudio_install_spellcheck_dictionaries
  )
})
