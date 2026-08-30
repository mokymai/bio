test_that("package version and installation helpers return expected results", {
  expect_identical(compare_version(c("1.0", "2.0"), c("1.1", "2.0")), c(-1, 0))
  expect_true(is_pkg_installed("utils"))
  expect_identical(is_pkg_installed(c("utils", "not-a-real-package")), c(TRUE, FALSE))

  installed <- get_pkgs_installed()
  expect_s3_class(installed, "data.frame")
  expect_named(installed, c("package", "current_version"))
  expect_true(all(c("utils", "stats") %in% installed$package))
})

test_that("package dependency helpers filter base packages and missing files", {
  dependency_data <- data.frame(
    Package = c("stats", "dplyr", "dplyr", "fs"),
    stringsAsFactors = FALSE
  )
  requested_paths <- NULL
  testthat::local_mocked_bindings(
    dependencies = function(path, ...) {
      requested_paths <<- path
      dependency_data
    },
    .package = "renv"
  )

  fixture_dir <- withr::local_tempdir()
  fixture_file <- fs::path(fixture_dir, "script.R")
  writeLines("stats::median(1:3)", fixture_file)

  expect_identical(list_pkgs_used_in_files("missing.R"), character())
  expect_identical(
    list_pkgs_used_in_files(c(fixture_file, "missing.R")),
    c("dplyr", "fs")
  )
  expect_identical(as.character(requested_paths), as.character(fixture_file))
  expect_identical(
    list_pkgs_used_in_files(fixture_file, exclude_base = FALSE),
    c("dplyr", "fs", "stats")
  )
  expect_identical(list_pkgs_used_in_dir(fixture_dir), c("dplyr", "fs"))
  expect_identical(requested_paths, fixture_dir)
})
