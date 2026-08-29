test_that("extract_proj_name works for mixed path styles and extensions", {
  proj_paths <- c(
    "list/proj.Rproj",
    "C:/R/BS-2020/bs.Rproj",
    "C:/data/analysis/proj.rproj",
    "C:/data/analysis/other-project.Rproj"
  )

  expect_identical(
    extract_proj_name(proj_paths),
    c("proj", "bs", "proj", "other-project")
  )
})

test_that("parse_proj_path records project metadata for existing files", {
  tmp_dir <- tempfile("bio-projects-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  proj_paths <- c(
    file.path(tmp_dir, "proj.Rproj"),
    file.path(tmp_dir, "bs.Rproj"),
    file.path(tmp_dir, "other-project.Rproj")
  )
  for (path in proj_paths) {
    writeLines("Project", path)
  }

  result <- parse_proj_path(proj_paths)

  expect_s3_class(result, "tbl_df")
  expect_identical(result$name, c("proj", "bs", "other-project"))
  expect_identical(result$path, proj_paths)
  expect_identical(unname(result$exists), rep(TRUE, length(proj_paths)))
  expect_identical(unname(result$dir_exists), rep(TRUE, length(proj_paths)))
})

test_that("read_projects reads project list files and sorts by name or path", {
  tmp_dir <- tempfile("bio-projects-")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_file <- file.path(tmp_dir, "projects.txt")

  project_paths <- c(
    file.path(tmp_dir, "zeta", "zeta.Rproj"),
    file.path(tmp_dir, "alpha", "alpha.Rproj"),
    file.path(tmp_dir, "beta", "beta.Rproj")
  )

  for (path in project_paths) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("Project", path)
  }

  writeLines(project_paths, tmp_file)

  result_name <- read_projects(tmp_file, sort_by = "name")
  expect_identical(result_name$name, c("alpha", "beta", "zeta"))

  result_path <- read_projects(tmp_file, sort_by = "path")
  expect_identical(result_path$path, project_paths[order(project_paths)])
})

test_that("read_projects warns and returns NULL for missing files", {
  missing_file <- tempfile(pattern = "no-projects-", fileext = ".txt")
  expect_warning(res <- read_projects(missing_file), "File was not found")
  expect_null(res)
})

test_that("open_project() accepts a supplied project table", {
  projects <- tibble::tibble(
    name = "demo",
    path = "demo.Rproj",
    exists = TRUE,
    dir_exists = TRUE
  )
  opened_path <- NULL
  opened_new_session <- NULL
  testthat::local_mocked_bindings(
    openProject = function(path, newSession) {
      opened_path <<- path
      opened_new_session <<- newSession
      invisible(NULL)
    },
    .package = "rstudioapi"
  )

  expect_invisible(open_project(proj_list = projects, name = "demo"))
  expect_identical(opened_path, "demo.Rproj")
  expect_identical(opened_new_session, TRUE)
})

test_that("make_unique_obj_names keeps duplicates unique relative to existing choices", {
  choices <- c("existing", "x", "x_1")

  expect_identical(
    make_unique_obj_names(c("x", "x"), list_of_choices = choices, all_numbered = FALSE),
    c("x_2", "x_3")
  )
})
