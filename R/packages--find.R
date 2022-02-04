# List packages --------------------------------------------------------------

#' List Packages Used in File(s)
#'
#' These functions search for patterns, such as `library(pkg)`, `require(pkg)`,
#' `pkg::function()`, or `data(package = "pkg")` and return package names from
#' these expressions.
#'
#' @param path Path to directory with R and R Markdown files. Defaults to `"."`.
#' @param files Path to R or Rmd files
#' @param regexp Regular expression to filter file names. Defaults to R and Rmd
#'        files.
#' @param ... Further arguments to [fs::dir_ls].
#'
#' @concept utilities
#'
#' @return Character vector with package names.
#' @export
list_pkgs_used_in_dir <- function(path = ".", regexp = "(?i)[.](rmd|r)$", ...) {
  path %>%
    fs::dir_ls(regexp = regexp, ...) %>%
    list_pkgs_used_in_files()
}

list_pkgs_used_in_dir_code <- function(path = ".", regexp = "(?i)[.](rmd|r)$", ...) {
  path %>%
    fs::dir_ls(regexp = regexp, ...) %>%
    list_pkgs_used_in_files_code()
}

#' @rdname list_pkgs_used_in_dir
#' @export
# NOTE: should work in all these situations:
# library("pkg1")
# library('pkg2')
# library(pkg3)
# require(pkg4)
# require2(pkg5)
# library(package = "pkg6")
# library(package = 'pkg7')
# library(package = pkg8)
# library(package=pkg9)
# library(package   =    pkg10)
# library(package = pkg11, pos = 2)
# library(package = 'pkg12', pos = 2)
# library(package = "pkg13", pos = 2)
# data("xxxx", package = "pkg14")
# data("xxxx", package = 'pkg15')
# pkg16::data
# pkg17:::data
# pkg18::fun()
# pkg19:::fun()
# (pkg20::fun())
# {pkg21::fun()}
# pkg22 :: fun()
#
# TODO: ignore code in comments
list_pkgs_used_in_files <- function(files) {
  files %>%
    purrr::map(~readr::read_file(.)) %>%
    stringr::str_extract_all(
      paste0(
        "(?<=(library|require|require2)\\()(.*?)(?=\\))|", # library(dplyr)
        "(?<=\\s|\\n|\\(|\\{|\\[)[a-zA-Z0-9.]*?( )*(?=:{2,3})|", # dplyr::select
        "package\\s*=\\s*(\"|')[a-zA-Z0-9.]*?(\"|')" # data(package = "dplyr")
      )
    ) %>%
    purrr::reduce(c) %>%
    stringr::str_remove_all("\'|\"|package\\s*=\\s*|,\\s*pos\\s*=\\s.*") %>%
    stringr::str_trim() %>%
    stringr::str_subset("^$", negate = TRUE) %>%
    unique()
}

list_pkgs_used_in_files_code <- function(files) {
  files %>%
    purrr::map(~readr::read_file(.)) %>%
    stringr::str_extract_all(
      paste0(
        "(library|require|require2)\\((.*?)\\)|",   # library(dplyr)
        "(\\s|\\n|\\(|\\{|\\[)[a-zA-Z0-9.]*?( )*:{2,3}[a-zA-Z0-9._]*|", # dplyr::select
        "package\\s*=\\s*(\"|')[a-zA-Z0-9.]*?(\"|')" # data(package = "dplyr")
      )
    ) %>%
    purrr::reduce(c) %>%
    unique()
}

