
get_released_r_version <- function(force = FALSE) {
  if (!force && pingr::is_online()) {
    c(
      "https://cran.r-project.org/src/base/R-3",
      "https://cran.r-project.org/src/base/R-4"
    ) %>%
      purrr::map(read_lines) %>%
      purrr::reduce(c) %>%
      stringr::str_extract("(?<=R-).\\d*[.].\\d*[.]\\d*(?=.tar.gz)") %>%
      tidyr::replace_na(0) %>%
      numeric_version() %>%
      max()

  } else {
    usethis::ui_stop("You are off-line but network connection is required.")
  }

}
