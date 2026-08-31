# Grammar, Spelling, Dictionaries ============================================

# Dictionaries:
# https://github.com/wooorm/dictionaries

# Language spelling checking tools
# https://github.com/nevrome/wellspell.addin
# https://www.languagetool.org/download/

# LanguageToolR::lato_quick_setup()

# https://support.posit.co/hc/en-us/articles/200551916-Spelling-Dictionaries



#' @name RStudio-dictionaries
#' @title RStudio Dictionaries
#' @description
#' Functions to work with RStudio dictionaries.
#'
#' @return String with path.
# @export
#'
#' @concept paths and dirs
#' @concept dictionaries
#'
#' @seealso [rstudioapi::dictionaries]
#' @examples
#' if (interactive()) {
#'   rstudioapi::dictionariesPath()
#'   rstudioapi::userDictionariesPath()
#'   get_path_rstudio_config_dir("dictionaries")
#' }
NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::dictionariesPath()]
#' - `open_rstudio_system_dictionaries_dir()`
open_rstudio_system_dictionaries_dir <- function() {
  # "C:/Users/User/AppData/Roaming/RStudio/dictionaries/languages-system"
  get_path_rstudio_config_dir("dictionaries/languages-system") |>
    fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::userDictionariesPath()]
#' - `open_rstudio_user_dictionaries_dir()`
open_rstudio_user_dictionaries_dir <- function() {
  # "C:/Users/User/AppData/Local/RStudio/dictionaries"
  rstudioapi::userDictionariesPath() |> fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::dictionariesPath()]
#' - `open_rstudio_internal_dictionaries_dir()`
open_rstudio_internal_dictionaries_dir <- function() {
  # "C:/Program Files/RStudio/resources/dictionaries"
  rstudioapi::dictionariesPath() |> fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' @name spelling
#' @title Dictionaries to check spelling
#' @description
#' - `rstudio_download_spellcheck_dictionaries()`
#'  downloads and updates RStudio (system) spellchecking dictionaries.
#' - `rstudio_delete_spellcheck_dictionaries()`
#'  deletes RStudio (system) spellchecking dictionaries.
#'
#' @param secure (logical) If `TRUE`, uses "https", if `FALSE`, uses "http".
#' @return Invisibly, `TRUE` on success and `FALSE` on failure.
#'
#' @concept r and rstudio settings
#' @concept dictionaries
#'
#' @examples
#' if (interactive()) {
#'   rstudio_delete_spellcheck_dictionaries()
#'   rstudio_download_spellcheck_dictionaries()
#' }
NULL

.is_valid_dictionary_archive <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) {
    return(FALSE)
  }

  contents <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(e) NULL
  )

  if (is.null(contents) || !"Name" %in% names(contents)) {
    return(FALSE)
  }

  expected_files <- c("lt_LT.aff", "lt_LT.dic")
  all(expected_files %in% basename(contents$Name))
}

.download_dictionary_archive_with_curl <- function(url, destfile) {
  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    return(FALSE)
  }

  status <- tryCatch(
    system2(
      curl,
      args = c(
        "--fail", "--location", "--retry", "3", "--retry-all-errors",
        "--connect-timeout", "15", "--output", shQuote(destfile), shQuote(url)
      ),
      stdout = FALSE,
      stderr = FALSE
    ),
    error = function(e) 1L
  )

  isTRUE(status == 0L) && .is_valid_dictionary_archive(destfile)
}

.download_dictionary_archive <- function(url, destfile, attempts = 3L) {
  for (attempt in seq_len(attempts)) {
    unlink(destfile)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        quiet = TRUE,
        method = "libcurl"
      )),
      error = function(e) 1L
    )

    if (isTRUE(status == 0L) && .is_valid_dictionary_archive(destfile)) {
      return(TRUE)
    }
  }

  unlink(destfile)
  .download_dictionary_archive_with_curl(url, destfile)
}

#' @rdname spelling
#' @export
rstudio_install_spellcheck_dictionaries <- function(secure = TRUE) {
  dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")
  usethis::ui_info("Downloading RStudio spellcheck dictionaries...")

  if (rstudioapi::isAvailable(version_needed = "1.3") &&
    exists(".rs.downloadAllDictionaries", envir = globalenv(), inherits = TRUE)) {
    .rs.downloadAllDictionaries(targetDir = dic_dir, secure = secure)
    usethis::ui_done("RStudio spellcheck dictionaries were installed in {usethis::ui_path(dic_dir)}.")
    return(invisible(TRUE))
  }

  # Headless fallback: execute RStudio's exact download-and-extract workflow directly
  protocol <- if (isTRUE(secure)) "https" else "http"
  url <- sprintf("%s://s3.amazonaws.com/rstudio-buildtools/dictionaries/all-dictionaries.zip", protocol)

  archive_path <- tempfile("all-dictionaries-", fileext = ".zip")
  on.exit(unlink(archive_path), add = TRUE)

  if (!.download_dictionary_archive(url, archive_path)) {
    usethis::ui_warn("Could not download a complete RStudio dictionary archive.")
    return(invisible(FALSE))
  }

  fs::dir_create(dic_dir, recurse = TRUE)
  unzip_res <- tryCatch(
    {
      utils::unzip(archive_path, exdir = dic_dir)
      TRUE
    },
    error = function(e) FALSE
  )

  if (isTRUE(unzip_res)) {
    usethis::ui_done("RStudio spellcheck dictionaries were installed in {usethis::ui_path(dic_dir)}.")
  } else {
    usethis::ui_warn("Could not extract the RStudio dictionary archive.")
  }

  invisible(isTRUE(unzip_res))
}
#' @rdname spelling
#' @export
rstudio_download_spellcheck_dictionaries <- rstudio_install_spellcheck_dictionaries

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name spelling
#' @param ask (logical) If `TRUE`, user will have to confirm his/her choice
#'        interactively.
#' @export
#' @concept r and rstudio settings
#' @concept dictionaries
rstudio_delete_spellcheck_dictionaries <- function(ask = TRUE) {
  dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")
  ans <- FALSE

  if (isTRUE(ask)) {
    ans <- usethis::ui_nope(
      "Do you really want to delete dictionaries in \n{ui_value(dic_dir)}?"
    )
  }
  if (ans) {
    usethis::ui_warn("Canceled (no dictionaries were deleted)")
    return(invisible())
  }

  if (fs::dir_exists(dic_dir)) {
    fs::dir_delete(dic_dir)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
