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
#'
#' @export
#' @concept r and rstudio settings
#' @concept dictionaries
#'
#' @examples
#' if (interactive()) {
#'   rstudio_delete_spellcheck_dictionaries()
#'   rstudio_download_spellcheck_dictionaries()
#' }
rstudio_install_spellcheck_dictionaries <- function(secure = TRUE) {
  dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")

  if (rstudioapi::isAvailable(version_needed = "1.3") &&
    exists(".rs.downloadAllDictionaries", envir = globalenv(), inherits = TRUE)) {
    .rs.downloadAllDictionaries(targetDir = dic_dir, secure = secure)
    return(TRUE)
  }

  # Headless fallback: execute RStudio's exact download-and-extract workflow directly
  protocol <- if (isTRUE(secure)) "https" else "http"
  url <- sprintf("%s://s3.amazonaws.com/rstudio-buildtools/dictionaries/all-dictionaries.zip", protocol)

  archive_path <- tempfile("all-dictionaries-", fileext = ".zip")
  on.exit(unlink(archive_path), add = TRUE)

  dl_status <- tryCatch(
    {
      utils::download.file(url = url, destfile = archive_path, mode = "wb", quiet = TRUE)
      0L
    },
    error = function(e) 1L
  )

  if (dl_status != 0L || !file.exists(archive_path)) {
    return(FALSE)
  }

  fs::dir_create(dic_dir, recurse = TRUE)
  unzip_res <- tryCatch(
    {
      utils::unzip(archive_path, exdir = dic_dir)
      TRUE
    },
    error = function(e) FALSE
  )

  isTRUE(unzip_res)
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
