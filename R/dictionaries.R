# Grammar, Spelling, Dictionaries ============================================

# Dictionaries:
# https://github.com/wooorm/dictionaries

# Language spelling checking tools
# https://github.com/nevrome/wellspell.addin
# https://www.languagetool.org/download/

# LanguageToolR::lato_quick_setup()

# https://support.rstudio.com/hc/en-us/articles/200551916-Spelling-Dictionaries



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
#' \dontrun{\donttest{
#'
#' rstudioapi::dictionariesPath()
#'
#' rstudioapi::userDictionariesPath()
#'
#' get_path_rstudio_config_dir("dictionaries")
#'
#' }}

NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::dictionariesPath()]
#' - `open_rstudio_system_dictionaries_dir()`
open_rstudio_system_dictionaries_dir <- function() {
  # "C:/Users/User/AppData/Roaming/RStudio/dictionaries/languages-system"
  get_path_rstudio_config_dir("dictionaries/languages-system") %>%
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
  rstudioapi::userDictionariesPath() %>% fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::dictionariesPath()]
#' - `open_rstudio_internal_dictionaries_dir()`
open_rstudio_internal_dictionaries_dir <- function() {
  # "C:/Program Files/RStudio/resources/dictionaries"
  rstudioapi::dictionariesPath() %>% fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' @name spelling
#' @title Dictionaries to check spelling
#' @description
#' - `rstudio_download_spellcheck_dictionaries()`
#'  downloads and updates RStudio (system) spellchecking dictionaries.
#' - `rstudio_download_spellcheck_dictionaries_lt()`
#'  installs improved Lithuanian spellchecking dictionary.
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
#' \dontrun{\donttest{
#' rstudio_delete_spellcheck_dictionaries()
#' rstudio_download_spellcheck_dictionaries()
#' rstudio_install_spellcheck_dictionary_lt()
#' }}

rstudio_download_spellcheck_dictionaries <- function(secure = TRUE) {

  if (rstudioapi::isAvailable(version_needed = 1.3)) {
    dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")
    .rs.downloadAllDictionaries(targetDir = dic_dir, secure = secure)
  } else {
    FALSE
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name spelling
#' @export
#' @concept r and rstudio settings
#' @concept dictionaries
rstudio_install_spellcheck_dictionary_lt <- function() {

  tryCatch({
    dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")
    zipped_dict <- system.file("dictionaries/lt_LT.zip", package = "bio")
    dic_paths <- unzip(zipped_dict, exdir = dic_dir)
    usethis::ui_done(paste0(
      "lt_LT dictionary installed. \n",
      "{usethis::ui_path(dic_paths[2])}"
    ))

    invisible(dic_paths)

  }, error = function(e) {
    usethis::ui_oops("Fail to install lt_LT dictionary. ")
    print(e)
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name spelling
#' @param ask (logical) If `TRUE`, user will have to confirm his/her choice
#'        interactively.
#' @export
#' @concept r and rstudio settings
#' @concept dictionaries
rstudio_delete_spellcheck_dictionaries <- function(ask = TRUE) {
  dic_dir <- get_path_rstudio_config_dir("dictionaries/languages-system")

  if (isTRUE(ask)) {
    ans <- usethis::ui_nope(
      "Do you really want to delete dictionaries in \n{ui_value(dic_dir)}?"
    )
    # ans <- usethis::ui_nope("...", yes = "Yes")
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
