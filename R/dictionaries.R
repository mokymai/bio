# Grammar, Spelling, Dictionaries ============================================


# Dictionaries:
# https://github.com/wooorm/dictionaries
#

# Language spelling checking tools
# https://github.com/nevrome/wellspell.addin
# https://www.languagetool.org/download/


# LanguageToolR::lato_quick_setup()
#' @name RStudio-dictionaries
#' @title RStudio dictionaries
#' @description
#' Functions to work with RStudio dictionaries.
#'
#' @return String with path.
#' @export
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
#' get_path_rs_system_dictionaries_dir()
#'
#' get_path_rstudio_config_dir("dictionaries")
#'
#' get_path_saved_words_dictionary()
#'
#' }}
get_path_saved_words_dictionary <- function() {
  # Path to dictionary of words saved by user
  get_path_rstudio_config_dir("monitored/lists/user_dictionary")
}

#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - `get_path_rs_saved_words_dictionary()` -- Path to dictionary of words
#'    saved by user
#' - `open_rs_saved_words_dictionary()`
get_path_rs_saved_words_dictionary <- function() {
  get_path_rstudio_config_dir("monitored/lists/user_dictionary")
}

#' @rdname RStudio-dictionaries
#' @export
open_rs_saved_words_dictionary <- function() {
  get_path_rs_saved_words_dictionary() %>% open_in_rstudio()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @param rstudio_version Numeric version of RStudio (the output is different
#'        for RStudio < 1.3 and 1.3 or newer series). If `"auto"`, the version
#'        of RStudio will be determined automatically.
#' @export
#' @description
#' - `get_path_rs_system_dictionaries_dir()`
#' - `open_rs_system_dictionaries_dir()`
get_path_rs_system_dictionaries_dir <- function(rstudio_version = "auto") {

  if (resolve_rs_version(rstudio_version) > "1.3") {
    get_path_user_settings_dir_rs_1.3("dictionaries", "languages-system")

  } else {
    fs::path(rstudioapi::userDictionariesPath(), "languages-system")
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
open_rs_system_dictionaries_dir <- function() {
  get_path_rs_system_dictionaries_dir() %>% fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::dictionariesPath()]
#' - `open_rs_dictionaries_dir()`
open_rs_dictionaries_dir <- function() {
  # "C:/Program Files/RStudio/resources/dictionaries"
  rstudioapi::dictionariesPath() %>% fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname RStudio-dictionaries
#' @export
#' @description
#' - [rstudioapi::userDictionariesPath()]
#' - `open_user_dictionaries_dir()`
open_user_dictionaries_dir <- function() {
  # "C:/Users/User/AppData/Local/RStudio-Desktop/dictionaries"
  # "C:/Users/User/AppData/Local/RStudio/dictionaries"
  rstudioapi::userDictionariesPath() %>% fs::file_show()
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' @name spelling
#' @title Dictionaries to check spelling
#' @description
#' `rstudio_download_spellcheck_dictionaries()`
#'  downloads and updates RStudio (system) spellchecking dictionaries.
#' `rstudio_delete_spellcheck_dictionaries()`
#' deletes RStudio (system) spellchecking dictionaries.
#'
#' @param rstudio_version Numeric version of RStudio (the output is different
#'        for RStudio < 1.3 and 1.3 or newer series). If `"auto"`, the version
#'        of RStudio will be determined automatically.
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
#' }}

rstudio_download_spellcheck_dictionaries <- function(rstudio_version = "auto", secure = TRUE) {
  if (rstudioapi::isAvailable()) {
    # if (rstudioapi::isAvailable() && rstudioapi::versionInfo()$version > "1.3") {
    #   warning("This function does not work in RStudio 1.3.0 or newer yet.\n")
    #   return(FALSE)
    # }
    dic_dir <- get_path_rs_system_dictionaries_dir(rstudio_version = rstudio_version)
    .rs.downloadAllDictionaries(targetDir = dic_dir, secure = secure)

  } else {
    FALSE
  }
}

#' @name spelling
#' @export
#' @concept r and rstudio settings
#' @concept dictionaries
rstudio_delete_spellcheck_dictionaries <- function() {
  # FIXME: Ask user permission to prevent accidental deletion.
  fs::dir_delete(get_path_rs_system_dictionaries_dir())
}

# ==========================================================================~~

# # @importFrom rstudioapi selectDirectory
# # @export
# #  @concept dictionaries
# rstudioapi::selectDirectory
