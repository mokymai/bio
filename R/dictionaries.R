# Grammar, Spelling, Dictionaries ============================================

# TODO: rstudio_download_spellcheck_dictionary_lt

# Dictionaries:
# https://github.com/wooorm/dictionaries
#

# Language spelling checking tools
# https://github.com/nevrome/wellspell.addin
# https://www.languagetool.org/download/

# LanguageToolR::lato_quick_setup()

# https://support.rstudio.com/hc/en-us/articles/200551916-Spelling-Dictionaries



#' @name RStudio-dictionaries
#' @title RStudio dictionaries
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
#' - `open_rsstudio_system_dictionaries_dir()`
open_rstudio_system_dictionaries_dir <- function() {
  # "C:/Program Files/RStudio/resources/dictionaries"
  rstudioapi::dictionariesPath() %>% fs::file_show()
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


#' @name spelling
#' @title Dictionaries to check spelling
#' @description
#' `rstudio_download_spellcheck_dictionaries()`
#'  downloads and updates RStudio (system) spellchecking dictionaries.
#' `rstudio_delete_spellcheck_dictionaries()`
#' deletes RStudio (system) spellchecking dictionaries.
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
#' }}

rstudio_download_spellcheck_dictionaries <- function(secure = TRUE) {

  if (rstudioapi::isAvailable(version_needed = 1.3)) {
    dic_dir <- rstudioapi::userDictionariesPath()
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
  fs::dir_delete(rstudioapi::userDictionariesPath())
}

# ==========================================================================~~

# # @importFrom rstudioapi selectDirectory
# # @export
# #  @concept dictionaries
# rstudioapi::selectDirectory
