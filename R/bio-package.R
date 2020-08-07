#' @keywords internal
#' @import backup.tools
#' @importFrom utils browseURL download.file installed.packages loadhistory
#'                   read.table savehistory unzip
#' @importFrom magrittr "%>%"
#' @importFrom crayon bold underline red green blue cyan yellow magenta silver
#' @importFrom usethis ui_done ui_todo ui_oops ui_info ui_code ui_field ui_path
#'                     ui_stop ui_warn ui_value ui_line
#' @importFrom stats setNames
#' @importFrom utils available.packages packageVersion sessionInfo
#'

"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Internal bio environment
bio_envir <- new.env()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.onLoad <- function(libname, pkgname) {
  # Autocompletions
  completeme::register_completion(bio = auto_completions_bio)
}

auto_completions_bio <- function(env) {

  funs_pkg_lists <- c(
    "check_packages_by_topic",
    "get_pkgs_installation_status",
    "get_path_pkgs_recommended",
    "get_pkgs_installation_status_local"
  )

  funs <- c(
    "reset_rstudio_keybindings",
    "reset_rstudio_user_settings",
    funs_pkg_lists
  )

  if (
    (!completeme::is_first_argument(env)) ||
      (!completeme::current_function(env) %in% funs)
  ) {
    return(NULL)
  }

  if (completeme::current_function(env) %in% "reset_rstudio_keybindings") {
    return(paste0('"', keybindings_defaults, '"'))
  }

  if (completeme::current_function(env) %in% "reset_rstudio_user_settings") {
    return(paste0('"', user_settings_defaults, '"'))
  }

  if (completeme::current_function(env) %in% funs_pkg_lists) {
    return(paste0('"', bio::get_pkg_lists_local(), '"'))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
globalVariables(c(
  ".", ".rs.downloadAllDictionaries", "bowse_meld_homepage", "cran_version",
  "current_version", "deps_below", "get_path_rs_desktop_dir", "install_from",
  "n_deps", "name", "on_cran", "package", "panderOptions", "pandoc.header",
  "pandoc.list", "pkg", "value"
))
