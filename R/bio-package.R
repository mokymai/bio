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
tmp_GITHUB_PAT <- "d1d1a11383f1d5fd01427008cd8967ae7698391f"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.onLoad <- function(libname, pkgname) {
  pat <- Sys.getenv("GITHUB_PAT", NA_character_)

  # Autocompletions
  completeme::register_completion(bio = auto_completions_bio)

  # Other
  try({
      if (is.na(pat) && pingr::my_ip(method = "https") %in% ip_ec_108)
        Sys.setenv(GITHUB_PAT = tmp_GITHUB_PAT)
  }, silent = TRUE)
}

auto_completions_bio <- function(env) {

  funs_pkg_lists <- c(
    "check_installed_packages",
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
