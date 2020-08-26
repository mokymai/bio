#' @keywords internal

"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import backup.tools
#' @importFrom utils browseURL download.file installed.packages loadhistory
#'                   read.table savehistory unzip
#' @importFrom magrittr "%>%"
#' @importFrom crayon bold underline red green blue cyan yellow magenta silver
#' @importFrom usethis ui_done ui_todo ui_oops ui_info ui_code ui_field ui_path
#'                     ui_stop ui_warn ui_value ui_line
#' @importFrom stats setNames
#' @importFrom utils available.packages packageVersion sessionInfo
## usethis namespace: end
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Internal bio environment
bio_envir <- new.env()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
globalVariables(c(
  ".", ".rs.downloadAllDictionaries", "bowse_meld_homepage", "cran_version",
  "current_version", "deps_below", "install_from",
  "n_deps", "name", "on_cran", "package", "panderOptions", "pandoc.header",
  "pandoc.list", "pkg", "value"
))
