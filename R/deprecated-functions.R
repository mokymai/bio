#    #' @name deprecated-functions
#    #' @title Deprecated functions
#    #' @description Functions that are no longer used in package \pkg{bio}.
#    #' In the next release of the package, these functions will be removed.
#    #' @param ... Arguments to further methods.
#    NULL
#
#    #' @rdname deprecated-functions
#    # @export
#    set_rstudio_keybindings <- function(...) {
#      .Deprecated("reset_rstudio_keybindings()")
#      reset_rstudio_keybindings(...)
#    }
#
#    #' @rdname deprecated-functions
#    # @export
#    update_bio <- function(...) {
#      .Deprecated("update_pkg_bio()")
#      update_pkg_bio(...)
#    }
#
#    #' @rdname deprecated-functions
#    # @export
#    restart_rstudio <- function() {
#      .Deprecated("reload_rstudio()")
#      reload_rstudio()
#    }
