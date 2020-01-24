# pander::panderOptions <- function(o, value, ...) {

#' Get or set Pander options.
#'
#' @param o (string) option name.
#' @param value value to assign (optional)
#' @param ... valid option - value pairs (e.g., `option = "value"`).
#' @seealso [pander::evalsOptions()]
#'
#' @export
#'
#' @examples
#' p_opts <- pander_options()
#' head(p_opts, n = 2)
#'
#' pander_options("digits")
#'
#' pander_options("digits", 5)
#' pander_options("digits")
#'
#' pander_options(digits = 2)
#' pander_options("digits")
#'
pander_options <- function(o, value, ...) {

  res <- pander::panderOptions()
  # res2 <- getOption("pander")
    available_pander_options <- function() {
    pander::pandoc.header("Possible `pander` options:", style = "setext")
    pander::pandoc.list(sort(names(res)))
  }

  opts_list <- list(...)

  # If option = value pairs are present
  if (length(opts_list) > 0) {
    if (!missing(o) && !missing(value)) {
      stop("Neither 'o' nor 'value' should be provided with 'option = value' pairs.")
    }

    opt_names  <- names(opts_list)
    wrong_name <- !opt_names %in% names(res)
    # If otion names are misspelled
    if (any(wrong_name)) {
      available_pander_options()
      stop(
        "The following options are either not present in pander or are misspelled: \n",
        paste(opt_names[wrong_name], collapse = ", ")
      )
    }
    # Set options
    lapply(seq_along(opts_list), function(i) {
      pander::panderOptions(opt_names[i], opts_list[[i]])
    })
    return(invisible())
  }
  pander::panderOptions(o, value)
}



panderOptions2 <- function(o, value, ...) {

  res <- getOption("pander")

  available_pander_options <- function() {
        pandoc.header("Possible `pander` options:", style = "setext")
        pandoc.list(sort(names(res)))
  }

  opts_list <- list(...)
  # If option = value pairs are present
  if (length(opts_list) > 0) {
    if (!missing(o) && !missing(value)) {
       stop("Neither 'o' nor 'value' should be provided with 'option = value' pairs.")
    }

    opt_names  <- names(opts_list)
    wrong_name <- !opt_names %in% names(res)
    # If otion names are misspelled
    if (any(wrong_name)) {
      available_pander_options()
      stop(
        "The following options are either not present in pander or are misspelled: \n",
        paste(opt_names[wrong_name], collapse = ", ")
      )
    }
    # Set options
    lapply(seq_along(opts_list), function(i) {
      panderOptions(opt_names[i], opts_list[[i]])
    })
    return(invisible())
  }

    if (missing(value)) {
        if (missing(o))
            return(res)
        if (o %in% names(res))
            return(res[[o]])
        available_pander_options()
        stop("Wrong option queried.")
    }
    else {
        if (!o %in% names(res))
            stop(paste("Invalid option name:", o))
        if (is.null(value)) {
            res[o] <- list(NULL)
        }
        else {
            res[[o]] <- value
        }
        options(pander = res)
    }
}
