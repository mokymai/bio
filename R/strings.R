#' @name str_move
#' @title Change the position of a string in a vector
#' @description
#' Change the position of a string in a character vector.
#'
#' @param x (character vector)
#' @param what (string)
#' @param move_before (string)
#' @param move_after (sting)
#'
#' @noRd
#'
#' @examples
#' x <- c("a", "b", "d", "e", "c", "f")
#' str_move_before(x, what = "c", move_before = "d")
#'
#' x <- c("f", "c", "e", "d", "b", "a")
#' str_move_after(x, what = "c", move_after = "d")
#'
#' x <- c("a", "b", "d", "e", "c", "f")
#' str_move_before(x, what = "c", move_before = "d")
#'
#' x <- c("f", "c", "e", "d", "b", "a")
#' str_move_after(x, what = "c", move_after = "d")
NULL

# @rdname str_move
# @export
str_move_before <- function(x, what, move_before) {
  checkmate::assert_string(what)
  checkmate::assert_string(move_before)
  checkmate::assert_choice(what, x)
  checkmate::assert_choice(move_before, x)

  i <- seq_along(x)

  i_from <- i[x == what]         # larger number
  i_to   <- i[x == move_before]  # smaller number

  # If no need to re-arrenge
  if (i_from < i_to) {
    return(x)
  }

  i_new <- i[i > i_to & i <= i_from]

  i[i_new] <- i_new - 1
  i[i_to]  <- i_from

  x[i]
}


# @rdname str_move
# @export
str_move_after <- function(x, what, move_after) {
  checkmate::assert_string(what)
  checkmate::assert_string(move_after)
  checkmate::assert_choice(what, x)
  checkmate::assert_choice(move_after, x)

  i <- seq_along(x)

  i_from <- i[x == what]        # smaller number
  i_to   <- i[x == move_after]  # larger number

  # If no need to re-arrenge
  if (i_to < i_from) {
    return(x)
  }

  i_new <- i[i >= i_from & i < i_to]
  i[i_new] <- i_new + 1
  i[i_to]  <- i_from

  x[i]
}
