#' optimize_order_to_install
#'
#' Helpeer function that suggest how to optimize order of packages in the
#' vector of packages in order not to repeat installation of
#' the same packages.
#'
#' recursive_dependencies = TRUE requires internet connection.
#'
#' @param pkgs_vec ???
#' @param recursive_dependencies (logical) ???
#'
#' @return ???
#'
#' @noRd
#'
#' @examples
#'
#' \dontrun{\donttest{
#'
#' }}
optimize_order_to_install <- function(pkgs_vec,
  recursive_dependencies = TRUE) {

  stop("This function is not implemented yet!!!")

  pkgs_vec_orig <- pkgs_vec
  # pkgs_vec <- pkgs_vec_orig
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # library(tidyverse)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list_after <- function(.which, list) {
    rev(rev(list)[0:(which(rev(list) == .which) - 1)])
  }
  # list_before <- function(.which, list) {
  #     list[(0:(which(list == .which) - 1))[-1]]
  # }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  str_move_before <- function(x, .what, .move_before) {
    checkmate::assert_string(.what)
    checkmate::assert_string(.move_before)
    checkmate::assert_choice(.what, x)
    checkmate::assert_choice(.move_before, x)

    i <- seq_along(x)

    i_from <- i[x == .what]         # larger number
    i_to   <- i[x == .move_before]  # smaller number

    # If no need to re-arrenge
    if (i_from < i_to) {
      return(x)
    }

    i_new <- i[i > i_to & i <= i_from]
    i[i_new] <- i_new - 1
    i[i_to]  <- i_from

    x[i]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  str_move_after <- function(x, .what, .move_after) {
    checkmate::assert_string(.what)
    checkmate::assert_string(.move_after)
    checkmate::assert_choice(.what, x)
    checkmate::assert_choice(.move_after, x)

    i <- seq_along(x)

    i_from <- i[x == .what]        # smaller number
    i_to   <- i[x == .move_after]  # larger number

    # If no need to re-arrange
    if (i_to < i_from) {
      return(x)
    }

    i_new <- i[i >= i_from & i < i_to]
    i[i_new] <- i_new + 1
    i[i_to]  <- i_from

    x[i]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # deps <-
  #     tools::package_dependencies(
  #         pkgs_vec,
  #         reverse = FALSE,
  #         which = "all",
  #         recursive = recursive_dependencies
  #     )

  # pkgs_vec <- x

  base_pkgs <- c("R", rownames(installed.packages(priority = "base")))

  get_deps <- function(x) {
    # NOTE: all packages must be installed
    ind <- is_pkg_installed(x)

    if (any(!ind)) {
      warning("These package are not installed and were removed from the list:\n",
        paste(x[!ind], sep = ", "))
    }

    x <- x[ind]

    x %>%
      purrr::map(
        ~ system.file(package = ., "DESCRIPTION") %>%
          desc::desc_get_deps() %>%
          dplyr::pull(package) %>%
          setdiff(base_pkgs)
      ) %>%
      purrr::set_names(x) %>%
      # imap(~ str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>%
      purrr::imap(~ stringr::str_c(.x[.x %in% x])) %>%
      purrr::imap_dfr(~tibble::tibble(
        pkg = .y,
        deps = list(.x),
        n_deps = length(.x)
      )) %>%
      dplyr::arrange(n_deps != 0) %>%
      dplyr::select(-n_deps)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  deps <- get_deps(x = pkgs_vec)

  # TODO: Does not work from here

  # x <- deps
  get_n_deps_below <- function(x) {
    lst <- x$pkg
    xx <-
      x %>%
      dplyr::mutate(
        deps_below = purrr::map2(deps, pkg,
          ~ stringr::str_c(.x[.x %in% list_after(.y, list = lst)])
        ),
        n_deps = purrr::map_dbl(deps_below, length),
        last_dep = purrr::map_dbl(deps_below, ~max(which(lst %in% .)))
      ) %>%
      dplyr::arrange(n_deps != 0)

    lst2 <- xx$pkg
    xxx <-
      xx %>%
      dplyr::mutate()
  }



  if (nrow(deps) > 0) {
    x <- pkgs_vec

    # tbl <- tibble()

    for (i in 1:nrow(deps)) {
      # x_old <- x
      x <- str_move_before(x, .what = deps$what[i], .move_before = deps$move_before[i])
      # tbl <- bind_rows(
      # tbl,
      # tibble(i = i,
      #        identical = identical(x_old, x),
      #        what = obj$what[i],
      #        move_before = obj$move_before[i],
      #        x_old = list(x_old))
      # )
    }
    return(x)

  } else {
    return(pkgs_vec)
  }

  # Test output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # all(x %in% pkgs_vec)
  # all(pkgs_vec %in% x)
  # all.equal(pkgs_vec, x)
  # tibble(before = sort(pkgs_vec), after = sort(x), match = before == after) %>%
  #     print(n = Inf)
  # tibble(before = pkgs_vec, after = x, match = before == after) %>%
  #     print(n = Inf)
  #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # move_after <-
  #     imap(deps, ~ str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>%
  #     discard(~length(.) == 0) %>%
  #     imap_dfr(~tibble(what = .y, move_after = str_c(.x, collapse = ","))) %>%
  #     separate_rows(move_after)

  # move_before <-
  #     imap(rev_deps, ~ str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])) %>%
  #     discard(~length(.) == 0) %>%
  #     imap_dfr(~tibble(what = .y, move_before = str_c(.x, collapse = ","))) %>%
  #     separate_rows(move_before)

  # list(move_before = move_before, move_after = move_after)
}
