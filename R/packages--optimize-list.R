# Optimize order of packages to install ======================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Optimize order of packages to install
#'
#' Helper function that takes a vector of package names and suggests how to
#' sort the packages in order not to repeat installation of the same
#' package if it can be installed as a dependence of some other package.
#'
#'
#' @param pkgs_vec (character) Vector of package names.
#' @param recursive_dependencies (logical) If `TRUE`, recursive dependencies are
#'        also checked.
#'        **NOTE:** `recursive_dependencies = TRUE`requires internet connection.
#'
#' @return
#' A list with suggestion, which packages should be moved.
#'
#' @noRd
#'
#' @concept packages
#'
#' @examples
#' \dontrun{\donttest{
#' suggest_optimized_order_of_packages(c("stringr", "stringi", "glue", "readr"))
#' }}
suggest_optimized_order_of_packages <- function(pkgs_vec,
  recursive_dependencies = FALSE) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # library(tidyverse)

  list_after <- function(.which, list) {
    rev(rev(list)[1:which(rev(list) == .which)])
  }

  list_before <- function(.which, list) {
    list[1:which(list == .which)]
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  deps <-
    tools::package_dependencies(
      pkgs_vec,
      which   = c("Depends", "Imports"),
      reverse = FALSE,
      recursive = recursive_dependencies
    )

  rev_deps <-
    tools::package_dependencies(
      pkgs_vec,
      which   = c("Depends", "Imports"),
      reverse = TRUE,
      recursive = recursive_dependencies
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  move_after <-
    purrr::imap(
      deps,
      ~ stringr::str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])
    ) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move after): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")


  move_before <-
    purrr::imap(
      rev_deps,
      ~ stringr::str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])
    ) %>%
    purrr::discard(~length(.) == 0) %>%
    purrr::imap_chr(~stringr::str_c(stringr::str_pad(.y, 20), " (move before): ",
      stringr::str_c(.x, collapse = ", "))) %>%
    purrr::map_chr(structure, class = "glue") %>%
    unname() %>%
    structure(class = "glue")

  pkgs_base <- pkgs_vec[pkgs_vec %in% base_r_packages()]

  list(move_before = move_before, move_after = move_after, base_packages = pkgs_base)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIXME: this function does not work yet!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
      purrr::imap_dfr(~ tibble::tibble(
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
