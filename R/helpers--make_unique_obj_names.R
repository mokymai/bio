make_unique_obj_names <- function(names, prefix = "", suffix = "",
                                  list_of_choices = objects(all.names = TRUE,
                                                            envir = .GlobalEnv),
                                  all_numbered = TRUE) {
  if (length(names) == 0) {
    return(NULL)
  }
  initial_names <- glue::glue("{prefix}{names}{suffix}")
  n_names <- length(names)

  list_to_check <-
    if (all_numbered) {
      c(list_of_choices, initial_names, initial_names)
    } else {
      c(list_of_choices, initial_names)
    }

  list_to_check %>%
    make.unique(sep = "_") %>%
    rev() %>%
    .[1:n_names] %>%
    rev()
}
