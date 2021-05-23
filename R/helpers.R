# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str_to_quotes <- function(x) {
  if (is.character(x)) {
    x <- glue::glue('"{x}"')
  }
  x
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If x is string, quotes " are added on both sides of this string to work well
# with glue().
chk_arg_upgrade <- function(x) {
  checkmate::assert_choice(
    as.character(x),
    c(TRUE, "default", "ask", "always", "never", FALSE)
  )
  str_to_quotes(x)
}

get_upgrade_str <- function(upgrade) {
  if (upgrade == "default") {
    ""
  } else {
    paste0(", upgrade = ", chk_arg_upgrade(upgrade))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui_msg_restart_rstudio <- function() {
  usethis::ui_todo(paste0(
    "To take effect, {underline('RStudio')} should be ",
    "{underline('closed and reopened')}."
  ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# str_glue_eval ==============================================================
str_glue_eval <- function(..., envir = parent.frame(), .sep = "",
  .open = "{", .close = "}", envir_eval = envir,  envir_glue = envir) {

  commands_as_text <- glue::glue(...,
    .envir = envir_glue,
    .open  = .open,
    .close = .close
  )
  eval(parse(text = commands_as_text), envir = envir_eval)
}
