
load_rcmdr_biostat <- function() {

  if ("RcmdrPlugin.biostat" %in% .packages(all.available = TRUE)) {

    rstudioapi::sendToConsole(
      paste(sep = "\n",
        "library(RcmdrPlugin.biostat)",
        "RcmdrPlugin.biostat::restart_commander()",
        "RcmdrPlugin.biostat::set_biostat_mode()")
    )

  } else {
    cat("\n")
    usethis::ui_todo("Package {crayon::green('RcmdrPlugin.biostat')} is not installed.")
    cat("\n")

    to_install <-
      rstudioapi::showQuestion(
        'Install "RcmdrPlugin.biostat"?',
        "Do you want to install 'RcmdrPlugin.biostat' now?",
        " Install "
      )
    if (to_install) {
      rstudioapi::sendToConsole('check_installed_packages("Rcmdr-biostat")')

    } else {
      invisible()
    }
  }
}

