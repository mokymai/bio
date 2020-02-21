% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settings.R
\name{RStudio-settings}
\alias{RStudio-settings}
\alias{reset_rstudio_user_settings}
\alias{read_rs_user_settings}
\alias{read_rs_ui_prefs}
\alias{get_rs_ui_prefs}
\alias{get_rs_user_settings_names}
\alias{get_rs_ui_pref_names}
\title{RStudio settings and preferences.}
\usage{
reset_rstudio_user_settings(to, backup = TRUE, ask = TRUE)

read_rs_user_settings(which = "current")

read_rs_ui_prefs(which = "current")

get_rs_ui_prefs()

get_rs_user_settings_names(which = "current")

get_rs_ui_pref_names(which = "current")
}
\arguments{
\item{to}{The set of RStudio user settings.
One of: "rstudio-default" or "bio-default".}

\item{backup}{(logical) If \code{TRUE}, a backup copy of files with settings is
created.}

\item{ask}{(logical) If \code{TRUE}, user confirmation to reset settings is
required.}

\item{which}{(character) type of settings: "current", "bio-default".}
}
\value{
A named list with some of current RStudio preferences.
}
\description{
Get a list of current RStudio preferences.
}
\details{
Find the names of the preferences at:
https://github.com/rstudio/rstudio/blob/5f2b79427ed526e22f78c794a4b5894ebee2de75/src/cpp/session/SessionUserSettings.cpp#L357-L447
}
\examples{
\dontrun{\donttest{
#-------------------------------------------------
# .rs.readUiPref()
# .rs.writeUiPref()

#-------------------------------------------------
head(read_rs_user_settings(), n = 2)

head(read_rs_user_settings("bio-default"), n = 2)

#-------------------------------------------------
head(read_rs_ui_prefs(), n = 2)

head(read_rs_ui_prefs("bio-default"), n = 2)

#-------------------------------------------------
head(get_rs_ui_prefs(), n = 2)

#-------------------------------------------------
get_rs_user_settings_names()

get_rs_ui_pref_names()

#-------------------------------------------------

reset_rstudio_user_settings(to = "bio-default")

reset_rstudio_user_settings(to = "rstudio-default")

}}
}
\seealso{
RStudio functions \code{.rs.readUiPref()}, \code{.rs.writeUiPref()}.
\itemize{
\item https://stackoverflow.com/a/55940249/4783029
\item https://stackoverflow.com/a/54982341/4783029
}
}