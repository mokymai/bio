% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects.R
\name{projects}
\alias{projects}
\alias{read_projects}
\title{Manage RStudio Projects}
\usage{
read_projects(file, sort_by = FALSE)
}
\arguments{
\item{file}{(character) Path to file with RStudio project names.}

\item{sort_by}{(\code{"name"}|\code{"path"}|\code{\link{FALSE}})}
}
\description{
\itemize{
\item \code{read_projects()} - reads file with projects and list their names and paths.
}
}
\examples{
\dontrun{\donttest{
read_projects(get_path_recent_proj_list())
}}

}
\concept{rstudio projects}
