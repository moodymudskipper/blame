% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{review_object_history}
\alias{review_object_history}
\title{Review object history}
\usage{
review_object_history(
  git_history,
  object,
  ascending = TRUE,
  mode = c("sidebyside", "unified", "context", "auto"),
  context = -1
)
}
\arguments{
\item{git_history}{output of git_history()}

\item{object}{The name of an object.}

\item{ascending}{A boolean.}

\item{mode, context}{Passed to \code{diffobj::diffChr()}, with different defaults.}
}
\description{
On the left (resp. right) we find the latest (resp. earliest) commit for a given object.
}
\details{
If the object was moved from a file to another it is tracked, we also detect if
the object was removed/recreated.
}
