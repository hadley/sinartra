\name{error}
\alias{error}
\title{Return an error page.
Error pages located in \code{public/error/status.html}.  If appropriate
page not found, error returned as string}
\usage{
  error(status = 404L, path = getwd())
}
\arguments{
  \item{status}{HTTP status code}

  \item{path}{web app path}
}
\description{
  Return an error page. Error pages located in
  \code{public/error/status.html}.  If appropriate page not
  found, error returned as string
}

