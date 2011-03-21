\name{error}
\alias{error}
\title{Return an error page.}
\usage{error(status=404, path=getwd())}
\description{Return an error page.
Error pages located in \code{public/error/status.html}.  If appropriate
page not found, error returned as string}
\arguments{\item{status}{HTTP status code}
\item{path}{web app path}}
