\name{route_matcher}
\alias{route_matcher}
\title{Generate functions to match specified route.}
\usage{
  route_matcher(route)
}
\arguments{
  \item{route}{route url}
}
\value{
  \item{match}{function returning \code{TRUE} if argument
  matches route, \code{FALSE} otherwise}
  \item{params}{function that parses path and returns named
  list of parameters}
}
\description{
  Generate functions to match specified route.
}

