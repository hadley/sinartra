\name{static_file}
\alias{static_file}
\title{Send static file to browser, if present.}
\usage{static_file(path, status=200, remove=FALSE)}

\description{
  Send static file to browser, if present. Otherwise pass.
}
\arguments{
  \item{path}{path to file}
  \item{status}{http status code}
  \item{remove}{boolean to remove file after being read}
}
