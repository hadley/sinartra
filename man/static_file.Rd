\name{static_file}
\alias{static_file}
\title{Send static file to browser, if present. Otherwise pass.}
\usage{
  static_file(path, status = 200L, remove = FALSE)
}
\arguments{
  \item{path}{path to file}

  \item{status}{http status code}

  \item{remove}{boolean to remove file after being read}
}
\description{
  Send static file to browser, if present. Otherwise pass.
}

