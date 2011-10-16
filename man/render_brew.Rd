\name{render_brew}
\alias{render_brew}
\title{Render template using brew.
Templates are located in \code{views}.}
\usage{
  render_brew(template, params = NULL, path = getwd(),
  parent = parent.frame())
}
\arguments{
  \item{template}{string giving template name (file name
  without extension)}

  \item{params}{list of parameters to be evaluated in
  template}

  \item{path}{web app path}

  \item{parent}{parent.frame() to be used}
}
\description{
  Render template using brew. Templates are located in
  \code{views}.
}

