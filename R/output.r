#' Return an error page.
#' Error pages located in \code{public/error/status.html}.  If appropriate
#' page not found, error returned as string
#'
#' @param status HTTP status code
#' @param path web app path
#' @export
error <- function(status = 404L, path = getwd()) {
  path <- file.path(path, "public", "errors", stringr::str_c(status, ".html"))
  if (!file.exists(path)) {
    msg <- stringr::str_c("Error status: ", status)
    render(msg, status)
  } else {
    static_file(path, status = status)    
  }

}

#' Pass on this match and defer to next.
#' Pass if you don't want to abort url matching, but want to try the next 
#' possible match.
#' 
#' @export
pass <- function() structure(TRUE, class = "pass")

is.pass <- function(x) inherits(x, "pass")

#' Send static file to browser, if present. Otherwise pass.
#'
#' @param path path to file
#' @param status http status code
#' @export
static_file <- function(path, status = 200L) {
  path <- suppressWarnings(normalizePath(path))
  if (!file.exists(path)) return(pass())
  
  list(
    file = path, 
    "content-type" = mime_type(path), 
    "status code" = status
  )
}

#' Redirect to new url.
#'
#' @param url full url to redirect to
#' @param status http status code
#' @export
redirect <- function(url, status = 301L) {
  render("Redirecting...", status = status, 
    header = stringr::str_c('Location: ', url))
}
