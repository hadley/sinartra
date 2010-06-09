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
    return(list(paylout = msg, "status code" = status))
  }
  
  static_file(path, status = status)
}

#' Pass on this match and defer to next.
#' Pass if you don't want to abort url matching, but want to try the next 
#' possible match.
#' 
#' @export
pass <- function() structure(TRUE, class = "pass")

is.pass <- function(x) inherits(x, "pass")

#' Return a static file, if present.  Otherwise pass.
#'
#' @param path path to file
#' @param status http status code
#' @export
static_file <- function(path, status = 200L) {
  path <- normalizePath(path)
  if (!file.exists(path)) return(pass())
  
  list(file = path, mime_type(path), "status code" = status)
}

#' Render template using brew.
#' Templates are located in \code{views}.
#' 
#' @param template string giving template name (file name without extension)
#' @param params list of parameters to be evaluated in template
#' @param path web app path
#' @export
render_brew <- function(template, params = list(), path = getwd()) {
  if (is.list(params)) {
    env <- new.env(TRUE)
    for(name in names(params)) {
      env[[name]] <- params[[name]]
    }
    params <- env
  }
  
  path <- file.path(path, "views", stringr::str_join(template, ".html"))
  if (!file.exists(path)) stop("Can not find ", template, " template ",
    call. = FALSE)

  list(payload = 
    stringr::str_join(capture.output(brew::brew(path, envir = params)), collapse = "\n"))
}

#' Redirect to new url.
#'
#' @param url full url to redirect to
#' @param status http status code
#' @export
redirect <- function(url, status = 301L) {
  list(
    payload = "Redirecting...", 
    "content-type" = 'text/html',
    header = stringr::str_c('Location: ', url),
    "status code" = 302L
  )
}