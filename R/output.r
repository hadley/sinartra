# Return an error page
error <- function(status, path) {
  list(
    payload = c("file" = str_join("public/errors/", status, ".html")), 
    status = status
  )
}

# Pass on this match and defer to next
pass <- function() structure(TRUE, class = "pass")
is.pass <- function(x) inherits(x, "pass")

# Return a static file, if present.  Otherwise pass.
static_file <- function(path) {
  path <- normalizePath(path)
  if (!file.exists(path)) return(pass())
  
  list(
    payload = c("file" = str_join(readLines(path), collapse = "\n")),
    "content-type" = mime_type(path)
  )  
}

render_brew <- function(template, params = list(), path = getwd()) {
  if (is.list(params)) {
    env <- new.env(TRUE)
    for(name in names(params)) {
      env[[name]] <- params[[name]]
    }
    params <- env
  }
  
  path <- file.path(path, "views", str_join(template, ".html"))
  if (!file.exists(path)) stop("Can not find ", template, " template ",
    call. = FALSE)

  list(payload = 
    str_join(capture.output(brew(path, envir = params)), collapse = "\n"))
}

redirect <- function(path, status = 301L) {
  list(
    payload = "Redirecting...", 
    "content-type" = 'text/html',
    header = paste('Location: ', path, sep=''),
    "status code" = 302L
  )
}