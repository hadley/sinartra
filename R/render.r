#' Send output to browser
#'
#' @param text a character vector 
#' @param status http status code
#' @param mime_type mime type
#' @param ... other named arguments become other http headers
render <- function(text, status = 200L, mime_type = "text/html", ...) {
  text <- stringr:str_c(text, collapse = "\n")
  list(
    payload = text, 
    "content-type" = mime_type, 
    "status code" = status,
     ...)
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
  
  path <- file.path(path, "views", stringr::str_c(template, ".html"))
  if (!file.exists(path)) stop("Can not find ", template, " template ",
    call. = FALSE)

  render(capture.output(brew::brew(path, envir = params)))
}

#' Produce JSON from an R object
#'
#' @param object R object to be converted to JSON
render_json <- function(object) {
  json <- rjson::toJSON(object)
  render(json, mime_type = "application/json")
}
