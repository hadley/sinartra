#' Router object.
#' The router object coordinates matching urls to functions. 
#' 
#' Add possible route using the appropriate http verb (get, post, put,
#' delete). Currently only get is implemented.  These methods have two 
#' arguments: \code{route}, the description of the url to match, and the
#' \code{callback} to run when the route is matched.
#'
#' Route urls have a special syntax.  They are basically paths but with two
#' special keywords: \code{:param} which matches a single component and
#' \code{*} which will match anything.
#'
#' \itemize{
#'  \item \code{hello.html} matches \code{hello.html only}
#'  \item \code{packages/:package.html} will match \code{packages/a.html},
#'     \code{packages:/my-package.html} and so on, and the callback will be
#'     called with an argument named package containing "a" and "my-package"
#'     respectively.
#'  \item \code{*.html} will match anything ending in \code{.html} and 
#'     callback will be called with a "splat" argument
#' }
#' @name Router
#' @import evaluate
#' @export
Router <- mutatr::Object$clone(base_url = "", file_path = getwd())$do({
  self$matchers <- list()
  
  self$get <- function(route, callback) {
    rm <- route_matcher(route)
    rm$callback <- callback
    
    self$matchers[[length(self$matchers) + 1]] <- rm
  }
    
  self$route <- function(path, query) {
    for(matcher in rev(self$matchers)) {
      p <- str_replace(path, base_url, "")
      if (matcher$match(p)) {
        params <- matcher$params(p)
        params$query <- query
        
        call <- bquote(do.call(.(matcher$callback), .(params)))
        res <- try_capture_stack(call, sys.frame())
        
        if (is.error(res)) {
          traceback <- stringr::str_c(create_traceback(res$calls), collapse = "\n")
          return(stringr::str_c("ERROR: ", res$message, "\n\n", traceback))
        }
        
        if (!is.pass(res)) return(res)
      }
    }
    
    # If none match, return 404
    error(404)
  }
  
  combine_url <- function(y) {
    if(str_length(base_url) < 1) return(y)
    if(str_sub(base_url, start = -1) == "/") base_url <- str_sub(base_url, end = -2) 
    str_c(base_url, y, sep = "/")
  }
  combine_path <- function(b){ 
    file.path(file_path, b)
  }
  
  self$error <- function(..., path){
    p <- combine_url(path)
    error(..., path = p)
  }
  self$static_file <- function(path, ...) {
    p <- combine_path(path)
    static_file(path = p, ...)    
  }

  self$redirect <- function(url, ...) {
    u <- combine_url(url)
    redirect(url = u, ...)
  }
  
  self$render_brew <- function(template, ...){
    t <- combine_path(template)
    render_brew(template = t, ...)
  }
  
})


is.error <- function(x) inherits(x, "simpleError")
