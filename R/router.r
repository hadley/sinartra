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
Router <- mutatr::Object$clone()$do({
  self$matchers <- list()
  self$base_url  <- ""
  self$base_path <- getwd()
  
  
  self$get <- function(route, callback) {
    rm <- route_matcher(route)
    rm$callback <- callback
    
    self$matchers[[length(self$matchers) + 1]] <- rm
  }
    
  self$route <- function(path, query) {
    for(matcher in rev(self$matchers)) {
      p <- str_replace(path, self$base_url, "")
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
    if(str_length(self$base_url) < 1) return(y)
    u <- self$base_url
    if(str_sub(u, start = -1) == "/") u <- str_sub(u, end = -2) 
    str_c(u, y, sep = "/")
  }
  combine_path <- function(y){ 
    if(str_sub(y, end = 1) == "/"){
      y
    } else {
      file.path(self$base_path, y)
    }
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
  
  self$render_brew <- function(..., path){
    if(missing(path)) path <- self$base_path
    
    render_brew(..., path = path, parent = parent.frame())
  }
  
})


is.error <- function(x) inherits(x, "simpleError")
