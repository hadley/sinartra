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
  
  self$get <- function(route, callback) {
    rm <- route_matcher(route)
    rm$callback <- callback
    
    self$matchers[[length(self$matchers) + 1]] <- rm
  }
  
  self$route <- function(path, query) {
    for(matcher in rev(self$matchers)) {
      if (matcher$match(path)) {
        params <- matcher$params(path)
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
})


is.error <- function(x) inherits(x, "simpleError")
