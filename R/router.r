Router <- Object$clone()$do({
  self$matchers <- list()
  
  self$get <- function(route, callback) {
    rm <- route_matcher(route)
    rm$callback <- callback
    
    self$matchers[[length(self$matchers) + 1]] <- rm
  }
  
  self$route <- function(path) {
    for(matcher in rev(self$matchers)) {
      if (matcher$match(path)) {
        params <- matcher$params(path)
        
        call <- bquote(do.call(.(matcher$callback), .(params)))
        res <- try_capture_stack(call, sys.frame())
        
        if (is.error(res)) {
          traceback <- str_c(create_traceback(res$calls), collapse = "\n")
          return(str_c("ERROR: ", res$message, "\n\n", traceback))
        }
        
        if (!is.pass(res)) return(res)
      }
    }
    
    # If none match, return 404
    error(404)
  }
  
})


is.error <- function(x) inherits(x, "simpleError")
