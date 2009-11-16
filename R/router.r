source("route.r")
source("output.r")

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
        
        res <- do.call(matcher$callback, params)
        if (!is.pass(res)) return(res)
      }
    }
    
    # If none match, return 404
    error(404)
  }
  
})
