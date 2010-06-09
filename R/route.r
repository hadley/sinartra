#' Parse route (with parameters) into regular expression.  
#'
#' @param route route url.
#' @return 
#'   \item{match}{regular expression to match route}
#'   \item{params}{parameter names}
route_re <- function(route) {
  # Escape special characters that can occur in both urls and regexps
  route <- str_replace(route, "([.])", "\\\\\\1")
  
  # Extract parameters
  params <- str_extract_all(route, ":[a-zA-Z0-9_.]+|[*]")[[1]]
  
  keys <- str_replace(params, ":", "")
  keys[keys == "*"] <- "splat"
  
  match <- str_join("^", route, "$")
  match <- str_replace(match, ":[a-zA-Z0-9_.]+", "([^/?&#]+)")
  match <- str_replace(match, "[*]", "(.*?)")
  
  list(
    match = str_join(match, collapse = "/"),
    params = keys
  )
}

#' Generate functions to match specified route.
#'
#' @param route route url
#' @return 
#'   \item{match}{function returning \code{TRUE} if argument matches route,
#'     \code{FALSE} otherwise}
#'   \item{params}{function that parses path and returns named list of
#'      parameters}
route_matcher <- function(route) {
  re <- route_re(route)
  
  list(
    match = function(path) str_detect(path, re$match),
    params = function(path) {
      matches <- str_match(path, re$match)[1, -1]
      if (length(re$params) > 0) {
        # c is simplest way convert from array to vector
        c(tapply(matches, re$params, "c", simplify = FALSE))
      } else {
        list()
      }
    }
  )
}

