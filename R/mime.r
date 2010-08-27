#' Compute mime type from file extension.
#' Needs mechanism for user extension.
#'
#' @param path path to file
#' @return mime string
mime_type <- function(path) {
  ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
  n <- length(ext)
  
  if (n == 0) return()
  
  types <- c(
    "css" = "text/css",
    "gif" = "image/gif", # in R2HTML
    "js" = "text/javascript",
    "jpg" = "image/jpeg",
    "html" = "text/html",
    "ico" = "image/x-icon",
    "pdf" = "application/pdf",
    "eps" = "application/postscript",
    "ps" = "application/postscript", # in GLMMGibbs, mclust
    "sgml"= "text/sgml", # in RGtk2
    "xml" = "text/xml",  # in RCurl
    "text/plain"
  )
  
  unname(types[ext[n]])
}
