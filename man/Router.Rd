\name{Router}
\alias{Router}
\title{Router object.}
\description{Router object.
The router object coordinates matching urls to functions.}
\details{Add possible route using the appropriate http verb (get, post, put,
delete). Currently only get is implemented.  These methods have two 
arguments: \code{route}, the description of the url to match, and the
\code{callback} to run when the route is matched.

Route urls have a special syntax.  They are basically paths but with two
special keywords: \code{:param} which matches a single component and
\code{*} which will match anything.

\itemize{
\item \code{hello.html} matches \code{hello.html only}
\item \code{packages/:package.html} will match \code{packages/a.html},
\code{packages:/my-package.html} and so on, and the callback will be
called with an argument named package containing "a" and "my-package"
respectively.
\item \code{*.html} will match anything ending in \code{.html} and 
callback will be called with a "splat" argument
}}
