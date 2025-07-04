% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ropenblas.R
\name{rcompiler}
\alias{rcompiler}
\title{Compile a version of R on GNU/Linux systems}
\usage{
rcompiler(x = NULL)
}
\arguments{
\item{x}{Version of R you want to compile. By default (\code{x = NULL}) the latest stable version of the R
language will be compiled. For example, \code{x = "4.5.0"} will compile and link \strong{R-4.5.0} as the major version on your system.}
}
\value{
Returns a message informing you if the procedure occurred correctly. You will also be able to receive information about
missing dependencies.
}
\description{
This function is responsible for compiling a version of the R language.
}
\details{
This function is responsible for compiling a version of the \href{https://www.r-project.org/}{\strong{R}} language. The \code{x} argument is the version of \href{https://www.r-project.org/}{\strong{R}} that you want to compile.
For example, \code{x = "4.5.1"} will compile and link the \strong{R-4.5.1} version as the major version on your system. By default (\code{x = NULL}), the latest stable version of \href{https://www.r-project.org/}{\strong{R}} will be compiled.

For example, to compile the latest stable version of the \href{https://www.r-project.org/}{\strong{R}} language, do:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ rcompiler()
}\if{html}{\out{</div>}}

Regardless of your GNU/Linux distribution and what version of \href{https://www.r-project.org/}{\strong{R}} is in your repositories, you can have the latest stable version of the \href{https://www.r-project.org/}{\strong{R}} language compiled
into your computer's architecture.

You can use the \code{rcompiler()} function to compile different versions of \href{https://www.r-project.org/}{\strong{R}}. For example, running \code{rcompiler(x = "3.6.3")} and \code{rcompiler()} will install versions 3.6.3 and 4.0.0 on your GNU/Linux distribution,
respectively. If you are on version 4.0.0 of \href{https://www.r-project.org/}{\strong{R}} and run the code \code{rcompiler(x = "3.6.3")} again, the function will identify the existence of version 3.6.3 on the system and give you the option to use the binaries
that were built in a previous compilation. This avoids unnecessary compilations.
}
\examples{
# rcompiler()
}
\seealso{
\code{\link{ropenblas}}, \code{\link{last_version_r}}
}
