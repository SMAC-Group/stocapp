% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R
\name{getIteration}
\alias{getIteration}
\alias{getIteration,Stocapp-method}
\title{Accessor to the object in class union "Stocapp"}
\usage{
getIteration(x)

\S4method{getIteration}{Stocapp}(x)
}
\arguments{
\item{x}{an object of class union "Stocapp"}
}
\value{
a number of iterations (as in \code{\link{getExtra}}).
}
\description{
Method for obtaining the number of iteration from fitted model within
any object of class union \linkS4class{Stocapp}.
}
\details{
This methods allow to access extra information about
the number of iterations.
}
\seealso{
\linkS4class{Stocapp}
}
