% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engdata.R
\docType{data}
\name{Weng}
\alias{Weng}
\title{A 313 by 313 proximity matrix for the 313 LADCUAS in England. Each entry is either 0 or 1 
and is 1 if the corresponding row and column LADCUAs share a common boundary.}
\format{
An object of class \code{matrix} (inherits from \code{array}) with 313 rows and 313 columns.
}
\source{
\insertCite{SahuBohning2021;textual}{bmstdr}.
}
\usage{
Weng
}
\description{
A 313 by 313 proximity matrix for the 313 LADCUAS in England. Each entry is either 0 or 1 
and is 1 if the corresponding row and column LADCUAs share a common boundary.
}
\examples{
 dim(Weng)
 summary(apply(Weng, 1, sum))
}
\references{
\insertAllCited{}
}
\keyword{datasets}
