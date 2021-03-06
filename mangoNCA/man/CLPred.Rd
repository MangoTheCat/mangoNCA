% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clearance.R
\name{CLPred}
\alias{CLPred}
\title{Apparent Clearance (predicted)}
\usage{
CLPred(Conc, Time, Dose, numPoints = NULL)
}
\arguments{
\item{Conc}{A numeric vector of concentration values}

\item{Time}{A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted
in ascending order.}

\item{Dose}{A single numeric}

\item{numPoints}{Number of points to use for the lambda z calculation, counted from the last measurable concentration.
Must be a single integer greater than 1 and less than or equal to the length of \code{Time} and \code{Conc}.
Default is NULL, numPoints is then calculated by \code{selectPoints}.}
}
\value{
apparent clearance (single numeric)
}
\description{
Calculates apparent clearance (predicted), which is equal to administered dose divided by AUC0Inf_Pred.
}
\note{
The following additional input processing/checks are performed:
\enumerate{
     \item All checks for \code{AUCInfPred} apply
     \item \code{Dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
         \code{Dose} is \code{NA}, 0 or less, \code{NA} will be returned 
}
}
\examples{
Theoph1 <- subset(  Theoph, Subject == 1)
CLPred(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1])
}
\author{
Mango Solutions
}
\keyword{math}
