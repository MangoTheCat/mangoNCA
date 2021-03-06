% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AUC.R
\name{AUCLin}
\alias{AUCLin}
\title{Calculate AUC Using Linear Trapezoidal Rules}
\usage{
AUCLin(Conc, Time)
}
\arguments{
\item{Conc}{numeric vector of concentrations}

\item{Time}{numeric vector of time points (same length as and parallel to Conc).
These elements should be sorted in ascending order, otherwise an exception will be generated.}
}
\value{
numeric vector of trapezium AUC values.
}
\description{
Calculates the area under a time-concentration curve (AUC) using the linear trapezium rule.
}
\details{
A vector of areas under individual trapezoids (not the total area) is returned.
These will be the individual AUC values at a given time/concentration index.
In other words, auc[i] is the area under the trapezium bounded by time[i], conc[i] and time[i+1], conc[i+1].
This function is called by \code{AUCLast}.
For a given time interval (t1:t2), the AUC can be calculated as follows:
    AUC_t1_t2 = (C1 + C2) * (t2 - t1) / 2
}
\note{
It is assumed that these times and concentrations come from a single curve.
The following error checks / processing will be performed:
 \enumerate{
     \item If \code{Conc} or \code{Time} are of length 0, a single NA is returned
     \item \code{Conc} and \code{Time} should be equal length numeric vectors, 
     otherwise an exception will be generated
     \item if \code{Time[i]} or \code{Conc[i]} is \code{NA} and \code{auc <-  AUCLin(Conc,Time)}, 
     then \code{auc[i]} and \code{auc[i - 1]} will be \code{NA}.
 }
}
\examples{
Theoph2 <- subset(Theoph, Subject == 2)
x <- MangoNca:::AUCLin(Theoph2$conc, Theoph2$Time) 
sum(x) # 91.5268
}
\keyword{math}
