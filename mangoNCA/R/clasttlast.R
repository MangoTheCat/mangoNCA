# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate:  $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Finds the last measurable (non-zero) concentration value and the last time point
#' with that concentration.
#'
#' @title Calculate Last Measurable concentration
#' @param conc numeric vector of concentrations
#' @param time numeric vector of time points (should be of equal length to conc, and sorted).
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{conc} or \code{time} are of length 0, a single NA is returned
#'      \item \code{conc} and \code{time} should be equal length numeric vectors, otherwise an exception will be generated
#'      \item It is assumed that these times and concentrations come from a single curve 
#'      \item time should be sorted
#'      \item If the sum of \code{conc} (after omitting missing values) is 0, returned values are NA
#'  }
#' @return list with numeric scalar components \code{clast}, \code{tlast} and \code{index}
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' ClastTlast( conc = Theoph1$conc, time = Theoph1$time )
#' @keywords math utils
#' @export

ClastTlast <- function(conc, time) {
    
    checkNumericSameLength(time, conc, "time", "concentration")
    
    # check that the time vector is ordered, and generated an exception if it is not
    # timeOrder : a numeric vector holding the ordering of the time vector (with NA removed)
    
    timeOrder <- order(na.omit(time))
    
    if (!all( timeOrder == seq_along(na.omit(time)) ) ) {
        stop("time vector is not sorted!")
    }
    
    # conctime: data.frame holding columns of times and concentrations
    conctime <- data.frame(time, conc)
    
    # clast : single length numeric holding last measurable concentration
    # tlast : single length numeric holding time of last measureable concentration
    # indexlast : single length numeric holding last Measurable concentration Index
    
    clast <- as.numeric(NA)
    tlast <- as.numeric(NA)
    indexlast <- as.numeric(NA)
    
    # Do Data check.  There should be at least one concentration/time record, and the sum of the concentrations (after removing
    # NAs) should not be 0
    
    if (nrow(conctime) < 1 || sum(conctime$conc, na.rm = TRUE) <= 0) {
        return(list(clast = clast, tlast = tlast, index = indexlast))
    }
    
    # now find the last measurable concentration.
    # concentrationIsMeasurable : a logical vector of the same length as conc which indicates if corresponding elements of 
    # concentration are nonzero and non-missing
    # indexlast : either a 0-length numeric vector (if there were no non-missing concentrations) or a 
    # length-one numeric vector holding the highest index of measurable concetration data
    
    concentrationIsMeasurable <- !( is.na(conctime$conc) | ( conctime$conc == 0 ) )
    indexlast <- tail( which(concentrationIsMeasurable), n = 1)
    
    # no non-measurable concentrations, so return missing values
    
    if (length(indexlast) == 0 ) {
        return(list(clast = clast,tlast = tlast, index = as.numeric(NA)))
    }
    
    clast <- conctime$conc[indexlast]
    tlast <- conctime$time[indexlast]
    
    return(list(clast = clast, tlast = tlast, index = indexlast))
}
