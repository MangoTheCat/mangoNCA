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
#' @title Calculate Last Measurable Concentration
#' @param Conc numeric vector of concentrations
#' @param Time numeric vector of time points (should be of equal length to Conc, and sorted).
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{Conc} or \code{Time} are of length 0, a single NA is returned
#'      \item \code{Conc} and \code{Time} should be equal length numeric vectors, otherwise an exception will be generated
#'      \item It is assumed that these times and concentrations come from a single curve 
#'      \item time should be sorted
#'      \item If the sum of \code{Conc} (after omitting missing values) is 0, returned values are NA
#'  }
#' @return list with numeric scalar components \code{clast}, \code{tlast} and \code{index}
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' ClastTlast( Conc = Theoph1$conc, Time = Theoph1$Time )
#' @keywords math utils
#' @export

ClastTlast <- function(Conc, Time)
{
        
    checkNumericSameLength(Time, Conc, "Time", "Concentration")
    
    # check that the time vector is ordered, and generated an exception if it is not
    # TimeOrder : a numeric vector holding the ordering of the Time vector (with NA removed)
    
    TimeOrder <- order(na.omit(Time))
    
    if( !all( TimeOrder == seq_along(na.omit(Time)) ) ) {
        stop("Time vector is not sorted!")
    }
    
    # concTime: data.frame holding columns of times and concentrations
    concTime <- data.frame(Time, Conc)
    
    # clast : single length numeric holding last measurable concentration
    # tlast : single length numeric holding time of last measureable concentration
    # indexlast : single length numeric holding last Measurable Concentration Index
    
    clast <- as.numeric(NA)
    tlast <- as.numeric(NA)
    indexlast <- as.numeric(NA)
    
    # Do Data check.  There should be at least one concentration/time record, and the sum of the concentrations (after removing
    # NAs) should not be 0
    
    if( nrow(concTime) < 1 || sum(concTime$Conc, na.rm = TRUE) <= 0)
    {
        return(list(clast = clast, tlast = tlast, index = indexlast))
    }
    
    # now find the last measurable concentration.
    # concentrationIsMeasurable : a logical vector of the same length as Conc which indicates if corresponding elements of 
    # concentration are nonzero and non-missing
    # indexlast : either a 0-length numeric vector (if there were no non-missing concentrations) or a 
    # length-one numeric vector holding the highest index of measurable concetration data
    
    concentrationIsMeasurable <- !( is.na(concTime$Conc) | ( concTime$Conc == 0 ) )
    indexlast <- tail( which(concentrationIsMeasurable), 1 )
    
    # no non-measurable concentrations, so return missing values
    
    if( length(indexlast) == 0 )
    {
        return(list(clast = clast,tlast = tlast, index = as.numeric(NA)))
    }
    
    clast <- concTime$Conc[indexlast]
    tlast <- concTime$Time[indexlast]
    
    return(list(clast = clast, tlast = tlast, index = indexlast))
}
