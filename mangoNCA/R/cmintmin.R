# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 20/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Finds the minimum concentration value and the first time point (in the sense of corresponding to the minimal index)
#' with that concentration.
#'
#' @title Calculate Minimum Concentration and time of minimum concentration
#' @param Conc numeric vector of concentrations
#' @param Time numeric vector of time points (parallel to \code{Conc})
#' @return list with numeric scalar (length 1 vector) components "cmin", "tmin" and "index".
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{Conc} or \code{Time} are of length 0, NA is returned for both values
#'      \item If the sum of \code{Code} (after omitting missing values) is 0, NA is returned for both values
#'      \item \code{cmin} is computed by stripping NA values from \code{Conc}
#'      \item if \code{Conc} and \code{Time} are not equal length numeric vectors, an exception will be generated by \code{\link{checkNumericSameLength}}
#'  }
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' CminTmin( Theoph2$conc, Theoph2$Time ) 
#' @keywords math

CminTmin<- function(Conc, Time){
    
    checkNumericSameLength(Time, Conc, "Time", "Concentration")
    
    # timeConc : data.frame which holds time and concentration in a single object
    timeConc <- data.frame(Time, Conc)
    
    # Initialize variables, set default value to NA
    # cmin : single element numeric vector, will hold minimum concentration
    # tmin : single element numeric vector, will hold first time of minimum concentration
    
    cmin <- NA
    tmin <- NA
    index <- NA
    
    ## Do Data check : there should be at least one conc/time element, and sum of concentrations should be non-zero
    if(nrow(timeConc) < 1){
        return(list(cmin = cmin, tmax = tmin, index = index))
    }
    
    if(sum(timeConc$Conc, na.rm=TRUE) == 0){
        return(list(cmin = cmin, tmin = tmin, index = index))
    }
    
    # Find the minimum concentration value (ignore NA values).
    
    cmin <- min(timeConc$Conc, na.rm = TRUE)
    
    # Get the indices that match cmin and then find the minimum index value.
    # This assumes that x is sorted in assending order of time to get the first occurance of cmin.    
    
    index <- min(which(timeConc$Conc == cmin))
    
    tmin <- timeConc[index, "Time"]
    
    
    return(list(cmin = cmin, tmin = tmin, index = index))
}
