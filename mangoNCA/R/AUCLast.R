# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Curve to from Time = 0 to Time = Last
#' 
#' AUCLast calculates the area under a time/concentration curve from the first point up until the last 
#' measurable concentration using one of three methods.  
#' \code{Linear} rule calculates the area of the trapezia bounded by each sequential pair of time and concentration points (see \code{AUCLin}).  
#' 
#' \code{Linear Log} rule is a combination of the Linear trapezoidal rule and the Log trapezoidal rule (see \code{AUCLog}). Before Cmax  
#' (as in the absorption phase), the linear trapezoidal method is used. After Cmax  
#' (as in the elimination phase), the logarithmic trapezoidal method is used. 
#' 
#' \code{Lin Up Log Down} rule is a combination of the Linear trapezoidal rule and the Log trapezoidal rule. When concentrations are increasing  
#' (as in the absorption phase), the linear trapezoidal method is used. When concentrations are decreasing  
#' (as in the elimination phase), the logarithmic trapezoidal method is used. 
#' 
#' These alternative methods to the Linear  is often preferred because  
#' the linear method is often a good approximation of drug absorption while logarithmic decline is well represented  
#' by the logarithmic trapezoidal method during drug elimination.
#'
#' Calculations are performed for the area bounded by T0 and the time of the last nonzero, non-missing concentration.
#' For comparison with WinNonLin, a timepoint at T = 0 is added if it does not exist. 
#' Missing values are removed before further calculations are performed.
#'
#' @title Calculate Area Under Curve Until Last Measurable Concentration
#' @param Conc A numeric vector of concentration values
#' @param Time A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted in ascending order.
#' @param addT0 Single logical value declaring whether T0 should be added if missing provided execution is Safe (default TRUE).
#' @param Safe Single logical value declaring whether to perform data checks and data cleaning (default is TRUE).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default), 
#' \code{"Lin up Log down"} or \code{"Linear Log"} (not case or space sensitive).
#' @return single numeric - area under the concentration/time curve until the last measurable concentration.
#' @export
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{Conc} or \code{Time} are of length 1 or 0, a single NA is returned
#'      \item \code{Conc} and \code{Time} should be equal length numeric vectors, otherwise an exception will be generated.
#'      \item If \code{clast} or \code{tlast} is NA, NA will be returned
#'      \item There should be no duplicated time entries
#'  }
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(Theoph, Subject == 1)
#' AUCLast(Theoph1$conc, Theoph1$Time)

AUCLast <- function(Conc, Time, addT0 = FALSE, Safe = TRUE, inter = "Linear") {

    checkSingleLogical(Safe, description = "Safe", functionName = "AUCLast")
    
    if (Safe) {
        
        checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "AUCLast")
        checkOrderedVector(Time, description = "Time", functionName = "AUCLast")
        checkSingleCharacter(inter, description = "inter", functionName = "AUCLast")
        
        # Add T = 0 if it is missing and remove missing values
        
        cleanData <- try(cleanConcTime(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
        
        if (is(cleanData, "try-error")) {
            
            stop(paste("Error in AUCLast: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        }
        
        Conc <- cleanData$Conc
        Time <- cleanData$Time
    }
    
    inter <- gsub(" *", "", casefold(inter, upper = FALSE))

    if (!inter %in% c("linear", "linearlog", "linuplogdown")) { stop("argument inter should have value 'Linear', 'Lin up Log down' or 'Linear Log' in AUCLast") }

    # cLastTlast : list with elements clast and tlast corresponding to last measurable concentration and time
    
    cLastTlast <- ClastTlast(Conc = Conc, Time = Time)
    
    # if either element is NA, return NA
    
    auc <- as.numeric(NA)
    
    if (is.na(cLastTlast$clast) | is.na(cLastTlast$tlast)) { inter <- "na" }
    
    # find the index of tlast, and use this to calculate AUC.  Since time must be sorted and there should be no duplicates
    # (by assumption), this must be the time of the last measurable concentration
    
    # lastTimeIndex : single numeric holding index of tlast
    
    lastTimeIndex <- cLastTlast$index
    
    checkSingleNumeric(lastTimeIndex, "Last time index")
    
    if (length(Conc) == 1) { inter <- "single" }
    
    # now calculate auc
    
    switch(inter, 
        
        "linear" = auc <- sum(AUCLin(Conc = Conc[seq_len(lastTimeIndex)], Time = Time[seq_len(lastTimeIndex)])), 
        
        "linearlog" = { 
            
            # cMaxTmax : list with elements cmax and tmax corresponding to max measurable concentration and time
            
            cMaxTmax <- CmaxTmax(Conc = Conc, Time = Time)
            
            # cmaxIndex : single numeric holding index of tlast
            
            cmaxIndex <- cMaxTmax$index
            
            checkSingleNumeric(lastTimeIndex, "Last time index")
            
            auc <- sum(AUCLin(Conc = Conc[seq_len(cmaxIndex)], Time = Time[seq_len(cmaxIndex)]), 
                AUCLog(Conc = Conc[seq.int(cmaxIndex, lastTimeIndex)], Time = Time[seq.int(cmaxIndex, lastTimeIndex)]))
        },
        
        "linuplogdown" = {
            
            is_Conc_increasing <- diff(Conc) > 0
            
            auc <- sum(AUCLin(Conc = Conc, Time = Time)[is_Conc_increasing], 
                AUCLog(Conc = Conc, Time = Time)[!is_Conc_increasing])
            
        },
        
        "single" = { auc <- as.numeric(0) },    
        
        "na" = { warning("missing value at Tlast") },
        
        stop("only methods 'Linear', 'Linear Log', 'Lin up Log down' implemented"))
    
    return(auc)
    
}
