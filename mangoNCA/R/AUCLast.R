
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Curve to from time = 0 to time = Last
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
#' @title Calculate Area Under Curve Until Last Measurable concentration
#' @inheritParams AUCInfObs
#' @return single numeric - area under the concentration/time curve until the last measurable concentration.
#' @export
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{conc} or \code{time} are of length 1 or 0, a single NA is returned
#'      \item \code{conc} and \code{time} should be equal length numeric vectors, otherwise an exception will be generated.
#'      \item If \code{clast} or \code{tlast} is NA, NA will be returned
#'      \item There should be no duplicated time entries
#'  }
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(Theoph, Subject == 1)
#' AUCLast(Theoph1$conc, Theoph1$time)

AUCLast <- function(conc, time, addt0 = FALSE, inter = "Linear") {

    checkNumericSameLength(time, conc, "time", "concentration", functionName = "AUCLast")
    checkOrderedVector(time, description = "time", functionName = "AUCLast")
    checkSingleCharacter(inter, description = "inter", functionName = "AUCLast")
    
    # Add T = 0 if it is missing and remove missing values
    
    cleanData <- try(cleanconctime(conc = conc, time = time, addt0 = addt0), silent = TRUE)
    
    if (is(cleanData, "try-error")) {
        
        stop(paste("Error in AUCLast: Error during data cleaning", as.character(cleanData), collapse = "\n"))
    }
    
    conc <- cleanData$conc
    time <- cleanData$time
    
    inter <- gsub(" *", "", casefold(inter, upper = FALSE))

    if (!inter %in% c("linear", "linearlog", "linuplogdown")) { 
        stop("argument inter should have value 'Linear', 'Lin up Log down' or 'Linear Log' in AUCLast") 
    }
    # cLastTlast : list with elements clast and tlast corresponding to 
    # last measurable concentration and time
    
    cLastTlast <- ClastTlast(conc = conc, time = time)
    
    # if either element is NA, return NA
    
    auc <- as.numeric(NA)
    
    if (is.na(cLastTlast$clast) | is.na(cLastTlast$tlast)) { inter <- "na" }
    
    # find the index of tlast, and use this to calculate AUC.  Since time must be sorted and there should be no duplicates
    # (by assumption), this must be the time of the last measurable concentration
    
    # lasttimeIndex : single numeric holding index of tlast
    
    lasttimeIndex <- cLastTlast$index
    
    checkSingleNumeric(lasttimeIndex, "Last time index")
    
    if (length(conc) == 1) { inter <- "single" }
    
    # now calculate auc
    
    switch(inter, 
        
        "linear" = auc <- sum(AUCLin(conc = conc[seq_len(lasttimeIndex)], time = time[seq_len(lasttimeIndex)])), 
        
        "linearlog" = { 
            
            # cMaxTmax : list with elements cmax and tmax corresponding to max measurable concentration and time
            
            cMaxTmax <- CmaxTmax(conc = conc, time = time)
            
            # cmaxIndex : single numeric holding index of tlast
            
            cmaxIndex <- cMaxTmax$index
            
            checkSingleNumeric(lasttimeIndex, "Last time index")
            
            auc <- sum(AUCLin(conc = conc[seq_len(cmaxIndex)], time = time[seq_len(cmaxIndex)]), 
                AUCLog(conc = conc[seq.int(cmaxIndex, lasttimeIndex)], time = time[seq.int(cmaxIndex, lasttimeIndex)]))
        },
        
        "linuplogdown" = {
            
            is_conc_increasing <- diff(conc) > 0
            
            auc <- sum(AUCLin(conc = conc, time = time)[is_conc_increasing], 
                AUCLog(conc = conc, time = time)[!is_conc_increasing])
            
        },
        
        "single" = { auc <- as.numeric(0) },    
        
        "na" = { warning("missing value at Tlast") },
        
        stop("only methods 'Linear', 'Linear Log', 'Lin up Log down' implemented"))
    
    return(auc)
}
