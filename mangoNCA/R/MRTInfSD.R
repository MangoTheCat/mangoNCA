
# Date of last change: 30/03/2016
# Last changed by: ccampbell
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Mean Residence time (Single dose)
#'
#' Calculates mean residence time from AUMC, AUC and duration of infusion.
#' The value returned is: 
#' \deqn{\frac{AUMC}{AUC} - \frac{duration}{2}}
#'
#' @param AUC Single numeric value, area under the concentration time curve
#' @param AUMC Single numeric value, area under the concentration time moment curve
#' @param duration Duration of infusion, single numeric value
#' @title Calculate Mean Residence time
#' @return A single numeric vector.  
#' @author Mango Solutions
#' @export
#' @keywords math


MRTSD <- function(AUC, AUMC, duration = 0) {
    
    checkSingleNumeric(AUC, description = "AUC")
    
    checkSingleNumeric(AUMC, description = "AUMC")
    
    checkSingleNumeric(duration, description = "Duration of infusion")
    
    if (duration < 0) {
        return(NA_real_)
    }
    
    mrt <- AUMC / AUC - duration / 2
    
    return(mrt)
}


#' Mean Residence time (Single dose) Predicted
#'
#' Calculates the mean residence time to infinity (predicted), for steady-state models of type "M3".
#' @inheritParams AUCPartial
#' @param duration Duration of infusion, single numeric value
#' @title Calculate Mean Residence time to infinity (predicted) for steady-state data, model M3
#' @return A single numeric vector.  The value returned is:
#' \deqn{\frac{AUMCInfPred}{AUCInfPred} - \frac{duration}{2}}
#' @author Mango Solutions
#' @export
#' @keywords math

MRTInfPredSD <- function(conc, time, duration = 0, 
    lamznpt = NULL, lambdaZStats = NULL, 
    usepoints = NULL, excpoints = FALSE, minpoints = 3, addt0 = FALSE, inter = "Linear", 
    useObs = FALSE) {
    
    checkSingleNumeric(duration, description = "Duration of infusion")
    
    checkNumericSameLength(time, conc, "time", "concentration", "MRTInfPredSD") 
    
    if (duration < 0) {
        return(NA_real_)
    }
    
    AUMCIP <- AUCInfPred(conc = conc, time = time, lamznpt = lamznpt, calculation = "moment", addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter)
    
    AUCIP <- AUCInfPred(conc = conc, time = time, lamznpt = lamznpt, addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter)
    
    mrtp <- MRTSD(AUC = AUCIP, AUMC = AUMCIP, duration = duration)
    
    return(mrtp)
}


#' Mean Residence time (Single dose) Observed
#'
#' Calculates the mean residence time to infinity (observed), for steady-state models of type "M3".
#' @inheritParams AUCPartial
#' @param duration Duration of infusion. Must be a 
#' @title Calculate Mean Residence time to infinity (observed) for steady-state data, model M3
#' @return A single numeric vector.  The value returned is:
#' \deqn{\frac{AUMCInfObs}{AUCInfObs} - \frac{duration}{2}}
#' @author Mango Solutions
#' @export
#' @keywords math

MRTInfObsSD <- function(conc, time, duration = 0,
    lamznpt = NULL, lambdaZStats = NULL, 
    usepoints = NULL, excpoints = FALSE, minpoints = 3, addt0 = FALSE, inter = "Linear", 
    useObs = FALSE) {
    
    checkSingleNumeric(duration, description = "Duration of infusion")
    
    checkNumericSameLength(time, conc, "time", "concentration", "MRTInfObsSD") 
    
    if (duration < 0) {
        return(NA_real_)
    }
        
    AUMCIO <- AUCInfObs(conc = conc, time = time, lamznpt = lamznpt, calculation = "moment", addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter)
    
    AUCIO <- AUCInfObs(conc = conc, time = time, lamznpt = lamznpt, addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter)
    
    mrto <- MRTSD(AUC = AUCIO, AUMC = AUMCIO, duration = duration)
    
    return(mrto)
}
