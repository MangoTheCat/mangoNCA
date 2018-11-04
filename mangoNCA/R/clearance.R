# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' @title Clearance
#'
#' Calculates Clearance, which is equal to administered dose divided by AUCInf.
#'
#' @param AUCInf A numeric value
#' @param dose A numeric value
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item \code{AUCInf} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{AUCInf} is \code{NA}, 0 or less, \code{NA} will be returned for Observed
#'      \item \code{dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{dose} is \code{NA}, 0 or less, \code{NA} will be returned for both
#' }
#' @return Single numeric of clearance
#' @export
#' @author Mango Business Solutions
#' @keywords math
#' @examples 
#' clearance(AUCInf = 100, dose = 10)

clearance <- function(AUCInf, dose) {
    checkSingleNumeric(AUCInf, description = "AUCInf", functionName = "clearance")
    checkSingleNumeric(dose, description = "dose", functionName = "clearance")
    
    clearanceResult <- as.numeric(NA)

    if (!is.na(AUCInf) && AUCInf > 0 && !is.na(dose) && dose > 0) {
        clearanceResult <- dose/AUCInf
    }
    
    return(clearanceResult)
}



#' Calculates apparent clearance (predicted), which is equal to administered dose divided by AUC0Inf_Pred.
#' 
#' @param conc A numeric vector of concentration values
#' @param time A numeric vector of time values, parallel to \code{conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param lamznpt Number of points to use for the lambda z calculation, counted from the last measurable concentration.
#'  Must be a single integer greater than 1 and less than or equal to the length of \code{time} and \code{conc}.
#'  Default is NULL, lamznpt is then calculated by \code{selectPoints}.
#' @param dose A single numeric
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item All checks for \code{AUCInfPred} apply
#'      \item \code{dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{dose} is \code{NA}, 0 or less, \code{NA} will be returned 
#' }
#' @title Apparent Clearance (predicted)
#' @return apparent clearance (single numeric)
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' CLPred(conc = Theoph1$conc, time = Theoph1$time, dose = Theoph1$Dose[1])

CLPred <- function(conc, time, dose, lamznpt = NULL)
{
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    
    checkOrderedVector(time, description = "time", functionName = "CLPred")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "CLPred")
    checkSingleNumeric(dose, "dose", functionName = "CLPred")
    
    if(is.na(dose) || dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(lamznpt)) 
    { 
        lamznpt <- selectPoints(conc = conc, time = time)
        
    } else
    {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "CLPred")
        
    }
    
    # aucInfPred : Area under curve extrapolated to infinity
    
    aucInfPred <- AUCInfPred(conc = conc, time = time, lamznpt = lamznpt)
    
    cl <- clearance(AUCInf = aucInfPred, dose = dose)
    
    return(cl)
}


#' Calculates apparent clearance (observed), which is equal to administered dose divided by AUC0Inf_Obs.
#' 
#'
#' @param conc A numeric vector of concentration values
#' @param time A numeric vector of time values, parallel to \code{conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param lamznpt Number of points to use for the lambda z calculation, counted from the last measurable concentration.
#'  Must be a single integer greater than 1 and less than or equal to the length of \code{time} and \code{conc}.
#'  Default is NULL, lamznpt is then calculated by \code{selectPoints}.
#' @param dose A single numeric
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item All checks for \code{AUCInfObs} apply
#'      \item \code{dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{dose} is \code{NA}, 0 or less, \code{NA} will be returned 
#' }
#' @title Apparent Clearance (Observed)
#' @return apparent clearance (single numeric)
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(Theoph, Subject == 1)
#' CLObs(conc = Theoph1$conc, time = Theoph1$time, dose = Theoph1$Dose[1])

CLObs <- function(conc, time, dose, lamznpt = NULL) {
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    checkOrderedVector(time, description = "time", functionName = "CLObs")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "CLObs")
    checkSingleNumeric(dose, "dose", functionName = "CLObs")
    
    if (is.na(dose) || dose <= 0) {
        return(NA_real_)
    }
    if (is.null(lamznpt)) { 
        lamznpt <- selectPoints(conc = conc, time = time)
    } else {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "CLObs")
    }
    # aucInfObs : Area under curve extrapolated to infinity
    aucInfObs <- AUCInfObs(conc = conc, time = time, lamznpt = lamznpt)
    cl <- clearance(AUCInf = aucInfObs, dose = dose)
    return(cl)
}
