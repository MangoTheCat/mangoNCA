# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 04/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Clearance
#'
#' Calculates Clearance, which is equal to administered Dose divided by AUCInf.
#'
#' @title Clearance
#' @param AUCInf A numeric value
#' @param Dose A numeric value
#' @param Safe A single logical
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item \code{AUCInf} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{AUCInf} is \code{NA}, 0 or less, \code{NA} will be returned for Observed
#'      \item \code{Dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{Dose} is \code{NA}, 0 or less, \code{NA} will be returned for both
#' }
#' @return Single numeric of clearance
#' @export
#' @author Mango Business Solutions
#' @keywords math
#' @examples 
#' clearance(AUCInf = 100, Dose = 10)

clearance <- function(AUCInf, Dose, Safe = TRUE)
{

    checkSingleLogical(Safe, description = "Safe", functionName = "clearance")
    
    if( Safe ) 
    {
        checkSingleNumeric(AUCInf, description = "AUCInf", functionName = "clearance")
        checkSingleNumeric(Dose, description = "Dose", functionName = "clearance")
    
    }
    
    clearanceResult <- as.numeric(NA)

    if( !is.na(AUCInf) && AUCInf > 0 && !is.na(Dose) && Dose > 0) 
    {
        clearanceResult <- Dose/AUCInf
   
    }
    
    return(clearanceResult)

}



#' Calculates apparent clearance (predicted), which is equal to administered dose divided by AUC0Inf_Pred.
#' 
#' @param Conc A numeric vector of concentration values
#' @param Time A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param numPoints Number of points to use for the lambda z calculation, counted from the last measurable concentration.
#'  Must be a single integer greater than 1 and less than or equal to the length of \code{Time} and \code{Conc}.
#'  Default is NULL, numPoints is then calculated by \code{selectPoints}.
#' @param Dose A single numeric
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item All checks for \code{AUCInfPred} apply
#'      \item \code{Dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{Dose} is \code{NA}, 0 or less, \code{NA} will be returned 
#' }
#' @title Apparent Clearance (predicted)
#' @return apparent clearance (single numeric)
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' CLPred(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1])

CLPred <- function(Conc, Time, Dose, numPoints = NULL)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "CLPred")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "CLPred")
    checkSingleNumeric(Dose, "Dose", functionName = "CLPred")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "CLPred")
        
    }
    
    # aucInfPred : Area under curve extrapolated to infinity
    
    aucInfPred <- AUCInfPred(Conc = Conc, Time = Time, numPoints = numPoints)
    
    cl <- clearance(AUCInf = aucInfPred, Dose = Dose)
    
    return(cl)
}


#' Calculates apparent clearance (observed), which is equal to administered dose divided by AUC0Inf_Obs.
#' 
#'
#' @param Conc A numeric vector of concentration values
#' @param Time A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param numPoints Number of points to use for the lambda z calculation, counted from the last measurable concentration.
#'  Must be a single integer greater than 1 and less than or equal to the length of \code{Time} and \code{Conc}.
#'  Default is NULL, numPoints is then calculated by \code{selectPoints}.
#' @param Dose A single numeric
#' @note The following additional input processing/checks are performed:
#' \enumerate{
#'      \item All checks for \code{AUCInfObs} apply
#'      \item \code{Dose} must be a numeric vector of length 1, otherwise an exception will be generated.  If
#'          \code{Dose} is \code{NA}, 0 or less, \code{NA} will be returned 
#' }
#' @title Apparent Clearance (Observed)
#' @return apparent clearance (single numeric)
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples 
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' CLObs( Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1])

CLObs <- function(Conc, Time, Dose, numPoints = NULL)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "CLObs")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "CLObs")
    checkSingleNumeric(Dose, "Dose", functionName = "CLObs")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "CLObs")
        
    }
    
    # aucInfObs : Area under curve extrapolated to infinity
    
    aucInfObs <- AUCInfObs(Conc = Conc, Time = Time, numPoints = numPoints)
    
    cl <- clearance(AUCInf = aucInfObs, Dose = Dose)
    
    return(cl)
}
