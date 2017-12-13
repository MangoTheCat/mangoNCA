# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 03/02/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' @title Volume Distribution at Steady-State (predicted)
#' 
#' Function to Calculate Volume of Distribution at Steady State (Predicted)
#'
#' Calculates volume distribution at steady-state (predicted).
#' This is a convenience function for when using the R package. 
#' \code{\link{ncaComplete}} calls VSS since MRTInfPred and CLPred have already been calculated.
#' \code{VSSPred} calls \code{\link{VSS}} after calling \code{\link{MRTInfPredSD}}, 
#' \code{\link{AUCInfPred}} and \code{\link{clearance}}.
#' 
#' @inheritParams getNCAnalysis
#' @return Single numeric value 
#' @export
#' @note All input checks / error handling for CLPred and MRTInfPredSD apply.   
#' The formula used to calculate this quantity is 
#' \deqn{ MRTPredSD_{\inf} * CLPred }  
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' VSSPred(conc = Theoph1$conc, time = Theoph1$time, dose = Theoph1$Dose[1], duration = 3)

VSSPred <- function(conc, time, dose, duration, lamznpt = NULL) {
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    
    checkOrderedVector(time, description = "time", functionName = "VSSPred")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "VSSPred")
    checkSingleNumeric(dose, "dose", functionName = "VSSPred")
    checkSingleNumeric(duration, "Duration of Infusion", functionName = "VSSPred")
    
    if (is.na(dose) || dose <= 0 ) {
        return(as.numeric(NA))
        
    }
    
    if (is.null(lamznpt)) { 
        lamznpt <- selectPoints(conc = conc, time = time)
    } else {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "VSSPred")
    }
    
    MRT <- MRTInfPredSD(conc = conc, time = time, lamznpt = lamznpt, duration = duration)
    
    AUCinfpred <- AUCInfPred(conc = conc, time = time, 
        lamznpt = lamznpt, calculation = "standard")
        
    CL <- clearance(AUCInf = AUCinfpred, dose = dose)
    
    VSSpred <- VSS(MRT = MRT, CL = CL)
    
    return(VSSpred)
}


#' Function to Calculate Volume of Distribution at Steady State (Observed)
#'
#' Calculates volume distribution at steady-state (observed).
#' This is a convenience function for when using the R package. 
#' \code{\link{ncaComplete}} calls VSS directly since MRTInfObs and CLObs have already been calculated.
#' \code{VSSObs} calls \code{\link{VSS}} after calling \code{\link{MRTInfObsSD}}, \code{\link{AUCInfObs}} and \code{\link{clearance}}.
#' 
#' @inheritParams getNCAnalysis
#' @title Volume Distribution at Steady-State (observed)
#' @return Single numeric value 
#' @export
#' @note All input checks / error handling for CLPred and MRTInfPredSD apply.  The formula used to calculate this 
#' quantity is \deqn{ MRTPredSD_{\inf} * CLPred }  
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' VSSObs(conc = Theoph1$conc, time = Theoph1$time, lamznpt = 3, dose = Theoph1$Dose[1], duration = 3 )

VSSObs <- function(conc, time, dose, duration, lamznpt = NULL) {
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    
    checkOrderedVector(time, description = "time", functionName = "VSSObs")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "VSSObs")
    checkSingleNumeric(dose, "dose", functionName = "VSSObs")
    checkSingleNumeric(duration, "Duration of Infusion", functionName = "VSSObs")
    
    if (is.na(dose) || dose <= 0 ) {
        return(as.numeric(NA))
    }
    
    if (is.null(lamznpt)) {
        lamznpt <- selectPoints(conc = conc, time = time)
    } else {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "VSSObs")
    }
    
    MRT <- MRTInfObsSD(conc = conc, time = time, lamznpt = lamznpt, duration = duration)
    
    AUCinfobs <- AUCInfObs(conc = conc, time = time, 
        lamznpt = lamznpt, calculation = c("standard"))
        
    CL <- clearance(AUCInf = AUCinfobs, dose = dose)
    
    VSSObs <- VSS(MRT = MRT, CL = CL)
    
    return(VSSObs)
}


#' Calculate Volume of Distribution at Steady-State.
#'
#' This function simply calculates VSS from MRT and CL when they have already been  calculated.
#' \deqn{ VSS = MRT * CL } 
#'
#' @param MRT Single numeric value of time
#' @param CL Single numeric value of clearance
#' @title Volume Distribution at Steady-State (observed)
#' @return Single numeric value  
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples
#' VSS(MRT = 20, CL = 0.1)

VSS <- function(MRT, CL) {
     
    checkSingleNumeric(MRT, description = "MRT", "VSS")
    checkSingleNumeric(CL, description = "CL", "VSS")

    VSS <- MRT * CL
    
    return(VSS)
}

