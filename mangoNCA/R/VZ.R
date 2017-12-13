# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' @title Terminal Volume Distribution, Single dose (predicted)
#' 
#' Terminal Volume Distribution, Single dose (predicted)
#'
#' Calculates volume distribution for single dose from terminal phase (predicted).
#'
#' @inheritParams getNCAnalysis
#' @note All input checks / error handling for LambdaZStatistics and AUCInfPred apply.  
#' The formula used to calculate this quantity is 
#' \deqn{ dose / {\lambda}_z * AUC({\inf})_Pred }  
#' @return Single numeric value 
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' VZPred(conc = Theoph1$conc, time = Theoph1$time, dose = Theoph1$Dose[1])
#' @export

VZPred <- function(conc, time, dose, lamznpt = NULL) {
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    
    checkOrderedVector(time, description = "time", functionName = "VZPred")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "VZPred")
    checkSingleNumeric(dose, "dose", functionName = "VZPred")
    
    if(is.na(dose) || dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(lamznpt)) 
    { 
        lamznpt <- selectPoints(conc = conc, time = time)
        
    } else
    {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "VZPred")
        
    }
    
    Lambda_Z_Stats <- lambdaZStatistics(conc = conc, time = time, lamznpt = lamznpt)
    
    AUCinfpred <- AUCInfPred(conc = conc, time = time, lamznpt = lamznpt, calculation = "standard")

    VZ_F <- VZ(lambdaz = Lambda_Z_Stats$Lambdaz, AUCInf = AUCinfpred, dose = dose) # volume of distribution HL

    return(VZ_F)
}



#' Terminal Volume Distribution, Single dose (observed)
#'
#' Calculates volume distribution for single dose from terminal phase (observed).
#'
#' @inheritParams getNCAnalysis
#' @title Terminal Volume Distribution, Single dose (observed)
#' @note All input checks / error handling for LambdaZStatistics and AUCInfPred apply.  
#' The formula used to calculate this quantity is 
#' \deqn{ dose / {\lambda}_z * AUC({\inf})_Obs }  
#' @return Single numeric value
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' VZObs(conc = Theoph1$conc, time = Theoph1$time, dose = Theoph1$Dose[1])
#' @export

VZObs  <- function(conc, time, dose, lamznpt = NULL) {
    # check conc, time, lamznpt and dose, and calculate lamznpt if missing
    
    checkOrderedVector(time, description = "time", functionName = "VZObs")
    checkNumericSameLength(time, conc, "time", "concentration", functionName = "VZObs")
    checkSingleNumeric(dose, "dose", functionName = "VZObs")
    
    if(is.na(dose) || dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(lamznpt)) 
    { 
        lamznpt <- selectPoints(conc = conc, time = time)
        
    } else
    {
        checkSingleNumeric(lamznpt, "Number of Points lambdaz", functionName = "VZObs")
        
    }
    
    Lambda_Z_Stats <- lambdaZStatistics(conc = conc, time = time, lamznpt = lamznpt)
    
    AUCinfobs  <-  AUCInfObs(conc = conc, time = time, lamznpt = lamznpt, calculation = "standard")
    
    VZ_F <- VZ(lambdaz = Lambda_Z_Stats$Lambdaz, AUCInf = AUCinfobs, dose = dose) # volume of distribution HL

    return(VZ_F)
}




#' Calculate Volume of Distribution for single dose from terminal phase.
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
#' VZ(lambdaz = 0.001, AUCInf = 1000, dose = 10)

VZ <- function(lambdaz, AUCInf, dose){
    
        checkSingleNumeric(lambdaz, description = "lambdaz", functionName = "VZ")
        checkSingleNumeric(AUCInf, description = "AUCInf", functionName = "VZ")
        checkSingleNumeric(dose, description = "dose", functionName = "VZ")

    VZ <- dose / (lambdaz * AUCInf) # volume of distribution HL

    return(VZ)
}
