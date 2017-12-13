# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 04/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Terminal Volume Distribution, Single Dose (predicted)
#'
#' Calculates volume distribution for single dose from terminal phase (predicted).
#'
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param numPoints Number of points to use for the lambda z calculation(s)
#' @param Dose Single numeric with dose amount
#' @param Dof duration of infusion (default NULL)
#' @title Terminal Volume Distribution, Single Dose (predicted)
#' @note All input checks / error handling for LambdaZStatistics and AUCInfPred apply.  
#' The formula used to calculate this quantity is 
#' \deqn{ Dose / {\lambda}_z * AUC({\inf})_Pred }  
#' @return Single numeric value 
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' VZPred(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1])
#' @export

VZPred <- function(Conc, Time, Dose, numPoints = NULL)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "VZPred")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "VZPred")
    checkSingleNumeric(Dose, "Dose", functionName = "VZPred")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "VZPred")
        
    }
    
    Lambda_Z_Stats <- lambdaZStatistics(Conc = Conc, Time = Time, numPoints = numPoints)
    
    AUCinfpred <- AUCInfPred(Conc = Conc, Time = Time, numPoints = numPoints, calculation = "standard")

    VZ_F <- VZ(lambdaz = Lambda_Z_Stats$Lambdaz, AUCInf = AUCinfpred, Dose = Dose) # volume of distribution HL

    return(VZ_F)
}



#' Terminal Volume Distribution, Single Dose (observed)
#'
#' Calculates volume distribution for single dose from terminal phase (observed).
#'
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param numPoints Number of points to use for the lambda z calculation(s)
#' @param Dose Single numeric with dose amount
#' @param Dof duration of infusion (default NULL)
#' @title Terminal Volume Distribution, Single Dose (observed)
#' @note All input checks / error handling for LambdaZStatistics and AUCInfPred apply.  
#' The formula used to calculate this quantity is 
#' \deqn{ Dose / {\lambda}_z * AUC({\inf})_Obs }  
#' @return Single numeric value
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' VZObs(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1])
#' @export

VZObs  <- function(Conc, Time, Dose, numPoints = NULL)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "VZObs")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "VZObs")
    checkSingleNumeric(Dose, "Dose", functionName = "VZObs")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "VZObs")
        
    }
    
    Lambda_Z_Stats <- lambdaZStatistics(Conc = Conc, Time = Time, numPoints = numPoints)
    
    AUCinfobs  <-  AUCInfObs(Conc = Conc, Time = Time, numPoints = numPoints, calculation = "standard")
    
    VZ_F <- VZ(lambdaz = Lambda_Z_Stats$Lambdaz, AUCInf = AUCinfobs, Dose = Dose) # volume of distribution HL

    return(VZ_F)
}




#' Calculate Volume of Distribution for single dose from terminal phase.
#'
#' This function simply calculates VSS from MRT and CL when they have already been  calculated.
#' \deqn{ VSS = MRT * CL } 
#'
#' @param MRT Single numeric value of time
#' @param CL Single numeric value of clearance
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @title Volume Distribution at Steady-State (observed)
#' @return Single numeric value  
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples
#' VZ(lambdaz = 0.001, AUCInf = 1000, Dose = 10)

VZ <- function(lambdaz, AUCInf, Dose, Safe = TRUE)
{

    checkSingleLogical(Safe, description = "Safe", functionName = "VZ")
    
    if( Safe ) {
    
        checkSingleNumeric(lambdaz, description = "lambdaz", functionName = "VZ")
        checkSingleNumeric(AUCInf, description = "AUCInf", functionName = "VZ")
        checkSingleNumeric(Dose, description = "Dose", functionName = "VZ")
    
    }

    VZ <- Dose / (lambdaz * AUCInf) # volume of distribution HL

    return(VZ)
}
