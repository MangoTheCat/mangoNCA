# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 03/02/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Function to Calculate Volume of Distribution at Steady State (Predicted)
#'
#' Calculates volume distribution at steady-state (predicted).
#' This is a convenience function for when using the R package. 
#' \code{\link{ncaComplete}} calls VSS since MRTInfPred and CLPred have already been calculated.
#' \code{VSSPred} calls \code{\link{VSS}} after calling \code{\link{MRTInfPredSD}}, 
#' \code{\link{AUCinfpred}} and \code{\link{clearance}}.
#' 
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param numPoints Number of points to use for the lambda z calculation(s)
#' @param Dose Single numeric with dose amount
#' @param Dof duration of infusion
#' @title Volume Distribution at Steady-State (predicted)
#' @return Single numeric value 
#' @export
#' @note All input checks / error handling for CLPred and MRTInfPredSD apply.   
#' The formula used to calculate this quantity is 
#' \deqn{ MRTPredSD_{\inf} * CLPred }  
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' VSSPred( Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1], Dof = 3 )

VSSPred <- function(Conc, Time, Dose, Dof, numPoints = NULL, Safe = TRUE)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "VSSPred")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "VSSPred")
    checkSingleNumeric(Dose, "Dose", functionName = "VSSPred")
    checkSingleNumeric(Dof, "Duration of Infusion", functionName = "VSSPred")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "VSSPred")
        
    }
    
    MRT <- MRTInfPredSD(Conc = Conc, Time = Time, numPoints = numPoints, Dof = Dof)
    
    AUCinfpred <- AUCInfPred(Conc = Conc, Time = Time, 
        numPoints = numPoints, calculation = "standard", Safe = Safe)
        
    CL <- clearance(AUCInf = AUCinfpred, Dose = Dose)
    
    VSSpred <- VSS(MRT = MRT, CL = CL, Safe = Safe)
    
    return(VSSpred)
}


#' Function to Calculate Volume of Distribution at Steady State (Observed)
#'
#' Calculates volume distribution at steady-state (observed).
#' This is a convenience function for when using the R package. 
#' \code{\link{ncaComplete}} calls VSS directly since MRTInfObs and CLObs have already been calculated.
#' \code{VSSObs} calls \code{\link{VSS}} after calling \code{\link{MRTInfObsSD}}, \code{\link{AUCinfobs}} and \code{\link{clearance}}.
#' 
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param numPoints Number of points to use for the lambda z calculation(s)
#' @param Dose Single numeric with dose amount
#' @param Dof duration of infusion
#' @title Volume Distribution at Steady-State (observed)
#' @return Single numeric value 
#' @export
#' @note All input checks / error handling for CLPred and MRTInfPredSD apply.  The formula used to calculate this 
#' quantity is \deqn{ MRTPredSD_{\inf} * CLPred }  
#' @author Mango Solutions
#' @keywords math
#' @examples
#' Theoph1 <- subset(  Theoph, Subject == 1)
#' VSSObs( Conc = Theoph1$conc, Time = Theoph1$Time, numPoints = 3, Dose = Theoph1$Dose[1], Dof = 3 )

VSSObs <- function(Conc, Time, Dose, Dof, numPoints = NULL, Safe = TRUE)
{
    # check Conc, Time, numPoints and Dose, and calculate numPoints if missing
    
    checkOrderedVector(Time, description = "Time", functionName = "VSSObs")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "VSSObs")
    checkSingleNumeric(Dose, "Dose", functionName = "VSSObs")
    checkSingleNumeric(Dof, "Duration of Infusion", functionName = "VSSObs")
    
    if(is.na(Dose) || Dose <= 0 )
    {
        return(as.numeric(NA))
        
    }
    
    if(is.null(numPoints)) 
    { 
        numPoints <- selectPoints(Conc = Conc, Time = Time)
        
    } else
    {
        checkSingleNumeric(numPoints, "Number of Points lambdaz", functionName = "VSSObs")
        
    }
    
    MRT <- MRTInfObsSD(Conc = Conc, Time = Time, numPoints = numPoints, Dof = Dof)
    
    AUCinfobs <- AUCInfObs(Conc = Conc, Time = Time, 
        numPoints = numPoints, calculation = c("standard"), Safe = Safe)
        
    CL <- clearance(AUCInf = AUCinfobs, Dose = Dose)
    
    VSSObs <- VSS(MRT = MRT, CL = CL, Safe = Safe)
    
    return(VSSObs)
}


#' Calculate Volume of Distribution at Steady-State.
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
#' VSS(MRT = 20, CL = 0.1)

VSS <- function(MRT, CL, Safe = TRUE)
{

    checkSingleLogical(Safe, description = "Safe", functionName = "VSS")
    
    if( Safe ) {
    
        checkSingleNumeric(MRT, description = "MRT", "VSS")
        checkSingleNumeric(CL, description = "CL", "VSS")
    
    }

    VSS <- MRT * CL
    
    return(VSS)
}


