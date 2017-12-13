# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Mean Residence Time (Single Dose)
#'
#' Calculates mean residence time from AUMC, AUC and duration of infusion.
#' The value returned is: 
#' \deqn{\frac{AUMC}{AUC} - \frac{Dof}{2}}
#'
#' @param AUC Single numeric value, area under the concentration time curve
#' @param AUMC Single numeric value, area under the concentration time moment curve
#' @param Dof Duration of infusion, single numeric value
#' @title Calculate Mean Residence Time
#' @return A single numeric vector.  
#' @author Mango Solutions
#' @export
#' @keywords math


MRTSD <- function(AUC, AUMC, Dof = 0)
{    
    checkSingleNumeric(AUC, description = "AUC")
    checkSingleNumeric(AUMC, description = "AUMC")
    checkSingleNumeric(Dof, description = "Duration of infusion")
    
    if( Dof < 0 )
    {
        return(as.numeric(NA))
    }
    
    mrt <- AUMC / AUC - Dof / 2
    
    return(mrt)
}


#' Mean Residence Time (Single Dose) Predicted
#'
#' Calculates the mean residence time to infinity (predicted), for steady-state models of type "M3".
#' @param Conc Vector of concentrations
#' @param Time Vector of times.  Must be sorted, and without duplicates
#' @param numPoints Number of points to use for lambda z calculation. See \code{\link{lambdaZStatistics}}
#' @param Dof Duration of infusion, single numeric value
#' @param addT0 Single logical value determining whether T = 0 should be added
#' @title Calculate Mean Residence Time to infinity (predicted) for steady-state data, model M3
#' @return A single numeric vector.  The value returned is:
#' \deqn{\frac{AUMCInfPred}{AUCInfPred} - \frac{Dof}{2}}
#' @author Mango Solutions
#' @export
#' @keywords math

MRTInfPredSD <- function(Conc, Time, numPoints, Dof = 0, addT0 = FALSE)
{    
    checkSingleNumeric(Dof, description = "Duration of infusion")
    checkSingleNumeric(numPoints, description = "Number of Points")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", "MRTInfPredSD") 
    
    if( Dof < 0 )
    {
        return(as.numeric(NA))
    }
    
    AUMCIP <- AUCInfPred(Conc = Conc, Time = Time, numPoints = numPoints, calculation = "moment", addT0 = addT0)
    AUCIP <- AUCInfPred(Conc = Conc, Time = Time, numPoints = numPoints, addT0 = addT0)
    
    mrtp <- MRTSD(AUC = AUCIP, AUMC = AUMCIP, Dof = Dof)
    
    return(mrtp)
}


#' Mean Residence Time (Single Dose) Observed
#'
#' Calculates the mean residence time to infinity (observed), for steady-state models of type "M3".
#' @param Conc Vector of concentrations
#' @param Time Vector of times.  Must be sorted, and without duplicates
#' @param numPoints Number of points to use for lambda z calculation. See \code{\link{lambdaZStatistics}}
#' @param Dof Duration of infusion. Must be a 
#' @title Calculate Mean Residence Time to infinity (observed) for steady-state data, model M3
#' @return A single numeric vector.  The value returned is:
#' \deqn{\frac{AUMCInfObs}{AUCInfObs} - \frac{Dof}{2}}
#' @author Mango Solutions
#' @export
#' @keywords math

MRTInfObsSD <- function(Conc, Time, numPoints, Dof = 0, addT0 = FALSE)
{    
    checkSingleNumeric(Dof, description = "Duration of infusion")
    checkSingleNumeric(numPoints, description = "Number of Points")
    checkNumericSameLength(Time, Conc, "Time", "Concentration", "MRTInfObs") 
    
    if( Dof < 0 )
    {
        return(as.numeric(NA))
    }
        
    AUMCIO <- AUCInfObs(Conc = Conc, Time = Time, numPoints = numPoints, calculation = "moment", addT0 = addT0)
    AUCIO <- AUCInfObs(Conc = Conc, Time = Time, numPoints = numPoints, addT0 = addT0)
    
    mrto <- MRTSD(AUC = AUCIO, AUMC = AUMCIO, Dof = Dof)
    
    return(mrto)
}
