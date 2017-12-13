# SVN revision: $Rev: 23459 $
# Date of last change: $LastChangedDate: 2010-12-07 11:23:47 +0000 (Tue, 07 Dec 2010) $
# Last changed by: $LastChangedBy: fgochez $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Mean Residence Time to infinity (predicted) for steady-state data
#'
#' Calculate the mean residence time to infinity (predicted), for steady-state models of type "M3".
#' The value returned is 
#' \deqn{\frac{(AUMC_{tau} + tau * (AUCInfPred - AUC_{tau}))}{ AUC_{tau} } - dof/2}.  
#' AUC_{tau} is the partial area under the time-concentration curve up to tau (similarly for AUMC_{tau}).
#' All error checks for \code{AUCInfPred} apply.  
#' If \code{tau} or \code{dof} is not a numeric of length 1, an exception will be generated.
#' 
#' @param Conc Vector of concentrations
#' @param Time Vector of times.  Must be sorted, and without duplicates
#' @param numPoints Number of points to use for lambda z calculation. See \code{\link{lambdaZStatistics}}
#' @param tau Dosing interval.  Must lie in the range of time values, otherwise an exception will be generated
#' @param dof Duration of infusion, single numeric
#' @return single numeric vector
#' @author Mango Solutions
#' @keywords math
#' @noRd

MRTInfPredSS <- function( Conc, Time, numPoints, tau, dof, addT0 = TRUE)
{  
    checkSingleNumeric(dof, description = "duration of infusion")
    
    # AUMCtau : single numeric with partial area under moment curve from first time to tau
    # AUCtau : single numeric with partial area under curve from first time to tau
    
    AUMCtau <- AUMCPartial(Conc = Conc, Time = Time, endTime = tau, numPoints = numPoints, addT0 = addT0)
    
    AUCtau <- AUCPartial(Conc = Conc, Time = Time, endTime = tau, numPoints = numPoints, addT0 = addT0)    

    AUCip <- AUCInfPred(Conc = Conc, Time = Time, numPoints = numPoints, addT0 = addT0)
    
    MRTipss <- ((AUMCtau + tau * (AUCip - AUCtau)) / AUCtau) - dof / 2
    
    return(MRTipss)
}
