
# Date of last change: 30/03/2016
# Last changed by: ccampbell
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Mean Residence time to infinity (predicted) for steady-state data
#'
#' Calculate the mean residence time to infinity (predicted), for steady-state models of type "M3".
#' The value returned is 
#' \deqn{\frac{(AUMC_{tau} + tau * (AUCInfPred - AUC_{tau}))}{ AUC_{tau} } - dof/2}.  
#' AUC_{tau} is the partial area under the time-concentration curve up to tau (similarly for AUMC_{tau}).
#' All error checks for \code{AUCInfPred} apply.  
#' If \code{tau} or \code{dof} is not a numeric of length 1, an exception will be generated.
#' 
#' @inheritParams AUCPartial
#' @param tau Dosing interval.  Must lie in the range of time values, otherwise an exception will be generated
#' @param duration Duration of infusion, single numeric
#' @return single numeric vector
#' @author Mango Solutions
#' @keywords math
#' @noRd

MRTInfPredSS <- function(conc, time, tau, duration, 
    lamznpt = NULL, lambdaZStats = NULL, 
    usepoints = NULL, excpoints = FALSE, minpoints = 3, addt0 = FALSE, inter = "Linear", 
    useObs = FALSE, maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1) {
    
    checkSingleNumeric(duration, description = "duration of infusion")
    
    # AUMCtau : single numeric with partial area under moment curve from first time to tau
    # AUCtau : single numeric with partial area under curve from first time to tau
    
    AUMCtau <- AUMCPartial(conc = conc, time = time, endtime = tau, lamznpt = lamznpt, addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter, 
        maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, numhalflife = numhalflife)
    
    AUCtau <- AUCPartial(conc = conc, time = time, endtime = tau, lamznpt = lamznpt, addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter, 
        maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, numhalflife = numhalflife)    

    AUCip <- AUCInfPred(conc = conc, time = time, lamznpt = lamznpt, addt0 = addt0, 
        lambdaZStats = lambdaZStats, inter = inter)
    
    MRTipss <- ((AUMCtau + tau * (AUCip - AUCtau)) / AUCtau) - duration / 2
    
    return(MRTipss)
}
