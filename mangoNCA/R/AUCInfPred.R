# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the concentration time Curve from time = 0 to time = Inf using extrapolation from Predicted Clast
#'
#' AUCInfPred calculates the area under a time/concentration (or first moment) curve from the first point up to  
#' "infinity" using the linear trapezium rule.  This is performed by calculating the area under the concentration-time 
#' curve until the last measurable concentration (see \code{\link{AUCLast}}), and then adding an extrapolated 
#' AUC which is the area bounded by the exponential decay curve with rate constant -lambdaz from observed Clast.  
#' Analogous calculations for AUMC are used if appropriate. \cr
#' There are two methods for calculating AUC and AUMC.
#' LAMZstats can be calculated within the function. In this case, lamznpt should be provided.
#' lamznpt must be a single integer greater than minPonts (which should be at least 2) and less than or equal to the length of \code{time} and \code{conc}.
#' lamznpt is NULL by default. Must be provided if lambdaZStats is not.
#' The lambdaz statistics may be provided directly to this function; it should have length 1 numeric elements:
#' \enumerate{
#'      \item Lambdaz
#'      \item intercept
#'      \item R2
#'      \item R2ADJ
#'      \item CORRXY
#'      \item LAMZHL
#'      \item LAMZLL
#'      \item LAMZUL
#'      \item lamznpt
#' }
#' Use \code{unlist} to pass \code{lambdaZStatistics} output to this function.
#'
#' @title Calculate Area Under Curve or Moment Curve To "Infinity" (Predicted)
#' @inheritParams AUCInfObs
#' @return A numeric vector with a single element holding the area under the concentration/time curve until "infinity".
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item all error checks for \code{AUCLast} apply
#'      \item If any relevant lambda z calculations are NA, NA will be returned
#'      \item An exception will be generated if lamznpt is not a single integer numeric between 2 and  \code{length(conc)} (inclusive)
#'  }
#' The algorithm for calculating the extrapolated AUC is as follows:
#' \deqn{ClastExtrap = \exp(-\lambda_z * t_{upper}) * c_0} 
#' \deqn{AUCPredInf = AUCLast + ClastExtrap / \lambda_z}   
#' For the extrapolated AUMC, the calculation is
#' \deqn{AUMCPredInf = AUMCLast + ClastExtrap * tlast / \lambda_z + ClastExtrap / \lambda_z^2}
#' Here \eqn{c_0} is the exponential of the intercept term from the lambda z regression, and \eqn{t_{upper}} is the
#' last time point used in that regression
#' @seealso \code{\link{lambdaZStatistics}}, \code{\link{AUCLast}}
#' @author Mango Solutions
#' @export
#' @keywords math
#' @examples 
#'      Theoph1 <- subset(Theoph, Subject == 1)
#'      AUCInfPred(Theoph1$conc, Theoph1$time, lamznpt = 4)

AUCInfPred <- function(conc, time, lamznpt = NULL, lambdaZStats = NULL, 
    calculation = c("standard", "moment"), minpoints = 3, addt0 = FALSE, inter = "Linear") {
    
    checkNumericSameLength(time, conc, "time", "concentration", "AUCInfPred")  
    
    checkOrderedVector(time, description = "time", functionName = "AUCInfPred")
    
    checkSingleLogical(addt0, description = "addt0", functionName = "AUCInfPred")
    
    checkSingleCharacter(inter, description = "inter", functionName = "AUCInfPred")
    

    # calc will hold the chosen method of calculation
    
    calc <- match.arg(calculation)
    
    aucip <- as.numeric(NA)
 
    if (is.null(lambdaZStats)) {
        
        # check that lamznpt is valid, return NA if it is not  
        
        checkSingleNumeric(lamznpt, description = "lamznpt")
        
        if( !(lamznpt >= minpoints && lamznpt <= length(time) && floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste("Invalid value of lamznpt:", lamznpt, "in AUCInfPred", collapse = " "))
            
            return(aucip)
        }
    }
    
    # Add T = 0 if it is missing and remove missing values
    
    timeconc <- try(stripTrailingZeros(conc = conc, time = time, addt0 = addt0), silent = TRUE)
    
    if (is(timeconc, "try-error")) {
        
        stop(paste("Error during data cleaning in AUCInfPred", as.character(timeconc), collapse = "\n"))
    }
    
    # timeconc : data.frame with columns consisting of the last lamznpt values of time and conc
    
    if (sum(timeconc$conc, na.rm = TRUE) == 0) {
        
        return(aucip)
    }

    if (!is.null(lambdaZStats)) {
        
        if (!is.null(lamznpt)) { warning("both lamznpt and lambdaZStats provided to AUCInfPred, ignoring lamznpt") }
        
        if (is(lambdaZStats, "list")) {
            
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {

        # check that there are at least 3 rows after Cmax, otherwise return NA
        
        if( !testTrailPoints(conc = timeconc$conc, time = timeconc$time, minpoints = minpoints) ) {
            
            return(aucip)
        }
        
        # lamdbdaZStats : list with value of lambda z and related statistics
        
        lambdaZStats <- unlist(lambdaZStatistics( conc = timeconc$conc, time = timeconc$time, lamznpt = lamznpt ))
        
    }
    
    # cLastExtrap : extrapolated/predicted value of Clast  
    
    tLast <- ClastTlast(conc = timeconc$conc, time = timeconc$time)[["tlast"]]
    
    cLastExtrap <- exp(-lambdaZStats["Lambdaz"] * tLast) * lambdaZStats["intercept"]
        
    if (any(is.na(cLastExtrap) | is.na(tLast))) { calc <- "na" }
    
    switch(calc, "standard" = {
            
        # aucLast : single numeric holding value of AUCLast(conc, time)    
        
        aucLast <- AUCLast(conc = timeconc$conc, time = timeconc$time, addt0 = addt0, inter = inter)  
        
        # aucExtraPred : single numeric with extrapolated AUC
        
        aucExtraPred <-  cLastExtrap / lambdaZStats["Lambdaz"]
        
        aucip[1] <- aucLast + aucExtraPred
    },
        "moment" = {
        # aumcLast : single numeric holding value of AUCLast(conc * time, time) (area under moment curve)
        
        aumcLast <- AUCLast(conc = timeconc$conc * timeconc$time , time = timeconc$time, addt0 = addt0, inter = inter)
    
        # aumcExtraPred : single numeric with extrapolated AUMC
        
        aumcExtraPred <- cLastExtrap * tLast / lambdaZStats["Lambdaz"] + cLastExtrap / lambdaZStats["Lambdaz"]^2
        
        aucip[1] <- aumcLast + aumcExtraPred
        }, 
        "na" = { warning("missing values at tLast") },
        
        stop("only methods standard and moment recognized for calc")
    )
    
    return(aucip)
}
