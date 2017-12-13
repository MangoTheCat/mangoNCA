 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area under the concentration time Curve from T = 0 to T = Inf using extrapolation from Observed Clast
#' 
#' AUCInfObs calculates the area under a time/concentration (or first moment) curve from the first point up to  
#' "infinity" using the linear trapezium rule.  This is performed by calculating the area under the concentration-time
#' curve until the last measurable concentration (see \code{\link{AUCLast}}), and then adding an extrapolated 
#' AUC which is the area bounded by the exponential decay curve with rate constant -lambdaz from observed Clast.  
#' Analogous calculations for AUMC are used if appropriate. \cr
#' There are two methods for calculating AUC and AUMC.
#' LAMZstats can be calculated within the function. In this case, lamznpt should be provided.
#' lamznpt must be a single integer greater than minPonts (which should be at least 2) and less than or equal to the length of \code{time} and \code{conc}.
#' lamznpt is NULL by default. Must be provided if lambdaZStats is not.
#' The lambdaz statistics may be provided directly to this function; it should be a length 9 numeric vector with named elements:
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
#' @title Calculate Area Under Curve or Moment Curve To "Infinity" (Observed)
#' @param conc A numeric vector of concentration values
#' @param time A numeric vector of time values, parallel to \code{conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param lamznpt Number of points to use for the lambdaz calculation, counted from the end of the concentration/time vectors.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if lamznpt is not.
#' @param calculation Must be either the string "standard" or "moment".  If the former, calculates the standard area under
#' the curve.  For the latter, it will calculate the area under the moment curve
#' @param minpoints Minimum number of points to use for the LAMZcalculation(s), 3 by default. Single positive integer.
#' @param addt0 Single logical value declaring whether T0 should be added if missing (default \code{TRUE}).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' @return A numeric vector with a single element holding the area under the concentration/time curve until "infinity".
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item all error checks for \code{AUCLast} apply
#'      \item If any relevant lambda z calculations are NA, NA will be returned
#'      \item An exception will be generated if lamznpt is not a single integer numeric between 2 and  \code{length(conc)} (inclusive)
#'  }
#' The algorithm for calculating the observed AUC is as follows:
#' \deqn{AUCPredInf = AUCLast + Clast / \lambda_z}   
#' For the observed AUMC, the calculation is
#' \deqn{AUMCPredInf = AUMCLast + Clast * tlast / \lambda_z + Clast / \lambda_z^2}
#' Here \eqn{c_0} is the exponential of the intercept term from the lambda z regression, and \eqn{t_{upper}} is the
#' last time point used in that regression
#' @seealso \code{\link{lambdaZStatistics}}, \code{\link{AUCLast}}
#' @author Mango Solutions
#' @export
#' @keywords math
#' @examples 
#'      Theoph1 <- subset(Theoph, Subject == 1)
#'      AUCInfObs(conc = Theoph1$conc, time = Theoph1$time, lamznpt = 4, calculation = "standard")


AUCInfObs <- function(conc, time, lamznpt = NULL, lambdaZStats = NULL, 
    calculation = c("standard", "moment"), minpoints = 3, addt0 = FALSE, inter = "Linear") { 
    
    checkNumericSameLength(time, conc, "time", "concentration", "AUCInfObs")
    
    checkOrderedVector(time, description = "time", functionName = "AUCInfObs")
    
    checkSingleLogical(addt0, description = "addt0", functionName = "AUCInfObs")
    
    checkSingleCharacter(inter, description = "inter", functionName = "AUCInfObs")
    
    # calc will hold the chosen method of calculation
    
    calc <- match.arg(calculation)
    
    aucio <- NA_real_
    
    if (is.null(lambdaZStats)) {
        
        # check that lamznpt is valid, return NA if it is not  
        
        checkSingleNumeric(lamznpt, description = "lamznpt")
        
        if (!(lamznpt >= minpoints && lamznpt <= length(time) && floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste("Invalid value of lamznpt:", lamznpt, "in AUCInfObs", collapse = " "))
            
            return(aucio)
        }
    }
    
    # Add T = 0 if it is missing and remove missing values
    
    timeconc <- try(stripTrailingZeros(conc = conc, time = time, addt0 = addt0), silent = TRUE)
    
    if( is(timeconc, "try-error")) {
        
        stop(paste("Error during data cleaning in AUCInfObs", as.character(timeconc), collapse = "\n"))
    }
    
    # timeconc : data.frame with columns consisting of the last lamznpt values of time and conc
    
    if (sum(timeconc$conc, na.rm = TRUE) == 0) {
        
        return(aucio)
    }
    
    if (!is.null(lambdaZStats)) {
        
        if (!is.null(lamznpt)) { warning("both lamznpt and lambdaZStats provided to AUCInfObs, ignoring lamznpt") }
        
        if (is(lambdaZStats, "list")) {
            
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {
        
        # check that there are at least 3 rows after Cmax, otherwise return NA  
        
        if (!testTrailPoints(conc = timeconc$conc, time = timeconc$time, minpoints = minpoints) ) {
        
            return(aucio)
        }
        
        # lamdbdaZStats : list with value of lambda z and related statistics
        
        lambdaZStats <- unlist(lambdaZStatistics(conc = timeconc$conc, time = timeconc$time, lamznpt = lamznpt))
    
    }
    
    # calculate cLast and tLast
    
    cLast <- ClastTlast(conc = timeconc$conc, time = timeconc$time)
    
    tLast <- cLast[["tlast"]]
    
    cLast <- cLast[["clast"]]
    
    if (any(is.na(cLast) | is.na(tLast))) { calc <- "na" }

    switch(calc, "standard" = {
            
            # aucLast : single numeric holding value of AUCLast(conc, time)  
         
            aucLast <- AUCLast(conc = timeconc$conc, time = timeconc$time, addt0 = addt0, inter = inter) 
            
            # aucExtraObs : single numeric with extrapolated AUC
            
            aucExtraObs <-  cLast / lambdaZStats["Lambdaz"]  
            
            aucio[1] <- aucLast + aucExtraObs
        },
        "moment" = {
            # aumcLast : single numeric holding value of AUCLast(conc * time, time) (area under moment curve)
            
            aumcLast <- AUCLast(conc = timeconc$conc * timeconc$time , time = timeconc$time, addt0 = addt0, inter = inter)
            
            # aumcExtraObs : single numeric with extrapolated AUMC
            
            aumcExtraObs <- cLast * tLast / lambdaZStats["Lambdaz"] + cLast / lambdaZStats["Lambdaz"]^2
            
            aucio[1] <- aumcLast + aumcExtraObs
        }, 
        "na" = { warning("missing values at tLast") },
        
        stop("only methods standard and moment recognized for calc")
    )
    
    return(aucio)
}


