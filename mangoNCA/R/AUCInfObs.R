# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 24/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area under the Concentration Time Curve from T = 0 to T = Inf using extrapolation from Observed Clast
#' 
#' AUCInfObs calculates the area under a time/concentration (or first moment) curve from the first point up to  
#' "infinity" using the linear trapezium rule.  This is performed by calculating the area under the concentration-time
#' curve until the last measurable concentration (see \code{\link{AUCLast}}), and then adding an extrapolated 
#' AUC which is the area bounded by the exponential decay curve with rate constant -lambdaz from observed Clast.  
#' Analogous calculations for AUMC are used if appropriate. \cr
#' There are two methods for calculating AUC and AUMC.
#' Lambdaz stats can be calculated within the function. In this case, numPoints should be provided.
#' numPoints must be a single integer greater than minPonts (which should be at least 2) and less than or equal to the length of \code{Time} and \code{Conc}.
#' numPoints is NULL by default. Must be provided if lambdaZStats is not.
#' The lambdaz statistics may be provided directly to this function; it should be a length 9 numeric vector with named elements:
#' \enumerate{
#'      \item Lambdaz
#'      \item intercept
#'      \item r2
#'      \item adjr2
#'      \item rhoXY
#'      \item tPhaseHalfLife
#'      \item LambdazLower
#'      \item LambdazUpper
#'      \item numPoints
#' }
#' Use \code{unlist} to pass \code{lambdaZStatistics} output to this function.
#'
#' @title Calculate Area Under Curve or Moment Curve To "Infinity" (Observed)
#' @param Conc A numeric vector of concentration values
#' @param Time A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param numPoints Number of points to use for the lambdaz calculation, counted from the end of the concentration/time vectors.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if numPoints is not.
#' @param calculation Must be either the string "standard" or "moment".  If the former, calculates the standard area under
#' the curve.  For the latter, it will calculate the area under the moment curve
#' @param minPoints Minimum number of points to use for the Lambdaz calculation(s), 3 by default. Single positive integer.
#' @param addT0 Single logical value declaring whether T0 should be added if missing provided execution is Safe (default \code{TRUE}).
#' @param Safe Single logical value declaring whether to perform data checks and data cleaning (default is \code{TRUE}).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' @return A numeric vector with a single element holding the area under the concentration/time curve until "infinity".
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item all error checks for \code{AUCLast} apply
#'      \item If any relevant lambda z calculations are NA, NA will be returned
#'      \item An exception will be generated if numPoints is not a single integer numeric between 2 and  \code{length(Conc)} (inclusive)
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
#'      AUCInfObs(Conc = Theoph1$conc, Time = Theoph1$Time, numPoints = 4, calculation = "standard")


AUCInfObs <- function(Conc, Time, numPoints = NULL, lambdaZStats = NULL, calculation = c("standard", "moment"), minPoints = 3, addT0 = FALSE, Safe = TRUE, inter = "Linear")
{    
    checkSingleLogical(Safe, description = "Safe", functionName = "AUCInfObs")
    
    if (Safe) {
        
        checkNumericSameLength(Time, Conc, "Time", "Concentration", "AUCInfObs")
        
        checkOrderedVector(Time, description = "Time", functionName = "AUCInfObs")
        
        checkSingleLogical(addT0, description = "addT0", functionName = "AUCInfObs")
        
        checkSingleCharacter(inter, description = "inter", functionName = "AUCInfObs")
    }
    
    # calc will hold the chosen method of calculation
    
    calc <- match.arg(calculation)
    
    aucio <- as.numeric(NA)
    
    if (is.null(lambdaZStats)) {
        
        # check that numPoints is valid, return NA if it is not  
        
        checkSingleNumeric(numPoints, description = "numPoints")
        
        if( !(numPoints >= minPoints && numPoints <= length(Time) && floor(numPoints) ==  numPoints) ) {
            
            warning(paste("Invalid value of numPoints:", numPoints, "in AUCInfObs", collapse = " "))
            
            return(aucio)
        }
    }
    
    # Add T = 0 if it is missing and remove missing values
    
    timeConc <- try(stripTrailingZeros(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
    
    if( is(timeConc, "try-error")) {
        
        stop(paste("Error during data cleaning in AUCInfObs", as.character(timeConc), collapse = "\n"))
    }
    
    # timeConc : data.frame with columns consisting of the last numPoints values of Time and Conc
    
    if (sum(timeConc$Conc, na.rm = TRUE) == 0) {
        
        return(aucio)
    }
    
    if (!is.null(lambdaZStats)) {
        
        if (!is.null(numPoints)) { warning("both numPoints and lambdaZStats provided to AUCInfObs, ignoring numPoints") }
        
        if (is(lambdaZStats, "list")) {
            
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {
        
        # check that there are at least 3 rows after Cmax, otherwise return NA  
        
        if (!testTrailPoints(Conc = timeConc$Conc, Time = timeConc$Time, minPoints = minPoints) ) {
        
            return(aucio)
        }
        
        # lamdbdaZStats : list with value of lambda z and related statistics
        
        lambdaZStats <- unlist(lambdaZStatistics(Conc = timeConc$Conc, Time = timeConc$Time, numPoints = numPoints))
    
    }
    
    # calculate cLast and tLast
    
    cLast <- ClastTlast(Conc = timeConc$Conc, Time = timeConc$Time)
    
    tLast <- cLast[["tlast"]]
    
    cLast <- cLast[["clast"]]
    
    if (any(is.na(cLast) | is.na(tLast))) { calc <- "na" }

    switch(calc, "standard" = {
            
            # aucLast : single numeric holding value of AUCLast(Conc, Time)  
         
            aucLast <- AUCLast(Conc = timeConc$Conc, Time = timeConc$Time, addT0 = addT0, inter = inter) 
            
            # aucExtraObs : single numeric with extrapolated AUC
            
            aucExtraObs <-  cLast / lambdaZStats["Lambdaz"]  
            
            aucio[1] <- aucLast + aucExtraObs
        },
        "moment" = {
            # aumcLast : single numeric holding value of AUCLast(Conc * Time, Time) (area under moment curve)
            
            aumcLast <- AUCLast(Conc = timeConc$Conc * timeConc$Time , Time = timeConc$Time, addT0 = addT0, inter = inter)
            
            # aumcExtraObs : single numeric with extrapolated AUMC
            
            aumcExtraObs <- cLast * tLast / lambdaZStats["Lambdaz"] + cLast / lambdaZStats["Lambdaz"]^2
            
            aucio[1] <- aumcLast + aumcExtraObs
        }, 
        "na" = { warning("missing values at tLast") },
        
        stop("only methods standard and moment recognized for calc")
    )
    
    return(aucio)
}


