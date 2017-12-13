# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 24/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Concentration Time Curve from Time = 0 to Time = Inf using extrapolation from Predicted Clast
#'
#' AUCInfPred calculates the area under a time/concentration (or first moment) curve from the first point up to  
#' "infinity" using the linear trapezium rule.  This is performed by calculating the area under the concentration-time 
#' curve until the last measurable concentration (see \code{\link{AUCLast}}), and then adding an extrapolated 
#' AUC which is the area bounded by the exponential decay curve with rate constant -lambdaz from observed Clast.  
#' Analogous calculations for AUMC are used if appropriate. \cr
#' There are two methods for calculating AUC and AUMC.
#' Lambdaz stats can be calculated within the function. In this case, numPoints should be provided.
#' numPoints must be a single integer greater than minPonts (which should be at least 2) and less than or equal to the length of \code{Time} and \code{Conc}.
#' numPoints is NULL by default. Must be provided if lambdaZStats is not.
#' The lambdaz statistics may be provided directly to this function; it should have length 1 numeric elements:
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
#' @title Calculate Area Under Curve or Moment Curve To "Infinity" (Predicted)
#' @param Conc A numeric vector of concentration values
#' @param Time A numeric vector of time values, parallel to \code{Conc} and of the same length.  These should be sorted
#' in ascending order.
#' @param numPoints Number of points to use for the lambda z calculation, counted from the end of the concentration/time vectors.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if numPoints is not.
#' @param calculation Must be either the string "standard" or "moment".  If the former, calculates the standard area under
#' the curve.  For the latter, it will calculate the area under the moment curve
#' @param minPoints Minimum number of points to use for the Lambdaz calculation(s), 3 by default. Single positive integer.
#' @param addT0 Single logical value declaring whether T0 should be added if missing provided execution is Safe (default TRUE).
#' @param Safe Single logical value declaring whether to perform data checks and data cleaning (default is TRUE).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' @return A numeric vector with a single element holding the area under the concentration/time curve until "infinity".
#' @note The following error checks / processing will be performed:
#'  \enumerate{
#'      \item all error checks for \code{AUCLast} apply
#'      \item If any relevant lambda z calculations are NA, NA will be returned
#'      \item An exception will be generated if numPoints is not a single integer numeric between 2 and  \code{length(Conc)} (inclusive)
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
#'      AUCInfPred(Theoph1$conc, Theoph1$Time, numPoints = 4)

AUCInfPred <- function(Conc, Time, numPoints = NULL, lambdaZStats = NULL, calculation = c("standard", "moment"), minPoints = 3, addT0 = FALSE, Safe = TRUE, inter = "Linear")
{
    checkSingleLogical(Safe, description = "Safe", functionName = "AUCInfPred")
    
    if (Safe) {
    
        checkNumericSameLength(Time, Conc, "Time", "Concentration", "AUCInfPred")  
        
        checkOrderedVector(Time, description = "Time", functionName = "AUCInfPred")
        
        checkSingleLogical(addT0, description = "addT0", functionName = "AUCInfPred")
        
        checkSingleCharacter(inter, description = "inter", functionName = "AUCInfPred")
    }

    # calc will hold the chosen method of calculation
    
    calc <- match.arg(calculation)
    
    aucip <- as.numeric(NA)
 
    if (is.null(lambdaZStats)) {
        
        # check that numPoints is valid, return NA if it is not  
        
        checkSingleNumeric(numPoints, description = "numPoints")
        
        if( !(numPoints >= minPoints && numPoints <= length(Time) && floor(numPoints) ==  numPoints) ) {
            
            warning(paste("Invalid value of numPoints:", numPoints, "in AUCInfPred", collapse = " "))
            
            return(aucip)
        }
    }
    
    # Add T = 0 if it is missing and remove missing values
    
    timeConc <- try(stripTrailingZeros(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("Error during data cleaning in AUCInfPred", as.character(timeConc), collapse = "\n"))
    }
    
    # timeConc : data.frame with columns consisting of the last numPoints values of Time and Conc
    
    if (sum(timeConc$Conc, na.rm = TRUE) == 0) {
        
        return(aucip)
    }

    if (!is.null(lambdaZStats)) {
        
        if (!is.null(numPoints)) { warning("both numPoints and lambdaZStats provided to AUCInfPred, ignoring numPoints") }
        
        if (is(lambdaZStats, "list")) {
            
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {

        # check that there are at least 3 rows after Cmax, otherwise return NA
        
        if( !testTrailPoints(Conc = timeConc$Conc, Time = timeConc$Time, minPoints = minPoints) ) {
            
            return(aucip)
        }
        
        # lamdbdaZStats : list with value of lambda z and related statistics
        
        lambdaZStats <- unlist(lambdaZStatistics( Conc = timeConc$Conc, Time = timeConc$Time, numPoints = numPoints ))
        
    }
    
    # cLastExtrap : extrapolated/predicted value of Clast  
    
    tLast <- ClastTlast(Conc = timeConc$Conc, Time = timeConc$Time)[["tlast"]]
    
    cLastExtrap <- exp(-lambdaZStats["Lambdaz"] * tLast) * lambdaZStats["intercept"]
        
    if (any(is.na(cLastExtrap) | is.na(tLast))) { calc <- "na" }
    
    switch(calc, "standard" = {
            
        # aucLast : single numeric holding value of AUCLast(Conc, Time)    
        
        aucLast <- AUCLast(Conc = timeConc$Conc, Time = timeConc$Time, addT0 = addT0, inter = inter)  
        
        # aucExtraPred : single numeric with extrapolated AUC
        
        aucExtraPred <-  cLastExtrap / lambdaZStats["Lambdaz"]
        
        aucip[1] <- aucLast + aucExtraPred
    },
        "moment" = {
        # aumcLast : single numeric holding value of AUCLast(Conc * Time, Time) (area under moment curve)
        
        aumcLast <- AUCLast(Conc = timeConc$Conc * timeConc$Time , Time = timeConc$Time, addT0 = addT0, inter = inter)
    
        # aumcExtraPred : single numeric with extrapolated AUMC
        
        aumcExtraPred <- cLastExtrap * tLast / lambdaZStats["Lambdaz"] + cLastExtrap / lambdaZStats["Lambdaz"]^2
        
        aucip[1] <- aumcLast + aumcExtraPred
        }, 
        "na" = { warning("missing values at tLast") },
        
        stop("only methods standard and moment recognized for calc")
    )
    
    return(aucip)
}
