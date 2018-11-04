# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 21/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###########################################################


#' Predict concentration at time
#'
#' Calculate concentration at time.
#'
#' @param conc Vector of concentrations.
#' @param time Vector of times, must be ordered in ascending order and 
#' should not have duplicates.
#' @param predtime time at which conc should be calculated. if predtime 
#' is NA, NA is returned.
#' @param lamznpt Number of points to use for lambda-z calculation.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). 
#' Must be provided if lamznpt is not.
#' @param usepoints If \code{NULL} (default) automatically select, else, 
#' logical vector of points to use for calculation of terminal phase. 
#' Used rows are flagged by usepoints as \code{TRUE}.
#' @param excpoints If \code{NULL} (default) automatically select, else, 
#' logical vector of points to exclude from automatic calculation of terminal 
#' phase. Excluded rows are flagged by excpoints as \code{TRUE}.
#' @param addt0 single logical declaring whether to add T = 0 and remove missing 
#' values if TRUE or generate exception if FALSE (default is FALSE)
#' @title Predict concentration at time
#' @return Single numeric of predicted concentration at prediction time
#' @export
#' @seealso \code{\link{lambdaZStatistics}} for more about the lamznpt parameter
#' @author Mango Business Solutions
#' @keywords math


predictConc <- function(conc, time, predtime, lamznpt = NULL, lambdaZStats = NULL, 
    usepoints = NULL, excpoints = FALSE, minpoints = 3, addt0 = FALSE)
{
    
    checkOrderedVector(time, description = "time", functionName =  "predictConc")
    
    checkSingleNumeric(predtime, description = "predtime", functionName =  "predictConc")
    
    predconc <- as.numeric(NA)
    
    if (is.na(predtime)) {
    
        return(predconc)
    }    
    
    if (is.null(lambdaZStats)) {
        
        # check that lamznpt is valid, return NA if it is not  
        
        checkSingleNumeric(lamznpt, description = "lamznpt", functionName =  "predictConc")
        
        if( !(lamznpt >= minpoints && lamznpt <= length(time) && floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste("Invalid value of lamznpt:", lamznpt, "in predictConc", collapse = " "))
            
            return(predconc)
        }
    }
    
    if(!is.null(usepoints)) { checkLogicalSameLength(usepoints, conc, "usepoints", "concentration", "predictConc") }
    
    if (identical(excpoints, FALSE)) { excpoints <- rep(FALSE, times = length(time)) }
    
    checkLogicalSameLength(excpoints, time, "excpoints", "time", "predictConc")

    
    # Add T = 0 if it is missing and remove missing values if TRUE, otherwise throw error
    
    cleanData <- try(cleanconctime(conc = conc, time = time, addt0 = addt0), silent = TRUE)
    
    if( is(cleanData, "try-error") ) {
    
        stop(paste("Error in predictConc: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        
    }
    
    if (sum(cleanData$conc, na.rm = TRUE) == 0) {
        
        return(predconc)
    }
    
    conc <- cleanData$conc
    
    time <- cleanData$time
    
    ###############################################################################
    
    if (predtime < min(time)) {
    
        return(predconc)
    }
    
    ###############################################################################
    
    # if predtime is actually an element of the time vector, we can return data
    
    if( predtime %in% time )
    {
        # predtimeIndex : integer with index of predtime inside time
        
        predtimeIndex <- match(predtime, time )
        
        predconc <- conc[predtimeIndex]
        
        return(predconc)
    }

    
    ###############################################################################
    
    # if predtime > tlast, we need to extrapolate a new concentration element.   
    
    cLastTLast <- ClastTlast(conc = conc, time = time )
    
    if(predtime > cLastTLast$tlast) {
    
        if (!is.null(lambdaZStats)) {
            
            if (!is.null(lamznpt)) { warning("both lamznpt and lambdaZStats provided to predictConc, ignoring lamznpt") }
            
            if (is(lambdaZStats, "list")) {
                
                warning("lambdaZStats has been unlisted")
                
                lambdaZStats <- unlist(lambdaZStats)
            }
            
            checkLambdaZStats(lambdaZStats = lambdaZStats)
            
        } else {
            
            excpoints <- cleanData$excpoints
            
            if (!is.null(usepoints))  { usepoints <- cleanData$usepoints }
            
            
            ## Terminal phase calculation 
            
            doPoints <- list(ACTION = "fail")
            
            # T=0 not checked here
            
            doPoints <- chooseNumPointsAction(conc = conc, time = time, lamznpt = lamznpt, 
                usepoints = usepoints, excpoints = excpoints)
            
            result <- as.numeric(rep(NA, 9))
            
            lzColNames <- c("LAMZ", "intercept", "R2", "R2ADJ", "CORRXY", "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt")
            
            names(result) <- lzColNames
            
            lamznpt_result <- try(switch(doPoints[["ACTION"]], 
                
                    # if lamznpt is zero or less, suppress terminal phase calculation
                    
                    none = list(lamznpt = as.numeric(NA), result = result),
                    
                    # if lamznpt is one or more, suppress automatic selection
                    
                    fixed = fixedPoints(conc = conc, time = time, lamznpt = lamznpt, minpoints = doPoints[["MINROWSFORLAMBDAZ"]]),
                    
                    # if lamznpt is NA, perform automatic point selection
                    
                    auto = selectPoints(conc = conc, time = time, minpoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars", excpoints = excpoints),
                    
                    # if usepoints is logical, calculate lambdaz using specified subset of data
                    
                    used = usedPoints(conc = conc, time = time, usepoints = usepoints, excpoints = excpoints, minpoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
                    
                    # else error
                    
                    stop(paste("Error in ncaComplete: doPoints action was", doPoints[["ACTION"]], sep = "", collapse = ""))), 
                    
                silent = TRUE)
            
            # return with error
            
            if (is(lamznpt_result, "try-error")) {
                
                stop(paste("error in predictConc, action in doPoints was", doPoints[["ACTION"]], "message ws", lamznpt_result, sep = "", collapse = ""))
            }
            
            if (is.na(lamznpt_result$lamznpt)) { return(predconc) }
            
            lambdaZStats <- lamznpt_result$result
        }
        
        
        
        # calculate the extrapolated concentration based on concentration at infinity (predicted)
        
        predconc[1] <- lambdaZStats["intercept"] * exp(-lambdaZStats["LAMZ"] * predtime)
        
        return(predconc)
    }
    
    ###############################################################################
    
    # otherwise predtime lies between 2 data points
    
    # find the last element which predtime exceeds
    # t1Index the index of the largest time less than predtime
    # t2Index is next next time after t1Index
    
    t1Index <- tail( which(time < predtime), n = 1 )
    
    t2Index <- t1Index + 1
    
    # t1, t2 = left time, right time (of interval containing predtime)
    # c1, c2 = left concentration, right concentration (of interval containing predtime)
    
    t1 <- time[t1Index]
    
    t2 <- time[t2Index]
    
    c1 <- conc[t1Index]
    
    c2 <- conc[t2Index] 
    
    # cInter is the interpolated concentration
    
    predconc <- c1 + abs( (predtime - t1) / (t2 - t1) ) * (c2 - c1)
    
    return(predconc)
    
}
