# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 21/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Predict Concentration at Time
#'
#' Calculate Concentration at Time.
#'
#' @param Conc Vector of concentrations.
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates.
#' @param predTime time at which Conc should be calculated. if predTime is NA, NA is returned.
#' @param numPoints Number of points to use for lambda-z calculation.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if numPoints is not.
#' @param usePoints If \code{NULL} (default) automatically select, else, logical vector of points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param excPoints If \code{NULL} (default) automatically select, else, logical vector of points to exclude from automatic calculation of terminal phase. Excluded rows are flagged by excPoints as \code{TRUE}.
#' @param addT0 single logical declaring whether to add T = 0 and remove missing values if TRUE or generate exception if FALSE (default is FALSE)
#' @title Predict Concentration at Time
#' @return Single numeric of predicted concentration at prediction time
#' @export
#' @seealso \code{\link{lambdaZStatistics}} for more about the numPoints parameter
#' @author Mango Business Solutions
#' @keywords math


predictConc <- function(Conc, Time, predTime, numPoints = NULL, lambdaZStats = NULL, 
    usePoints = NULL, excPoints = FALSE, minPoints = 3, addT0 = FALSE)
{
    
    checkOrderedVector(Time, description = "Time", functionName =  "predictConc")
    
    checkSingleNumeric(predTime, description = "predTime", functionName =  "predictConc")
    
    predConc <- as.numeric(NA)
    
    if (is.na(predTime)) {
    
        return(predConc)
    }    
    
    if (is.null(lambdaZStats)) {
        
        # check that numPoints is valid, return NA if it is not  
        
        checkSingleNumeric(numPoints, description = "numPoints", functionName =  "predictConc")
        
        if( !(numPoints >= minPoints && numPoints <= length(Time) && floor(numPoints) ==  numPoints) ) {
            
            warning(paste("Invalid value of numPoints:", numPoints, "in predictConc", collapse = " "))
            
            return(predConc)
        }
    }
    
    if(!is.null(usePoints)) { checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "predictConc") }
    
    if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
    
    checkLogicalSameLength(excPoints, Time, "excPoints", "Time", "predictConc")

    
    # Add T = 0 if it is missing and remove missing values if TRUE, otherwise throw error
    
    cleanData <- try(cleanConcTime(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
    
    if( is(cleanData, "try-error") ) {
    
        stop(paste("Error in predictConc: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        
    }
    
    if (sum(cleanData$Conc, na.rm = TRUE) == 0) {
        
        return(predConc)
    }
    
    Conc <- cleanData$Conc
    
    Time <- cleanData$Time
    
    ###############################################################################
    
    if (predTime < min(Time)) {
    
        return(predConc)
    }
    
    ###############################################################################
    
    # if predTime is actually an element of the Time vector, we can return data
    
    if( predTime %in% Time )
    {
        # predTimeIndex : integer with index of predTime inside Time
        
        predTimeIndex <- match(predTime, Time )
        
        predConc <- Conc[predTimeIndex]
        
        return(predConc)
    }

    
    ###############################################################################
    
    # if predTime > tlast, we need to extrapolate a new concentration element.   
    
    cLastTLast <- ClastTlast(Conc = Conc, Time = Time )
    
    if(predTime > cLastTLast$tlast) {
    
        if (!is.null(lambdaZStats)) {
            
            if (!is.null(numPoints)) { warning("both numPoints and lambdaZStats provided to predictConc, ignoring numPoints") }
            
            if (is(lambdaZStats, "list")) {
                
                warning("lambdaZStats has been unlisted")
                
                lambdaZStats <- unlist(lambdaZStats)
            }
            
            checkLambdaZStats(lambdaZStats = lambdaZStats)
            
        } else {
            
            excPoints <- cleanData$excPoints
            
            if (!is.null(usePoints))  { usePoints <- cleanData$usePoints }
            
            
            ## Terminal phase calculation 
            
            doPoints <- list(ACTION = "fail")
            
            # T=0 not checked here
            
            doPoints <- chooseNumPointsAction(Conc = Conc, Time = Time, numPoints = numPoints, 
                usePoints = usePoints, excPoints = excPoints)
            
            result <- as.numeric(rep(NA, 9))
            
            lzColNames <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
            
            names(result) <- lzColNames
            
            numPoints_result <- try(switch(doPoints[["ACTION"]], 
                
                    # if numPoints is zero or less, suppress terminal phase calculation
                    
                    none = list(numPoints = as.numeric(NA), result = result),
                    
                    # if numPoints is one or more, suppress automatic selection
                    
                    fixed = fixedPoints(Conc = Conc, Time = Time, numPoints = numPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]),
                    
                    # if numPoints is NA, perform automatic point selection
                    
                    auto = selectPoints(Conc = Conc, Time = Time, minPoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars", excPoints = excPoints),
                    
                    # if usePoints is logical, calculate lambdaz using specified subset of data
                    
                    used = usedPoints(Conc = Conc, Time = Time, usePoints = usePoints, excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
                    
                    # else error
                    
                    stop(paste("Error in ncaComplete: doPoints action was", doPoints[["ACTION"]], sep = "", collapse = ""))), 
                    
                silent = TRUE)
            
            # return with error
            
            if (is(numPoints_result, "try-error")) {
                
                stop(paste("error in predictConc, action in doPoints was", doPoints[["ACTION"]], "message ws", numPoints_result, sep = "", collapse = ""))
            }
            
            if (is.na(numPoints_result$numPoints)) { return(predConc) }
            
            lambdaZStats <- numPoints_result$result
        }
        
        
        
        # calculate the extrapolated concentration based on Concentration at infinity (predicted)
        
        predConc[1] <- lambdaZStats["intercept"] * exp(-lambdaZStats["Lambdaz"] * predTime)
        
        return(predConc)
    }
    
    ###############################################################################
    
    # otherwise predTime lies between 2 data points
    
    # find the last element which predTime exceeds
    # t1Index the index of the largest time less than predTime
    # t2Index is next next time after t1Index
    
    t1Index <- tail( which(Time < predTime), n = 1 )
    
    t2Index <- t1Index + 1
    
    # t1, t2 = left time, right time (of interval containing predTime)
    # c1, c2 = left concentration, right concentration (of interval containing predTime)
    
    t1 <- Time[t1Index]
    
    t2 <- Time[t2Index]
    
    c1 <- Conc[t1Index]
    
    c2 <- Conc[t2Index] 
    
    # cInter is the interpolated concentration
    
    predConc <- c1 + abs( (predTime - t1) / (t2 - t1) ) * (c2 - c1)
    
    return(predConc)
    
}
