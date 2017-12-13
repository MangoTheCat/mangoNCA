# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Concentration Time Curve from T = startTime to T = endTime
#'
#' This is a wrapper function designed to be the interface for performing a partial AUC calculation. 
#' \code{getPartialAUC} returns the area under a time-concentration curve from \code{firstTime} up until \code{endTime}. 
#' The calculation is performed by subtracting AUC_0_startTime from AUC_0_endTime using \code{AUCPartial}.
#' \code{endTime} need not be one of the elements of \code{Time}.
#' If \code{endTime} does not coincide with an existing time element and is less than tlast, 
#' the following interpolation formula will be used to calculated a new concentration: 
#'     \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  
#' If endTime is greater than tlast, then another concentration is extrapolated via the formula: 
#'     \deqn{c_{extr} = c_0 + \exp(- \lambda_z * endTime)  }.
#' \code{numPoints} is calculated by \code{\link{selectPoints}}.
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{startTime}  
#'      \item \code{endTime} 
#'      \item \code{AUCPartial}
#'      \item \code{numPoints} 
#'      \item \code{Error} 
#'  }
#'
#' @title Partial Area Under Curve 
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param startTime Single numeric value, first time from which area should be calculated
#' @param endTime Single numeric value, last time to which area should be calculated
#' @param numPoints single numeric value declaring number of points to use
#' @param usePoints If \code{NULL} (default) automatically select, else, logical vector of points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param excPoints If \code{NULL} (default) automatically select, else, logical vector of points to exclude from automatic calculation of terminal phase. Excluded rows are flagged by excPoints as \code{TRUE}.
#' @param Safe Single logical value, if FALSE don't perform additional error checks
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @return Data frame 
#' @export
#' @author Mango Solutions
#' @keywords math

getPartialAUC <- function(Conc = NULL, Time = NULL, startTime = NULL, endTime = NULL, 
    numPoints = NULL, usePoints = NULL, excPoints = NULL, Safe = TRUE, inter = "Linear")  {

    # Initialise data check return object
    
    ROutput <- c(rep(as.numeric(NA), times = 4), 0)
    
    names(ROutput) <- c(
        "startTime", 
        "endTime",
        "AUCPartial",
        "numPoints", 
        "Error")
        
        
    # Check data for gross errors

    check01 <- try(checkOrderedVector(Time, description = "Time", functionName = "getPartialAUC"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(Time, Conc, "Time", "Concentration", "getPartialAUC"), silent = TRUE)
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check01, check02, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to data frame and return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- ROutput_Error
  
        return(ROutput)
        
    }
    
    check03 <- try(checkSingleNumeric(startTime, description = "startTime", "getPartialAUC"), silent = TRUE)
    
    check04 <- try(checkSingleNumeric(endTime, description = "endTime", "getPartialAUC"), silent = TRUE)
    
    if (is.null(numPoints)) { 
        numPoints <- as.numeric(NA)
    }
    
    check05 <- try(checkSingleNumeric(numPoints, description = "Number of Points", "getPartialAUC"), silent = TRUE)
    
    check06 <- try(if (any(is.na(Conc))) { stop("Missing values in Conc") }, silent = TRUE)
    
    check07 <- try(if (any(is.na(Time))) { stop("Missing values in Time") }, silent = TRUE)
    
    if(!is.null(usePoints)) { 
        
        check08 <- try(checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "getPartialAUC"), silent = TRUE)
        
    } else {
        
        check08 <- NULL
    }
    
    if(is.null(excPoints)) { 
        
        excPoints <- rep(FALSE, times = length(Time))
    }
 
    check09 <- try(checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "getPartialAUC"), silent = TRUE)

    check10 <- try(checkSingleCharacter(inter, "inter", "getPartialAUC"), silent = TRUE)
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check03, check04, check05, check06, check07, check08, check09, check10, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to dataframe
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["Error"] <- ROutput_Error
        
        return(ROutput)
        
    }
    
    ROutput["startTime"] <- startTime
    
    ROutput["endTime"] <- endTime   
    
    if (startTime >= endTime) { warning("startTime is greater than or equal to endTime") }
    
    # if addT0 is TRUE fix data errors (add T = 0 to data if it is missing and remove missing values)
    # otherwise throw exception if data errors are present
    # if no rows are returned by stripTrailingZeros, return empty data frame without error
    
    cleanData <- try(stripTrailingZeros(Conc = Conc, Time = Time, usePoints = usePoints, 
        excPoints = excPoints, addT0 = FALSE, checkT0 = TRUE), silent = TRUE)
    
    if (is(cleanData, "try-error") || identical(nrow(cleanData), as.integer(0)) || sum(cleanData$Conc, na.rm = TRUE) == 0) {
        
        if (is(cleanData, "try-error")) {
        
            ROutput_Error <- paste(ROutput_Error, capture.output(show(cleanData)), collapse = "\n")
            
        } else {
            
            warning("sum zero, or zero rows returned from stripTrailingZeros in getConcentration")
            
            ROutput_Error <- 0
        }
        
        # return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    }

    Conc <- cleanData$Conc
    
    Time <- cleanData$Time
    
    excPoints <- cleanData$excPoints
    
    if (!is.null(usePoints))  { usePoints <- cleanData$usePoints }
    
    # calculate number of points to use for lambdaz calculation
    
    doPoints <- list(ACTION = "fail")
    
    doPoints <- chooseNumPointsAction(Conc = Conc, Time = Time, numPoints = numPoints, 
        usePoints = usePoints, excPoints = excPoints)
    
    result <- as.numeric(rep(NA, 9))
    
    lzColNames <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
    
    names(result) <- lzColNames
    
    numPoints_result <- try(switch(doPoints[["ACTION"]], 
        
            # if numPoints is zero or less, suppress terminal phase calculation
            
            none = list(numPoints = as.numeric(NA), result = result),
            
            # if numPoints is one or more, suppress automatic selection
            
            fixed = fixedPoints(Conc = Conc, Time = Time, numPoints = numPoints, excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]),
            
            # if numPoints is NA, perform automatic point selection
            
            auto = selectPoints(Conc = Conc, Time = Time, minPoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars", excPoints = excPoints),
            
            # if usePoints is logical, calculate lambdaz using specified subset of data
            
            used = usedPoints(Conc = Conc, Time = Time, usePoints = usePoints, excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
            
            # else error
            
            stop(paste("Error in getConcentration: doPoints action was", doPoints[["ACTION"]], sep = "", collapse = ""))), 
        
        silent = TRUE)
    
    # return with error
    
    if (is(numPoints_result, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, numPoints_result, sep = "", collapse = "")
        
    } else {
    
        # calculate AUCPartial from 0 to startTime and 0 to endTime
        
        AUC_0_startTime <- try(AUCPartial(Conc = Conc, Time = Time, endTime = startTime, lambdaZStats = numPoints_result$result, inter = inter), silent = TRUE)
        
        AUC_0_endTime <- try(AUCPartial(Conc = Conc, Time = Time, endTime = endTime, lambdaZStats = numPoints_result$result, inter = inter), silent = TRUE)
        
        if (is(AUC_0_startTime, "try-error")) {
            
            ROutput_Error <- paste(ROutput_Error, AUC_0_startTime, sep = "", collapse = "")
        }

        if (is(AUC_0_endTime, "try-error")) {
            
            ROutput_Error <- paste(ROutput_Error, AUC_0_endTime, sep = "", collapse = "")
        }
        
        if (ROutput_Error == "") {
           
            ROutput["AUCPartial"] <- AUC_0_endTime - AUC_0_startTime
            
            ROutput["numPoints"] <- numPoints_result$result["numPoints"]
            
            ROutput_Error <- 0
        }
    }
    
    ROutput <- as.data.frame(as.list(ROutput))
    
    ROutput["Error"] <- ROutput_Error
    
    return(ROutput)
}

