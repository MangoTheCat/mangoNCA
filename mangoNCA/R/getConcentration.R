# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Concentration at Time at T = firstTime and T = secondTime
#'
#' This function calculates Concentration at Time
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{firstTime}  
#'      \item \code{secondTime} 
#'      \item \code{firstConc}
#'      \item \code{secondConc}
#'      \item \code{numPoints} 
#'      \item \code{Error} 
#'  }
#'
#' @title Concentration at Time at T = firstTime and T = secondTime
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param firstTime single numeric value, first time at which Concentration should be calculated
#' @param secondTime single numeric value, second time at which Concentration should be calculated
#' @param numPoints single numeric value declaring number of points to use
#' @param usePoints If \code{NULL} (default) automatically select, else, logical vector of points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param excPoints If \code{NULL} (default) automatically select, else, logical vector of points to exclude from automatic calculation of terminal phase. Excluded rows are flagged by excPoints as \code{TRUE}.
#' @param Safe single logical value declaring whether error checking should be performed
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @return Data frame
#' @export
#' @author Mango Solutions
#' @keywords math

getConcentration <- function(Conc = NULL, Time = NULL, firstTime = NA, secondTime = NA, 
    numPoints = NULL, usePoints = NULL, excPoints = NULL, Safe = TRUE, inter = "Linear")  {

    # Initialise data check return object
    
    ROutput <- c(rep(as.numeric(NA), times = 5), 0)
    
    names(ROutput) <- c(
        "firstTime", 
        "secondTime",
        "firstConc",
        "secondConc",
        "numPoints", 
        "Error")
        
        
    # Check data for gross errors but allow firstTime and secondTime to be NA or NULL

    check01 <- try(checkOrderedVector(Time, description = "Time", functionName = "getConcentration"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(Time, Conc, "Time", "Concentration", "getConcentration"), silent = TRUE)
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check01, check02, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to data frame and return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- ROutput_Error
  
        return(ROutput)
        
    }
    
    if (all(is.na(firstTime))) { firstTime <- as.numeric(NA) }
    
    check03 <- try(checkSingleNumeric(firstTime, description = "firstTime", "getConcentration"), silent = TRUE)
    
    if (all(is.na(secondTime))) { secondTime <- as.numeric(NA) }
    
    check04 <- try(checkSingleNumeric(secondTime, description = "secondTime", "getConcentration"), silent = TRUE)
    
    if (is.null(numPoints)) { 
        numPoints <- as.numeric(NA)
    }
    
    check05 <- try(checkSingleNumeric(numPoints, description = "Number of Points", "getConcentration"), silent = TRUE)
    
    check06 <- try(if (any(is.na(Conc))) { stop("Missing values in Conc") }, silent = TRUE)
    
    check07 <- try(if (any(is.na(Time))) { stop("Missing values in Time") }, silent = TRUE)
    
    if(!is.null(usePoints)) { 
        
        check08 <- try(checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "getConcentration"), silent = TRUE)
        
    } else {
        
        check08 <- NULL
    }
    
    if(is.null(excPoints)) { 
        
        excPoints <- rep(FALSE, times = length(Time))
    }
 
    check09 <- try(checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "getConcentration"), silent = TRUE)

    check10 <- try(checkSingleCharacter(inter, "inter", "getConcentration"), silent = TRUE)
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check03, check04, check05, check06, check07, check08, check09, check10, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to dataframe
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["Error"] <- ROutput_Error
        
        return(ROutput)
        
    }
    
    ROutput["firstTime"] <- firstTime
    
    ROutput["secondTime"] <- secondTime
    
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
        
        ROutput["Error"] <- ROutput_Error
        
        return(ROutput)
    }
    
    # calculate firstConc and secondConc
    
    firstConc <- try(predictConc(Conc = Conc, Time = Time, predTime = firstTime, lambdaZStats = numPoints_result$result), silent = TRUE)
    
    secondConc <- try(predictConc(Conc = Conc, Time = Time, predTime = secondTime, lambdaZStats = numPoints_result$result), silent = TRUE)
    
    if (is(firstConc, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, firstConc, sep = "", collapse = "")
    }

    if (is(secondConc, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, secondConc, sep = "", collapse = "")
    }
    
    if (ROutput_Error == "") {
        
        ROutput_Error <- 0
        
        ROutput["firstConc"] <- firstConc
        
        ROutput["secondConc"] <- secondConc
        
        ROutput["numPoints"] <- numPoints_result$result["numPoints"]
    }
    
    ROutput <- as.data.frame(as.list(ROutput))
    
    ROutput["Error"] <- ROutput_Error
    
    return(ROutput)
    
}

