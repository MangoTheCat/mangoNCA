
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' concentration at time at T = firsttime and T = secondtime
#'
#' This function calculates concentration at time
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{firsttime}  
#'      \item \code{secondtime} 
#'      \item \code{firstconc}
#'      \item \code{secondconc}
#'      \item \code{LAMZNPT} 
#'      \item \code{ERROR} 
#'  }
#'
#' @title concentration at time at T = firsttime and T = secondtime
#' @inheritParams AUCInfObs
#' @param firsttime single numeric value, first time at which concentration should be calculated
#' @param secondtime single numeric value, second time at which concentration should be calculated
#' @param usepoints If \code{NULL} (default) automatically select, else, 
#' logical vector of points to use for calculation of terminal phase. 
#' Used rows are flagged by usepoints as \code{TRUE}.
#' @param excpoints If \code{NULL} (default) automatically select, else, 
#' logical vector of points to exclude from automatic calculation of terminal phase. 
#' Excluded rows are flagged by excpoints as \code{TRUE}.
#' @param maxdiffrsq single numeric in range 0-1 The Adjusted R-squared method 
#' will select the number of points
#' for terminal phase calculation for the set of trailing points with the 
#' most points that is within maxdiffrsq of the maximum adjusted R-squared 
#' (default 1e-4)
#' @param minr2adj single numeric Minimum value permitted for adjusted 
#' R-squared for lambda-z to be calculated. 
#' If NA or NULL, lambda-z calculation is supressed. (default 0.8)
#' @param numhalflife single numeric Multiplier for terminal phase half life, 
#' when checking that terminal phase half life is not an excessively large 
#' portion of the AUC0_Inf. 
#' To always return lambda-z, set to 0.
#' To never return lambda-z set to Inf, NULL or NA. (default 1)
#' @return Data frame
#' @export
#' @author Mango Solutions
#' @keywords math

getConcentration <- function(conc = NULL, time = NULL, firsttime = NA, secondtime = NA, 
    lamznpt = NULL, usepoints = NULL, excpoints = NULL, inter = "Linear", 
    maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1)  {
    
    error <- ""
    
    # Initialise data check return object
    ROutput <- c(rep(NA_real_, times = 5), 0)
    
    names(ROutput) <- c(
        "firsttime", 
        "secondtime",
        "firstconc",
        "secondconc",
        "LAMZNPT", 
        "ERROR")
        
        
    # Check data for gross errors but allow firsttime and secondtime to be NA or NULL

    check01 <- try(checkOrderedVector(time, description = "time", 
            functionName = "getConcentration"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(time, conc, 
            "time", "concentration", "getConcentration"), silent = TRUE)
    
    # return if gross data errors present
    
    error <- paste(check01, check02, sep = "", collapse = "")
    
    if (!identical(x = error, y = "")) {
        
        # coerce to data frame and return
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ERROR"] <- error
  
        return(ROutput)
        
    }
    
    if (all(is.na(firsttime))) { firsttime <- NA_real_ }
    
    check03 <- try(checkSingleNumeric(firsttime, description = "firsttime", "getConcentration"), silent = TRUE)
    
    if (all(is.na(secondtime))) { secondtime <- NA_real_ }
    
    check04 <- try(checkSingleNumeric(secondtime, 
            description = "secondtime", "getConcentration"), silent = TRUE)
    
    if (is.null(lamznpt)) { 
        lamznpt <- NA_real_
    }
    
    check05 <- try(checkSingleNumeric(lamznpt, 
            description = "Number of Points", "getConcentration"), silent = TRUE)
    
    check06 <- try(if (any(is.na(conc))) { stop("Missing values in conc") }, silent = TRUE)
    
    check07 <- try(if (any(is.na(time))) { stop("Missing values in time") }, silent = TRUE)
    
    if(!is.null(usepoints)) { 
        
        check08 <- try(checkLogicalSameLength(usepoints, conc, 
                "usepoints", "concentration", "getConcentration"), silent = TRUE)
        
    } else {
        
        check08 <- NULL
    }
    
    if (is.null(excpoints)) { 
        
        excpoints <- rep(FALSE, times = length(time))
    }
 
    check09 <- try(checkLogicalSameLength(excpoints, conc, 
            "excpoints", "concentration", "getConcentration"), silent = TRUE)

    check10 <- try(checkSingleCharacter(inter, "inter", "getConcentration"), silent = TRUE)
    
    check11 <- try(checkSingleNumeric(maxdiffrsq, 
            description = "max difference in adj R-squared for greatest number of points", 
            functionName = "getConcentration"), silent = TRUE)
    
    check12 <- try(checkSingleNumeric(minr2adj, 
            description = "min adj R-squared which allows lambda-z to be returned", 
            functionName = "getConcentration"), silent = TRUE)
    
    check13 <- try(checkSingleNumeric(numhalflife, 
            description = "minimum proportion of halflife permitted for lambda-z data range", 
            functionName = "getConcentration"), silent = TRUE)
    
    # return if gross data errors present
    
    error <- paste(check03, check04, check05, check06, check07, 
        check08, check09, check10, check11, check12, check13, sep = "", collapse = "")
    
    if (!identical(x = error, y = "")) {
        
        # coerce to dataframe
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ERROR"] <- error
        
        return(ROutput)
    }
    
    ROutput["firsttime"] <- firsttime
    
    ROutput["secondtime"] <- secondtime
    
    # if addt0 is TRUE fix data errors (add T = 0 to data if it is missing and remove missing values)
    # otherwise throw exception if data errors are present
    # if no rows are returned by stripTrailingZeros, return empty data frame without error
    
    cleanData <- try(stripTrailingZeros(conc = conc, time = time, usepoints = usepoints, 
        excpoints = excpoints, addt0 = FALSE, checkT0 = TRUE), silent = TRUE)
    
    if (is(cleanData, "try-error") || identical(nrow(cleanData), as.integer(0)) || sum(cleanData$conc, na.rm = TRUE) == 0) {
        
        if (is(cleanData, "try-error")) {
        
            error <- paste(error, capture.output(show(cleanData)), collapse = "\n")
            
        } else {
            
            warning("sum zero, or zero rows returned from stripTrailingZeros in getConcentration")
            
            error <- 0
        }
        
        # return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ERROR"] <- error
        
        return(ROutput)
    }

    conc <- cleanData$conc
    
    time <- cleanData$time
    
    excpoints <- cleanData$excpoints
    
    if (!is.null(usepoints))  { usepoints <- cleanData$usepoints }
  
    # calculate number of points to use for lambdaz calculation
    # TODO see ncaComplete **
    doPoints <- list(ACTION = "fail")
    
    doPoints <- chooseNumPointsAction(conc = conc, time = time, lamznpt = lamznpt, 
        usepoints = usepoints, excpoints = excpoints)
    
    result <- as.numeric(rep(NA, 9))
    
    lzColNames <- c("Lambdaz", "intercept", "R2", "R2ADJ", "CORRXY", "LAMZHL", 
        "LAMZLL", "LAMZUL", "lamznpt")
    
    names(result) <- lzColNames
    
    lamznpt_result <- try(switch(doPoints[["ACTION"]], 
        
            # if lamznpt is zero or less, suppress terminal phase calculation
            none = list(lamznpt = NA_real_, result = result),
            
            # if lamznpt is one or more, suppress automatic selection
            fixed = fixedPoints(conc = conc, time = time, lamznpt = lamznpt, excpoints = excpoints, 
                minpoints = doPoints[["MINROWSFORLAMBDAZ"]]),
            
            # if lamznpt is NA, perform automatic point selection
            auto = selectPoints(conc = conc, time = time, 
                minpoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars", 
                maxdiffrsq = maxdiffrsq, minlambdaz = 0, 
                minr2adj = minr2adj, numhalflife = numhalflife,
                excpoints = excpoints),
            
            # if usepoints is logical, calculate lambdaz using specified subset of data
            used = usedPoints(conc = conc, time = time, usepoints = usepoints, excpoints = excpoints, 
                minpoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
            
            # else error
            stop(paste("Error in getConcentration: doPoints action was", 
                    doPoints[["ACTION"]], sep = "", collapse = ""))), 
        
        silent = TRUE)
    
    # return with error
    if (is(lamznpt_result, "try-error")) {
        
        error <- paste(error, lamznpt_result, sep = "", collapse = "")
        
        ROutput["ERROR"] <- error
        
        return(ROutput)
    }
    
    # calculate firstconc and secondconc
    
    firstconc <- try(predictConc(conc = conc, time = time, predtime = firsttime, 
        lambdaZStats = lamznpt_result$result), silent = TRUE)
    
    secondconc <- try(predictConc(conc = conc, time = time, predtime = secondtime, 
        lambdaZStats = lamznpt_result$result), silent = TRUE)
    
    if (is(firstconc, "try-error")) {
        
        error <- paste(error, firstconc, sep = "", collapse = "")
    }

    if (is(secondconc, "try-error")) {
        
        error <- paste(error, secondconc, sep = "", collapse = "")
    }
    
    if (identical(x = error, y = "")) {
        
        error <- 0
        
        ROutput["firstconc"] <- firstconc
        
        ROutput["secondconc"] <- secondconc
        
        ROutput["LAMZNPT"] <- lamznpt_result$result["lamznpt"]
    }
    
    ROutput <- as.data.frame(as.list(ROutput))
    
    ROutput["ERROR"] <- error
    
    return(ROutput)
}

