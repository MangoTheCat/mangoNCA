
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Area Under the concentration time Curve from T = starttime to T = endtime
#'
#' This is a wrapper function designed to be the interface for performing a partial AUC calculation. 
#' \code{getPartialAUC} returns the area under a time-concentration curve from \code{firsttime} up until \code{endtime}. 
#' The calculation is performed by subtracting AUC_0_starttime from AUC_0_endtime using \code{AUCPartial}.
#' \code{endtime} need not be one of the elements of \code{time}.
#' If \code{endtime} does not coincide with an existing time element and is less than tlast, 
#' the following interpolation formula will be used to calculated a new concentration: 
#'     \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  
#' If endtime is greater than tlast, then another concentration is extrapolated via the formula: 
#'     \deqn{c_{extr} = c_0 + \exp(- \lambda_z * endtime)  }.
#' \code{lamznpt} is calculated by \code{\link{selectPoints}}.
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{starttime}  
#'      \item \code{endtime} 
#'      \item \code{AUCPartial}
#'      \item \code{lamznpt} 
#'      \item \code{Error} 
#'  }
#'
#' @title Partial Area Under Curve 
#' @inheritParams getNCAnalysis
#' @param starttime Single numeric value, first time from which area should be calculated
#' @param endtime Single numeric value, last time to which area should be calculated
#' @return Data frame 
#' @export
#' @author Mango Solutions

getPartialAUC <- function(conc = NULL, time = NULL, starttime = NULL, endtime = NULL, 
    lamznpt = NULL, usepoints = NULL, excpoints = NULL, inter = "Linear", 
    maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1)  {
    
    error <- ""
    
    # Initialise data check return object
    ROutput <- c(rep(NA_real_, times = 4), 0)
    
    names(ROutput) <- c(
        "starttime", 
        "endtime",
        "AUCINT",
        "LAMZNPT", 
        "ERROR")
        
    # Check data for gross errors

    check01 <- try(checkOrderedVector(time, description = "time", functionName = "getPartialAUC"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(time, conc, "time", "concentration", "getPartialAUC"), silent = TRUE)
    
    # return if gross data errors present
    
    error <- paste(check01, check02, sep = "", collapse = "")
    
    if (!identical(x = error, y = "")) {
        
        # coerce to data frame and return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ERROR"] <- error
        
        return(ROutput)
    }
    
    check03 <- try(checkSingleNumeric(starttime, description = "starttime", "getPartialAUC"), silent = TRUE)
    
    check04 <- try(checkSingleNumeric(endtime, description = "endtime", "getPartialAUC"), silent = TRUE)
    
    if (is.null(lamznpt)) { 
        lamznpt <- NA_real_
    }
    
    check05 <- try(checkSingleNumeric(lamznpt, description = "Number of Points", "getPartialAUC"), silent = TRUE)
    
    check06 <- try(if (any(is.na(conc))) { stop("Missing values in conc") }, silent = TRUE)
    
    check07 <- try(if (any(is.na(time))) { stop("Missing values in time") }, silent = TRUE)
    
    if(!is.null(usepoints)) { 
        
        check08 <- try(checkLogicalSameLength(usepoints, conc, 
                "usepoints", "concentration", "getPartialAUC"), silent = TRUE)
        
    } else {
        
        check08 <- NULL
    }
    
    if(is.null(excpoints)) { 
        
        excpoints <- rep(FALSE, times = length(time))
    }
 
    check09 <- try(checkLogicalSameLength(excpoints, conc, 
            "excpoints", "concentration", "getPartialAUC"), silent = TRUE)

    check10 <- try(checkSingleCharacter(inter, "inter", "getPartialAUC"), silent = TRUE)
    
    check11 <- try(checkSingleNumeric(maxdiffrsq, 
            description = "max difference in adj R-squared for greatest number of points", 
            functionName = "getPartialAUC"), silent = TRUE)
    
    check12 <- try(checkSingleNumeric(minr2adj, 
            description = "min adj R-squared which allows lambda-z to be returned", 
            functionName = "getPartialAUC"), silent = TRUE)
    
    check13 <- try(checkSingleNumeric(numhalflife, 
            description = "minimum proportion of halflife permitted for lambda-z data range", 
            functionName = "getPartialAUC"), silent = TRUE)
    
    # return if gross data errors present
    
    error <- paste(check03, check04, check05, check06, check07, 
        check08, check09, check10, check11, check12, check13, sep = "", collapse = "")
    
    if (!identical(x = error, y = "")) {
        
        # coerce to dataframe
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ERROR"] <- error
        
        return(ROutput)
    }
    
    ROutput["starttime"] <- starttime
    
    ROutput["endtime"] <- endtime   
    
    if (starttime >= endtime) { 
        warning("starttime is greater than or equal to endtime") }
    
    # if addt0 is TRUE fix data errors 
    # (add T = 0 to data if it is missing and remove missing values)
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
        "LAMZLL", "LAMZUL", "LAMZNPT")
    
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
        
    } else {
        
        # calculate AUCPartial from 0 to starttime and 0 to endtime
        AUC_0_starttime <- try(AUCPartial(conc = conc, time = time, endtime = starttime, 
            lambdaZStats = lamznpt_result$result, inter = inter, 
            maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, numhalflife = numhalflife), silent = TRUE)
        
        AUC_0_endtime <- try(AUCPartial(conc = conc, time = time, endtime = endtime, 
            lambdaZStats = lamznpt_result$result, inter = inter, 
            maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, numhalflife = numhalflife), silent = TRUE)
        
        if (is(AUC_0_starttime, "try-error")) {
            
            error <- paste(error, AUC_0_starttime, sep = "", collapse = "")
        }

        if (is(AUC_0_endtime, "try-error")) {
            
            error <- paste(error, AUC_0_endtime, sep = "", collapse = "")
        }
        
        if (identical(x = error, y = "")) {
            
            error <- 0
            
            ROutput["AUCINT"] <- AUC_0_endtime - AUC_0_starttime
            
            ROutput["LAMZNPT"] <- lamznpt_result$result["LAMZNPT"]
        }
    }
    
    ROutput <- as.data.frame(as.list(ROutput))
    
    ROutput["ERROR"] <- error
    
    return(ROutput)
}

