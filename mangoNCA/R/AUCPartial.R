
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the concentration time Curve from T = 0 to T = endtime
#'
#' Calculates the area under a time-concentration curve from the first time up until the \code{endtime}.  
#' \code{endtime} need not be one of the elements of \code{time}, but it should not lie before the minimum time.
#' If the endtime does not coincide with an existing time element and is less than tlast, the following 
#' interpolation formula will be used to calculated a new concentration: 
#' \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  
#' If endtime is greater than tlast the extrapolated area is calculated using \code{getTerminalAUC} 
#' which calculates the area under the terminal phase exponential from TLast to endtime.
#' \code{lamznpt} is calculated by \code{\link{getPartialAUC}} .  
#' This calculation is the integral of the Terminal phase exponential decay function: 
#' \deqn{\int intercept \ast exp\left ( -\lambda _{z} \ast T\right ) dT = 
#'     -intercept / \left ( \lambda _{z} \ast exp\left ( \lambda _{z} \ast T\right )\right)}
#' The partial area under the terminal concentration time curve can be calculated 
#' by subtracting modeled AUC0Tlast from modeled AUC0endtime.
#' \code{lamznpt} may be NA provided endtime < Tlast.
#' 
#' @param conc Vector of concentrations
#' @param time Vector of times, must be ordered in ascending order and 
#' should not have duplicates
#' @param endtime : last time at which area should be calculated
#' @param lamznpt : Number of points to use for lambda-z calculation.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). 
#' Must be provided if lamznpt is not.
#' @param usepoints If NULL (default) automatically select, else, logical 
#' vector of points to use (\code{TRUE}) for calculation of terminal phase.
#' @param excpoints Logical vector (\code{FALSE} by default) excludeing 
#' \code{TRUE} rows from automatic calculation of terminal phase.
#' @param minpoints single numeric, the fewest points for which a linear model 
#' fit should be attempted (default 3)
#' @param addt0 single logical, should T=0, C=0 be added to the input data if 
#' T0 is missing? (default \code{FALSE})
#' @param inter Single character stating whether the interpolation method used 
#' is \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @param useObs single logical, use observed rather than predicted CLast, 
#' (default FALSE)
#' @param maxdiffrsq single numeric in range 0-1 The Adjusted R-squared method 
#' will select the number of points
#' for terminal phase calculation for the set of trailing points with the 
#' most points that is within maxdiffrsq of the maximum adjusted R-squared 
#' (default 1e-4)
#' @param minr2adj single numeric Minimum value permitted for adjusted 
#' R-squared for lambda-z to be calculated. 
#' If NA or NULL, lambda-z calculation is supressed. (default 0.8)
#' @param numhalflife single numeric Multiplier for terminal phase half life, 
#' when checking that terminal phase half life is not an excessively 
#' large portion of the AUC0_Inf. 
#' To always return lambda-z, set to 0.
#' To never return lambda-z set to Inf, NULL or NA. (default 1)
#' @title Partial Area Under concentration time Curve
#' @return Single numeric with partial area under curve
#' @export
#' @seealso \code{\link{lambdaZStatistics}} for more about the lamznpt parameter
#' @author Mango Business Solutions
#' @keywords math

AUCPartial <- function(conc, time, endtime, lamznpt = NULL, 
    lambdaZStats = NULL, usepoints = NULL, excpoints = FALSE, 
    minpoints = 3, addt0 = FALSE, inter = "Linear", useObs = FALSE, 
    maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1) {
    
    checkOrderedVector(
        x = time, 
        description = "time", 
        functionName =  "AUCPartial")    
    
    checkSingleNumeric(
        x = endtime, 
        description = "endtime", 
        functionName =  "AUCPartial")
    
    aucp <- NA_real_
    
    if (is.na(endtime)) {
        return(aucp)
    }
    
    if (is.null(lambdaZStats)) {
        checkSingleNumeric(
            x = lamznpt, 
            description = "lamznpt", 
            functionName = "AUCPartial")
        
        if (!(lamznpt >= minpoints && lamznpt <= length(time) && 
            floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste(
                "Invalid value of lamznpt:", 
                lamznpt, 
                "in AUCInfObs", 
                collapse = " "))
            
            return(aucp)
        }
    }
    
    if (!is.null(usepoints)) { 
        checkLogicalSameLength(
            x = usepoints, 
            y = conc, 
            xDescription = "usepoints", 
            yDescription = "concentration", 
            functionName = "AUCPartial") }
    
    if (identical(excpoints, FALSE)) {
        excpoints <- rep(FALSE, times = length(time)) }
    
    checkLogicalSameLength(
        x = excpoints, 
        y = time, 
        xDescription = "excpoints", 
        yDescription = "time", 
        functionName = "AUCPartial")
    
    checkSingleCharacter(
        x = inter, 
        description = "inter", 
        functionName = "AUCPartial")

    # Add T = 0 if it is missing and remove missing values if TRUE, 
    # otherwise throw error
    
    cleanData <- try(cleanconctime(
        conc = conc, time = time, 
        excpoints = excpoints, addt0 = addt0), silent = TRUE)
    
    if (is(cleanData, "try-error")) {
        stop(paste("Error in AUCPartial: Error during data cleaning", 
                as.character(cleanData), collapse = "\n"))
    }
    
    if (sum(cleanData$conc, na.rm = TRUE) == 0) {
        aucp <- as.numeric(0)
        
        return(aucp)
    }
    
    conc <- cleanData$conc
    
    time <- cleanData$time
    
    
    #######################################################
    
    # check endtime occurs during time
    
    if (endtime < min(time)) {
        return(aucp)
    }
    
    #######################################################
    
    # if endtime is actually an element of the time vector, 
    # we can fall back on standard AUC functions
    
    if (endtime %in% time) {
        # endtimeIndex : integer with index of endtime inside time
        endtimeIndex <- match(endtime, time)
        
        aucp <- AUCLast(conc = head(conc, n = endtimeIndex), 
            time = head(time, n = endtimeIndex), 
            addt0 = FALSE, inter = inter)
        
        return(aucp)
    }
    
    #######################################################
    
    # if endtime > tlast, we need to extrapolate final area
    
    cLastTLast <- ClastTlast(conc = conc, time = time)
    
    if (endtime > cLastTLast$tlast) {
        
        if (!is.null(lambdaZStats)) {
            
            if (!is.null(lamznpt)) { 
                warning("both lamznpt and lambdaZStats provided to predictConc, ignoring lamznpt") 
            }
            
            if (is(lambdaZStats, "list")) {
                warning("lambdaZStats has been unlisted")
                
                lambdaZStats <- unlist(lambdaZStats)
            }
            
            checkLambdaZStats(lambdaZStats = lambdaZStats)
            
            aucterm <- try(getTerminalAUC(conc = conc, time = time, 
                    endtime = endtime, lambdaZStats = lambdaZStats), silent = TRUE)
            
            if (is(aucterm, "try-error")) {
                
                stop(paste(
                        "Error in AUCPartial from call to getTerminalAUC with lambdaZStats, message was: ", 
                        aucterm, sep = "", collapse = ""))
            }
        } else {
            
            # calculate the terminal AUC using integral of 
            # exponential function (aucterm)
            excpoints <- cleanData$excpoints
            
            if (!is.null(usepoints))  { usepoints <- cleanData$usepoints }
            
            aucterm <- try(getTerminalAUC(conc = conc, time = time, 
                    endtime = endtime, lamznpt = lamznpt, 
                    usepoints = usepoints, excpoints = excpoints, useObs = useObs,
                    maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, numhalflife = numhalflife), 
                silent = TRUE)
            
            if(is(aucterm, "try-error")) {
                
                stop(paste(
                        "Error in AUCPartial from call to getTerminalAUC performing complete calculation, message was: ", 
                        aucterm, sep = "", collapse = ""))
            }
        }
        
        auclast <- AUCLast(conc = conc, time = time)
        
        aucp <- auclast + aucterm
        
        return(aucp)
    }
    
    #######################################################
    
    # endtime occurs between T0 and TLast
    
    # find the last element which endtime exceeds
    # t1Index the index of the largest time less than endtime
    # t2Index is next next time after t1Index
    
    t1Index <- tail( which(time < endtime), n = 1 )
    t2Index <- t1Index + 1
    
    # t1, t2 = left time, right time (of interval containing endtime)
    # c1, c2 = left concentration, right concentration (of interval 
    # containing endtime)
    
    t1 <- time[t1Index]
    t2 <- time[t2Index]
    c1 <- conc[t1Index]
    c2 <- conc[t2Index] 
    
    # cInter is the interpolated concentration
    
    cInter <- c1 + abs( (endtime - t1) / (t2 - t1) ) * (c2 - c1)
    
    # concBeforeEndtime, timeBeforeEndtime : vectors of concentrations and 
    # times that occur before endtime
    
    concBeforeEndtime <- head(conc, n = t1Index)
    timeBeforeEndtime <- head(time, n = t1Index)
    
    aucp <- sum(AUCLin(conc = c(concBeforeEndtime, cInter), 
            time = c(timeBeforeEndtime, endtime)))
    
    return(aucp)
}


#' Area Under Terminal concentration time Curve from T = Last to T = endtime
#'
#' This function calculates the area under the terminal phase exponential
#' from TLast to endtime. This calculation is the integral of the Terminal 
#' phase exponential decay function: 
#' \deqn{\int intercept \ast exp\left ( -\lambda _{z} \ast T\right ) dT = 
#'     -intercept / \left ( \lambda _{z} \ast exp\left ( \lambda _{z} \ast T\right )\right)}
#' The partial area under the terminal concentration time curve can be calculated 
#' by subtracting modeled AUC0Tlast from modeled AUC0endtime.
#' This is intended to be used as an internal function from \code{\link{AUCPartial}}, so no 
#' data checking is performed so clean data is assumed (see \code{cleanconctime}).
#'
#' @inheritParams AUCPartial
#' @title Area Under Terminal concentration time Curve
#' @return Single numeric with partial area under curve from TLast to endtime
#' @seealso \code{\link{lambdaZStatistics}} for more about the lamznpt parameter
#' @author Mango Business Solutions
#' @keywords math

getTerminalAUC <- function(conc, time, endtime, lambdaZStats = NULL, 
    lamznpt = NULL, usepoints = NULL, excpoints = FALSE, 
    minpoints = 3, useObs = FALSE, 
    maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1) {
    
    termAuc <- NA_real_
    
    if (is.na(endtime)) {
        
        return(termAuc)
    }
    
    if (is.null(lambdaZStats)) {
        
        # check that lamznpt is valid, return NA if it is not  
        
        checkSingleNumeric(lamznpt, description = "lamznpt", 
            functionName =  "getTerminalAUC")
        
        if (!(lamznpt >= minpoints && lamznpt <= length(time) && 
            floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste(
                    "Invalid value of lamznpt:", lamznpt, 
                    "in getTerminalAUC", collapse = " "))
            
            return(termAuc)
        }
    }
    
    
    # calculate if not known
    
    if (!is.null(lambdaZStats)) {
        
        if (!is.null(lamznpt)) { 
            warning("both lamznpt and lambdaZStats provided to getTerminalAUC, ignoring lamznpt")
        }
        
        if (is(lambdaZStats, "list")) {
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {
        
        ## Terminal phase calculation 
        # TODO see ncaComplete
        doPoints <- list(ACTION = "fail")
        
        # T=0 not checked here
        
        doPoints <- chooseNumPointsAction(
            conc = conc, time = time, lamznpt = lamznpt, 
            usepoints = usepoints, excpoints = excpoints)
        
        result <- rep(NA_real_, 9L)
        
        lzColNames <- c("LAMZ", "intercept", "R2", "R2ADJ", "CORRXY", 
            "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt")
        
        names(result) <- lzColNames
        
        lamznpt_result <- try(
            switch(
                doPoints[["ACTION"]], 
                # if lamznpt is zero or less, suppress terminal phase calculation
                none = list(lamznpt = NA_real_, result = result),
                
                # if lamznpt is one or more, suppress automatic selection
                fixed = fixedPoints(conc = conc, time = time, lamznpt = lamznpt, 
                    minpoints = doPoints[["MINROWSFORLAMBDAZ"]]),
                
                # if lamznpt is NA, perform automatic point selection
                auto = selectPoints(conc = conc, time = time, 
                    minpoints = doPoints[["MINROWSFORLAMBDAZ"]], 
                    method = "ars", excpoints = excpoints, 
                    maxdiffrsq = maxdiffrsq, minr2adj = minr2adj, 
                    numhalflife = numhalflife),
                
                # if usepoints is logical, calculate lambdaz using 
                # specified subset of data
                used = usedPoints(conc = conc, time = time, usepoints = usepoints, 
                    excpoints = excpoints, minpoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
                
                # else error
                stop(paste("Error in getTerminalAUC: doPoints action was", 
                        doPoints[["ACTION"]], sep = "", collapse = ""))), 
            silent = TRUE)
        
        # return with error
        if (is(lamznpt_result, "try-error")) {
            
            stop(paste("error in getTerminalAUC, action in doPoints was", 
                    doPoints[["ACTION"]], "message was", lamznpt_result, 
                    sep = "", collapse = ""))
        }
        
        if (is.na(lamznpt_result$lamznpt)) { return(termAuc) }
        
        lambdaZStats <- unlist(lamznpt_result$result)
    }
    
    cLastTLast <- ClastTlast(conc = conc, time = time)
        
    intercept <- lambdaZStats["intercept"]
    
    lambdaz <- lambdaZStats["LAMZ"]
    
    if (useObs) {
        logcLastPred <- log(intercept) - lambdaz * cLastTLast$tlast
        
        logadjust <- log(cLastTLast$clast) - logcLastPred
        
        intercept <- exp(log(intercept) + logadjust)
    }

    auc0last <- -intercept / (lambdaz * exp(lambdaz * cLastTLast$tlast))
    
    auc0end <- -intercept / (lambdaz * exp(lambdaz * endtime))
    
    termAuc[1] <- auc0end - auc0last
    
    return(termAuc)   
}
