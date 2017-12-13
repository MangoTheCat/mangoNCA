# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Concentration Time Curve from T = 0 to T = endTime
#'
#' Calculates the area under a time-concentration curve from the first time up until the \code{endTime}.  
#' \code{endTime} need not be one of the elements of \code{Time}, but it should not lie before the minimum time.
#' If the endTime does not coincide with an existing time element and is less than tlast, the following 
#' interpolation formula will be used to calculated a new concentration: 
#' \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  
#' If endTime is greater than tlast the extrapolated area is calculated using \code{getTerminalAUC} 
#' which calculates the area under the terminal phase exponential from TLast to endTime.
#' \code{numPoints} is calculated by \code{\link{getPartialAUC}} .  
#' This calculation is the integral of the Terminal phase exponential decay function: 
#' \deqn{\int intercept \ast exp\left ( -\lambda _{z} \ast T\right ) dT = 
#'     -intercept / \left ( \lambda _{z} \ast exp\left ( \lambda _{z} \ast T\right )\right)}
#' The partial area under the terminal concentration time curve can be calculated 
#' by subtracting modeled AUC0Tlast from modeled AUC0endTime.
#' \code{numPoints} may be NA provided endTime < Tlast.
#' 
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param endTime : last time at which area should be calculated
#' @param numPoints : Number of points to use for lambdaz calculation.
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if numPoints is not.
#' @param usePoints If NULL (default) automatically select, else, logical vector of points to use (\code{TRUE}) for calculation of terminal phase.
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param minPoints single numeric, the fewest points for which a linear model fit should be attempted (default 3)
#' @param addT0 single logical, should T=0, C=0 be added to the input data if T0 is missing? (default \code{FALSE})
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @title Partial Area Under Concentration Time Curve
#' @return Single numeric with partial area under curve
#' @export
#' @seealso \code{\link{lambdaZStastics}} for more about the numPoints parameter
#' @author Mango Business Solutions
#' @keywords math


AUCPartial <- function(Conc, Time, endTime, numPoints = NULL, lambdaZStats = NULL, 
    usePoints = NULL, excPoints = FALSE, minPoints = 3, addT0 = FALSE, inter = "Linear")
{
    
    checkOrderedVector(Time, description = "Time", functionName =  "AUCPartial")    
    
    checkSingleNumeric(endTime, description = "endTime", functionName =  "AUCPartial")
    
    aucp <- as.numeric(NA)
    
    if (is.na(endTime)) {
        
        return(aucp)
    }
    
    if (is.null(lambdaZStats)) {
        
        checkSingleNumeric(numPoints, description = "numPoints", functionName =  "AUCPartial")
        
        if( !(numPoints >= minPoints && numPoints <= length(Time) && floor(numPoints) ==  numPoints) ) {
            
            warning(paste("Invalid value of numPoints:", numPoints, "in AUCInfObs", collapse = " "))
            
            return(aucp)
        }
    }
    
    if(!is.null(usePoints)) { checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "AUCPartial") }
    
    if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
    
    checkLogicalSameLength(excPoints, Time, "excPoints", "Time", "AUCPartial")
    
    checkSingleCharacter(inter, "inter", "AUCPartial")

    # Add T = 0 if it is missing and remove missing values if TRUE, otherwise throw error
    
    cleanData <- try(cleanConcTime(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
    
    if( is(cleanData, "try-error") ) {
    
        stop(paste("Error in AUCPartial: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        
    }
    
    if (sum(cleanData$Conc, na.rm = TRUE) == 0) {
        
        aucp <- as.numeric(0)
        
        return(aucp)
    }
    
    Conc <- cleanData$Conc
    
    Time <- cleanData$Time
    
    
    ###############################################################################
    
    # check endTime occurs during Time
    
    if (endTime < min(Time)){
        
        return(aucp)
    }
    
    
    ###############################################################################
    
    # if endTime is actually an element of the Time vector, we can fall back on standard AUC functions
    
    if( endTime %in% Time )
    {
        # endTimeIndex : integer with index of endTime inside Time
        endTimeIndex <- match(endTime, Time )
        
        aucp <- AUCLast(Conc = head(Conc, n = endTimeIndex), Time = head(Time, n = endTimeIndex), addT0 = FALSE, Safe = TRUE, inter = inter)
        
        return(aucp)
    }
    
    
    ###############################################################################
    
    # if endTime > tlast, we need to extrapolate final area
    
    cLastTLast <- ClastTlast( Conc = Conc, Time = Time )
    
    if(endTime > cLastTLast$tlast) {
        
        if (!is.null(lambdaZStats)) {
            
            if (!is.null(numPoints)) { warning("both numPoints and lambdaZStats provided to predictConc, ignoring numPoints") }
            
            if (is(lambdaZStats, "list")) {
                
                warning("lambdaZStats has been unlisted")
                
                lambdaZStats <- unlist(lambdaZStats)
            }
            
            checkLambdaZStats(lambdaZStats = lambdaZStats)
            
            aucterm <- try(getTerminalAUC(Conc = Conc, Time = Time, endTime = endTime, lambdaZStats = lambdaZStats), silent = TRUE)
            
            if(is(aucterm, "try-error")) {
                
                stop(paste("Error in AUCPartial from call to getTerminalAUC with lambdaZStats, message was: ", aucterm, sep = "", collapse = ""))
            }
            
        } else {
            
            # calculate the terminal AUC using integral of exponential function (aucterm)
            
            excPoints <- cleanData$excPoints
            
            if (!is.null(usePoints))  { usePoints <- cleanData$usePoints }
            
            aucterm <- try(getTerminalAUC(Conc = Conc, Time = Time, endTime = endTime, numPoints = numPoints, usePoints = usePoints, excPoints = excPoints), silent = TRUE)
            
            if(is(aucterm, "try-error")) {
                
                stop(paste("Error in AUCPartial from call to getTerminalAUC performing complete calculation, message was: ", aucterm, sep = "", collapse = ""))
            }
        }
        
        auclast <- AUCLast(Conc = Conc, Time = Time)
        
        aucp <- auclast + aucterm
        
        return(aucp)
        
    }
    
    
    ###############################################################################
    
    # endTime occurs between T0 and TLast
    
    # find the last element which endTime exceeds
    # t1Index the index of the largest time less than endTime
    # t2Index is next next time after t1Index
    
    t1Index <- tail( which(Time < endTime), n = 1 )
    t2Index <- t1Index + 1
    
    # t1, t2 = left time, right time (of interval containing endTime)
    # c1, c2 = left concentration, right concentration (of interval containing endTime)
    
    t1 <- Time[t1Index]
    t2 <- Time[t2Index]
    c1 <- Conc[t1Index]
    c2 <- Conc[t2Index] 
    
    # cInter is the interpolated concentration
    
    cInter <- c1 + abs( (endTime - t1) / (t2 - t1) ) * (c2 - c1)
    
    # concBeforeEndTime, timeBeforeEndTime : vectors of concentrations and times that occur before endTime
    
    concBeforeEndTime <- head(Conc, n = t1Index)
    timeBeforeEndTime <- head(Time, n = t1Index)
    
    aucp <- sum(AUCLin(Conc = c(concBeforeEndTime, cInter), Time = c(timeBeforeEndTime, endTime)))
    
    return(aucp)
}


#' Area Under Terminal Concentration Time Curve from T = Last to T = endTime
#'
#' This function calculates the area under the terminal phase exponential
#' from TLast to endTime. This calculation is the integral of the Terminal 
#' phase exponential decay function: 
#' \deqn{\int intercept \ast exp\left ( -\lambda _{z} \ast T\right ) dT = 
#'     -intercept / \left ( \lambda _{z} \ast exp\left ( \lambda _{z} \ast T\right )\right)}
#' The partial area under the terminal concentration time curve can be calculated 
#' by subtracting modeled AUC0Tlast from modeled AUC0endTime.
#' This is intended to be used as an internal function from \code{\link{AUCPartial}}, so no 
#' data checking is performed so clean data is assumed (see \code{\link{cleanConcTime}}).
#'
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param endTime : single numeric, last time until which area should be calculated
#' @param numPoints : single numeric, number of points to use for lambdaz calculation
#' @param lambdaZStats if not \code{NULL}, a list with 9 elements (see details). Must be provided if numPoints is not.
#' @param usePoints If NULL (default) automatically select, else, logical vector of points to use (\code{TRUE}) for calculation of terminal phase.
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param useObs : single logical, use observed rather than predicted CLast, defaults to FALSE
#' @title Area Under Terminal Concentration Time Curve
#' @return Single numeric with partial area under curve from TLast to endTime
#' @seealso \code{\link{lambdaZStatistics}} for more about the numPoints parameter
#' @author Mango Business Solutions
#' @keywords math

getTerminalAUC <- function(Conc, Time, endTime, lambdaZStats = NULL, 
    numPoints = NULL, usePoints = NULL, excPoints = FALSE, minPoints = 3, useObs = FALSE) {
    
    termAuc <- as.numeric(NA)
    
    if (is.na(endTime)) {
        
        return(termAuc)
    }
    
    if (is.null(lambdaZStats)) {
        
        # check that numPoints is valid, return NA if it is not  
        
        checkSingleNumeric(numPoints, description = "numPoints", functionName =  "getTerminalAUC")
        
        if( !(numPoints >= minPoints && numPoints <= length(Time) && floor(numPoints) ==  numPoints) ) {
            
            warning(paste("Invalid value of numPoints:", numPoints, "in getTerminalAUC", collapse = " "))
            
            return(termAuc)
        }
    }
    
    
    # calculate if not known
    
    if (!is.null(lambdaZStats)) {
        
        if (!is.null(numPoints)) { warning("both numPoints and lambdaZStats provided to getTerminalAUC, ignoring numPoints") }
        
        if (is(lambdaZStats, "list")) {
            
            warning("lambdaZStats has been unlisted")
            
            lambdaZStats <- unlist(lambdaZStats)
        }
        
        checkLambdaZStats(lambdaZStats = lambdaZStats)
        
    } else {
        
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
                
                stop(paste("Error in getTerminalAUC: doPoints action was", doPoints[["ACTION"]], sep = "", collapse = ""))), 
                
            silent = TRUE)
        
        # return with error
        
        if (is(numPoints_result, "try-error")) {
            
            stop(paste("error in getTerminalAUC, action in doPoints was", doPoints[["ACTION"]], "message was", numPoints_result, sep = "", collapse = ""))
        }
        
        if (is.na(numPoints_result$numPoints)) { return(termAuc) }
        
        lambdaZStats <- unlist(numPoints_result$result)
    }
    
    cLastTLast <- ClastTlast(Conc = Conc, Time = Time)
        
    intercept <- lambdaZStats["intercept"]
    
    lambdaz <- lambdaZStats["Lambdaz"]
    
    if(useObs) {
        
        logcLastPred <- log(intercept) - lambdaz * cLastTLast$tlast
        
        logadjust <- log(cLastTLast$clast) - logcLastPred
        
        intercept <- exp(log(intercept) + logadjust)
    }

    auc0last <- -intercept / (lambdaz * exp(lambdaz * cLastTLast$tlast))
    
    auc0end <- -intercept / (lambdaz * exp(lambdaz * endTime))
    
    termAuc[1] <- auc0end - auc0last
    
    return(termAuc)   
}
    

