# Date of last change: 14/06/2016
# Last changed by: ccampbell
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the concentration time Moment Curve from T = 0 to T = endtime
#'
#' Calculates the area under a concentration-time moment curve from the first time up until the \code{endtime}.  \code{endtime}
#' need not be one of the elements of \code{time}, but it should like between the minimum and maximum times 
#' Note, all error checks that apply to \code{\link{AUCLin}} apply here.  If \code{endtime} is less than
#' \code{tmin} \code{NA} is returned.  There should be no missing times.
#' If the endtime does not coincide with an existing time element, the following interpolation formula will be
#' used to calculated a new concentration (if time is less than tlast): 
#' \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  If endtime is greater than
#' tlast, then another concentration is extrapolated via the formula \deqn{c_{extr} = c_0 + \exp(- \lambda_z * endtime)  }
#' 
#' @inheritParams AUCPartial
#' @title Partial Area Under Moment Curve
#' @return Single numeric 
#' @author Mango Solutions
# TODO check if needed \code{\link{MRTInfPredSS}}

AUMCPartial <- function(conc, time, endtime, lamznpt = NULL, lambdaZStats = NULL, 
    usepoints = NULL, excpoints = FALSE, minpoints = 3, addt0 = FALSE, inter = "Linear", 
    useObs = FALSE, maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1) {
    
    checkOrderedVector(time, description = "time", functionName =  "AUMCPartial")
    
    checkSingleNumeric(endtime, description = "endtime", functionName =  "AUMCPartial")
    
    if (is.null(lambdaZStats)) {
        checkSingleNumeric(lamznpt, description = "lamznpt", functionName =  "AUMCPartial")
        
        if (!(lamznpt >= minpoints && lamznpt <= length(time) && 
            floor(lamznpt) ==  lamznpt) ) {
            
            warning(paste("Invalid value of lamznpt:", lamznpt, "in AUMCPartial", collapse = " "))
            
            return(aucp)
        }
    }
    
    # Add T = 0 if it is missing and remove missing values
    
    cleanData <- try(cleanconctime(conc = conc, time = time, addt0 = addt0), silent = TRUE)
    
    if( is(cleanData, "try-error") ) {
    
        stop(paste("Error in AUMCPartial: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        
    }

    conc <- cleanData$conc
    time <- cleanData$time
        
    aucp <- NA_real_
    
    
    ###############################################################################
    
    # check endtime occurs during time
    
    if( endtime < min(time) ){
    
        return(aucp)
        
    }
    
    
    ###############################################################################
    
    # if endtime is actually an element of the time vector, we can fall back on standard AUC functions
    
    if (endtime %in% time) {
        # endtimeIndex : integer with index of endtime inside time
        endtimeIndex <- match(endtime, time )
        
        aucp <- sum(AUCLin( time = head(time, n = endtimeIndex), 
            conc = head(conc, n = endtimeIndex) * head(time, n = endtimeIndex) ))
        
        return(aucp)
    }
    
    
    ###############################################################################
    
    # if endtime > tlast, we need to extrapolate a new concentration element.  
  
    cLastTLast <- ClastTlast( conc = conc, time = time )
    
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
            
            # calculate the terminal AUC using integral of exponential function (aucterm)
            excpoints <- cleanData$excpoints
            
            if (!is.null(usepoints))  { usepoints <- cleanData$usepoints }
            
            lambdaZStats <- try(lambdaZStatistics(conc = conc, time = time, 
                    lamznpt = lamznpt), 
                silent = TRUE)
            
            if(is(lambdaZStats, "try-error")) {
                
                stop(paste(
                        "Error in AUMCPartial from call to lambdaZStatistics performing complete calculation, message was: ", 
                        lambdaZStats, sep = "", collapse = ""))
            }
        }
        
        
        auclast <- AUCLast(conc = conc * time, time = time)
        
        # calculate the terminal AUMC using integral of exponential function (aucterm)

        intercept <- lambdaZStats$intercept
        lambdaz <- lambdaZStats$Lambdaz

        auc0last <- -intercept / (lambdaz * exp(lambdaz * cLastTLast$tlast))
        auc0end <- -intercept / (lambdaz * exp(lambdaz * endtime))
    
        aucterm <- auc0end - auc0last
        
        aucp <- auclast + aucterm
        
        return(aucp)
        
    }
    
    
    ###############################################################################
    
    # endtime occurs between T0 and TLast
    
    # find the last element which endtime exceeds
    # t1Index the index of the largest time less than endtime
    # t2Index is next next time after t1Index
    
    t1Index <- tail( which(time < endtime), n = 1 )    
    t2Index <- t1Index + 1
    
    # t1, t2 = left time, right time (of interval containing endtime)
    # c1, c2 = left concentration, right concentration (of interval containing endtime)
    
    t1 <- time[t1Index]
    t2 <- time[t2Index]
    c1 <- conc[t1Index]
    c2 <- conc[t2Index] 
    
    # cInter is the interpolated concentration
    
    cInter <- c1 + abs( (endtime - t1) / (t2 - t1) ) * (c2 - c1)
    
    # concBeforeEndtime, timeBeforeEndtime : vectors of concentrations and times that occur before endtime 
    
    concBeforeEndtime <- head(conc, n = t1Index)
    timeBeforeEndtime <- head(time, n = t1Index)
    
    aucp <- sum(AUCLin( conc = c(concBeforeEndtime * timeBeforeEndtime, cInter * endtime), time = c(timeBeforeEndtime, endtime ) ) )
    
    return(aucp)
}

