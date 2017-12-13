# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 03/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Area Under the Concentration Time Moment Curve from T = 0 to T = endTime
#'
#' Calculates the area under a concentration-time moment curve from the first time up until the \code{endTime}.  \code{endTime}
#' need not be one of the elements of \code{Time}, but it should like between the minimum and maximum times 
#' Note, all error checks that apply to \code{\link{AUC}} apply here.  If \code{endTime} is less than
#' \code{tmin} \code{NA} is returned.  There should be no missing times.
#' If the endTime does not coincide with an existing time element, the following interpolation formula will be
#' used to calculated a new concentration (if time is less than tlast): 
#' \deqn{c_{inter} = c_1 +  \left| \frac{ t^{end} - t_1}{t_2 - t_1} \right|  (c_2 - c_1)}.  If endTime is greater than
#' tlast, then another concentration is extrapolated via the formula \deqn{c_{extr} = c_0 + \exp(- \lambda_z * endTime)  }
#' 
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param endTime : last time at which area should be calculated.  Must be greater than the smallest element of Time, else NA is returned.
#' @param numPoints : Number of points to use for lambda-z calculation.  This will only be used if endTime > tlast to 
#' extrapolate a new concentration point
#' @title Partial Area Under Moment Curve
#' @return Single numeric 
#' @author Mango Solutions
#' @keywords math


AUMCPartial <- function(Conc, Time, endTime, numPoints, addT0 = TRUE)
{
    
    checkOrderedVector(Time, description = "Time", functionName =  "AUMCPartial")    
    checkSingleNumeric(endTime, description = "endTime", functionName =  "AUMCPartial")
    checkSingleNumeric(numPoints, description = "numPoints", functionName =  "AUMCPartial")
    
    # Add T = 0 if it is missing and remove missing values
    
    cleanData <- try(cleanConcTime(Conc = Conc, Time = Time, addT0 = addT0), silent = TRUE)
    
    if( is(cleanData, "try-error") ) {
    
        stop(paste("Error in AUMCPartial: Error during data cleaning", as.character(cleanData), collapse = "\n"))
        
    }

    Conc <- cleanData$Conc
    Time <- cleanData$Time
        
    aucp <- as.numeric(NA)
    
    
    ###############################################################################
    
    # check endTime occurs during Time
    
    if( endTime < min(Time) ){
    
        return(aucp)
        
    }
    
    
    ###############################################################################
    
    # if endTime is actually an element of the Time vector, we can fall back on standard AUC functions
    
    if(endTime %in% Time)
    {
        # endTimeIndex : integer with index of endTime inside Time
        endTimeIndex <- match(endTime, Time )

        aucp <- sum(AUCLin( Time = head(Time, n = endTimeIndex), Conc = head(Conc, n = endTimeIndex) * head(Time, n = endTimeIndex) ))
        
        return(aucp)
    }
    
    
    ###############################################################################
    
    # if endTime > tlast, we need to extrapolate a new concentration element.  
  
    cLastTLast <- ClastTlast( Conc = Conc, Time = Time )
    
    if(endTime > cLastTLast$tlast)
    {
        
        auclast <- AUCLast(Conc = Conc * Time, Time = Time)
        
        # calculate the terminal AUMC using integral of exponential function (aucterm)

        lambdaZStats <- lambdaZStatistics(Conc = Conc, Time = Time, numPoints = numPoints)

        intercept <- lambdaZStats$intercept
        lambdaz <- lambdaZStats$Lambdaz

        auc0last <- -intercept / (lambdaz * exp(lambdaz * cLastTLast$tlast))
        auc0end <- -intercept / (lambdaz * exp(lambdaz * endTime))
    
        aucterm <- auc0end - auc0last
        
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
    
    aucp <- sum(AUCLin( Conc = c(concBeforeEndTime * timeBeforeEndTime, cInter * endTime), Time = c(timeBeforeEndTime, endTime ) ) )
    
    return(aucp)
}

