# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Try to Calculate lambdaz and Related Parameters
#'
#' Calculates the lambdaz estimate and related parameters.
#' The \code{lm} function is used to fit a linear regression of the last \code{numPoints}
#' concentration against time.  
#' These are counted from (and include) the last measurable concentration. 
#' The number of points does not necessarily correspond to the index of the input dataset since rows may be removed at a previous step.
#' Components of the \code{"lm"} object are then used to compute lambdaz and related statistics.
#' Returns a list of single numeric values:
#' \enumerate{ 
#'      \item \code{Lambdaz}: The negation of the slope of the regression line of the last 
#'      \item \code{intercept}: The exponential of the intercept term of the regression
#'      \item \code{r2}: The R^2 of the regression fit
#'      \item \code{adjr2}: The adjusted R^2 of the regression fit
#'      \item \code{rhoXY}: The correlation coefficient of the observed fit.
#'      \item \code{tPhaseHalfLife}: Terminal phase half-life, equal to ln(2) / Lambdaz 
#'      \item \code{LambdazLower}: Lower bound of time used in lambda z calculation
#'      \item \code{LambdazUpper}: Upper bound of time used in lambda z calculation
#'      \item \code{numPoints}: Number of points used in calculation
#' }
#' Note that the following additional error checking / additional processing will be performed on the inputs:
#' \enumerate{
#'      \item Missing elements (NA) of the time and concentration vectors will be removed during regression
#'          (a missing entry in one vector will cause the corresponding value in the other to be removed as well)
#'      \item If the sum of the (last \code{numPoints}) concentrations is 0, missing values will be returned for all elements of the returned list.
#'      \item \code{Conc} and \code{Time} must be numeric vectors of equal length, or an exception will be generated
#'      \item \code{Conc} has fewer than 2 elements, NA will be returned for all values
#'      \item If the linear regression fails for some reason, a warning will be emitted and NA will be returned for all values
#'      \item An exception will be generated if numPoints is not a single integer numeric between 3 and  \code{length(Conc)} (inclusive) 
#' }
#' This function is called from \code{selectPoints} that removes rows containing Cmax before passing them to \code{lambdaZStatistics}.
#' Therefore there is the option to supress Cmax check by setting the argument checkCmax to \code{FALSE}
#' 
#' @title Calculate lambdaz and related parameters
#' @param Conc numeric vector of concentrations
#' @param Time numeric vector of time points.  Must be sorted in ascending order
#' @param numPoints Number of points to use for the linear regression, counted from the end of the concentration/time vectors.
#' Must be a single integer greater than minPoints and less than or equal to the length of \code{Time} and \code{Conc}.
#' @param minPoints Single numeric declaring minimum acceptable number of rows for linear model. Default is 3, minimum accepted value is 2.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @param checkCmax Single logical value declaring whether Cmax is included in the data (default is TRUE).
#' @return A list with a set of length 1 numeric vectors.  
#' @keywords math
#' @export
#' @examples
#'      Theoph2 <- subset(Theoph, Subject == 2)
#'      lambdazStatistics(Theoph2$conc, Theoph2$Time)


lambdaZStatistics <- function(Conc, Time, numPoints = 3, Safe = TRUE, checkCmax = TRUE, minPoints = 3) {
    
    if (Safe) {
        
        # data checks
        
        checkNumericSameLength(Time, Conc, "Time", "Concentration", "lambdaZStatistics")
        
        checkSingleNumeric(numPoints, description = "numPoints", "lambdaZStatistics")
        
        checkSingleLogical(checkCmax, description = "checkCmax", "lambdaZStatistics")
        
        checkSingleNumeric(minPoints, description = "minPoints", "lambdaZStatistics")
    }
    
    
    if (minPoints < 2) { stop("minPoints must be at least 2 in lambdaZStatistics") }
    
    # Initialize list of returned parameters
    # result : named list of parameters to return.  Each is initialized to NA, and is a length-1 numeric vector
    # see the comment under @return in the header for details
    
    result <- list(Lambdaz = as.numeric(NA), intercept = as.numeric(NA), 
        r2 = as.numeric(NA), adjr2 = as.numeric(NA), 
        rhoXY = as.numeric(NA), tPhaseHalfLife = as.numeric(NA), 
        LambdazLower = as.numeric(NA), LambdazUpper = as.numeric(NA),
        numPoints = as.numeric(NA))
    

    # is numPoints is NA or less than minPoints, all lambdaz statistics are NA

    if (is.na(numPoints)) { numPoints <- 0 }
    
    if (numPoints < minPoints)
    {
        return(result)
    }
    
    if( sum(Conc, na.rm = TRUE) <= 0 )
    {
        warning(paste("Warning in lambdazStatistics: sum of Conc is <= 0"))
        
        return(result)
    }
    
    timeConc <- stripTrailingZeros(Conc = Conc, Time = Time, checkT0 = FALSE, Safe = Safe)
    
    # check that there are at least 3 rows after Cmax, otherwise return list of NAs

    if (!testTrailPoints(Conc = timeConc$Conc, Time = timeConc$Time, minPoints = minPoints, checkCmax = checkCmax, Safe = Safe)) {
        
        return(result)
    }
    
    # check that numPoints is valid, return NA if it is not 

    if(!(numPoints <= nrow(timeConc) && identical(floor(numPoints), numPoints)) ) {
        
        warning(paste("Warning in lambdazStatistics: invalid value of numPoints:", 
            numPoints, collapse = " ") )
            
        return(result)
    }
    
    # use only numPoints rows
    
    timeConc <- tail(timeConc, n = numPoints)
    
    # take log and remove -Inf values
    
    timeConc$logConc <- log(timeConc$Conc)
    
    timeConc[timeConc$logConc == -Inf, "logConc"] <- NA
    
    # lmFit : object of class lm holding regression results (or try-error if something goes wrong)
    
    lmFit <- try(getLambdaz(timeConc), silent = TRUE)
    
    if( is( lmFit, "try-error" ) )
    {
        warning("Call to lm function failed, so regression could not be calculated successfully")
        return(result)
    }
    
    # lmFitSummary : summary.lm object holding summary of lmFit
    
    lmFitSummary <- summary(lmFit, correlation = TRUE)

    # Correlation Coefficient calc 
 
    rhoXY <- cor(timeConc[, c("logConc", "Time")], use = "complete.obs")[2, 1]
    
    # populate output
    
    result$Lambdaz <- unname(coef(lmFit)["Time"]) * -1.0
    
    result$intercept <- exp(unname(coef(lmFit)["(Intercept)"]))
    
    result$r2 <- lmFitSummary$r.squared
    
    result$adjr2 <- lmFitSummary$adj.r.squared
    
    result$rhoXY <- rhoXY
    
    result$tPhaseHalfLife <- log(2) / unname(result$Lambdaz)
    
    # NOTE : here we are using the data from the regression fit object, which has omitted missing values
    # from the original data set. This seems like the correct approach to me, though legacy code used 
    # the original data.frame timeConc for extracting these data points
    # TODO Compare with WNL
    
    result$LambdazLower <- head(lmFit$model$Time, 1)
    
    result$LambdazUpper <- tail(lmFit$model$Time, 1)
    
    # The number of points should INCLUDE omitted points so that index corresponds to the input dataset
    
    numberRows <- nrow(lmFit$model)
    
    if( !setequal(numberRows, numPoints) )
    {
        warning(paste("Number of rows used for lambdaz calculation (", numberRows, 
            ") does not equal numPoints (", numPoints, ")", collapse = ""))
    }
    
    result$numPoints <- numPoints

    return(result)
}



#' Calculate lambdaz using lm
#'
#' Calculates lambdaz for a data frame consisting of logConc and Time.
#' The \code{lm} function is used to fit a linear regression of the last \code{numPoints}
#' concentration against time.  These are counted from (and include) the last measurable concentration.  
#'
#' @title Perform \code{lm} on logConc-Time data
#' @param timeConc Data frame with columns logConc and Time used for calculating lambdaz.
#' @return \code{"lm"} object to be used by \code{getLambdaZStatistics}.

getLambdaz <- function(timeConc) {

    if( !all(is.element(c("Time", "logConc"), names(timeConc))) ) 
    {
        stop("Error in getLambdaz: data frame timeConc passed from lambdaZStatistics does not have columns logConc and Time")
    }
    
    lmFit <- lm(logConc ~ Time, na.action = na.exclude, data = timeConc)
    
    return(lmFit)
    
}
