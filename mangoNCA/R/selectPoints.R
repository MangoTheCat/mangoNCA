# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 25/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Choose Number of Trailing Points to use for Terminal Rate Constant (Lambdaz) Calculation.
#'
#' Calculates number of points to use for Terminal Rate Constant (Lambdaz) calculation based on Adjusted R-squared method.
#' selectPoints calls \code{\link{lambdaZStatistics}} to perform regressions using the last three rows, 
#' then the last four rows, last five, etc. Rows prior to Cmax are not used.
#' Exclusion of rows prior to the end of infusion is not currently implemented.
#' For each regression, an adjusted R2 is computed. The regression with the largest adjusted R2 is 
#' selected to estimate lambdaz, with these caveats:
#'  \enumerate{
#'      \item Number of rows with the greatest number of rows AND within MAXDIFFRSQ (0.0001) of the largest adjusted R2 value is selected.
#'      \item Rows where Conc <= 0 are not used in the calculation
#'      \item Trailing rows where Conc <= 0 are removed by \code{stripTrailingZeros} and not counted in number returned
#'      \item Cmax is not included in Lambdaz calculation (User Requirements PEX-010)
#'      \item lambdaz must be greater than MINLAMBDAZ (0)
#'      \item lambdaz must be calculated using at least minPoints (3) data rows
#'      \item adjr2 must be at least MINADJR2 (0.8)
#'      \item tPhaseHalfLife must be less than lambdaz interval (LambdazUpper - LambdazLower)
#'  }
#' Zero or missing values will be removed with a warning. 
#'
#' @param Conc Vector of concentrations.
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates.
#' @param minPoints Minimum number of points to use for the Lambdaz calculation(s), 3 by default. Single positive integer.
#' @param method Method of selecting points (only Adjusted R-Squared Method implemented).
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @title Number of Points for Calculating Terminal Rate Constant
#' @return List consisting of numPoints, a single numeric value, and result, a length 9 numeric vector.
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples
#'      Theoph1 <- subset(Theoph, Subject == 1)
#'      selectPoints(Conc = Theoph1$conc, Time = Theoph1$Time)

selectPoints <- function(Conc, Time, minPoints = 3, method = "ars", excPoints = FALSE, Safe = TRUE)
{

    if (!(method == "ars")[1]) {
        
	    warning("Only adjusted R squared method implemented; using method = \"ars\"")
	}
	
    if (Safe) {
        
        # data checks, or error
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "selectPoints")
        
        checkSingleNumeric(minPoints, description = "minPoints", functionName = "selectPoints")
        
        checkOrderedVector(Time, description = "Time", functionName = "selectPoints")
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "selectPoints")
    }
    
    if (minPoints < 2) { stop("Error in selectPoints: minPoints must be at least 2") }   
 
    timeConc <- try(cleanConcTime(Conc = Conc, Time = Time, excPoints = excPoints, addT0 = FALSE, checkT0 = FALSE, Safe = Safe), silent = TRUE)
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("call to cleanConcTime failed, message is: ", timeConc))
    }
    
    # initialize outputs
    
    numPoints <- as.numeric(NA)
    
    result <- as.numeric(rep(NA, 9))
    
    names(result) <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")

    # lambdaz must be calculated using at least minPoints (3) data rows
    # Cmax
    
    if (!testTrailPoints(Conc = timeConc$Conc, Time = timeConc$Time, excPoints = timeConc$excPoints, minPoints = minPoints, Safe = Safe)) {
        
        warning("There are fewer non-zero points between Cmax and Clast than minPoints")
        
		return(list(numPoints = numPoints, result = result))
	}
    
    # flag rows before and including Cmax
    
    CmaxTmax <- CmaxTmax(Conc = timeConc$Conc, Time = timeConc$Time)
    
    CmaxIndex <- CmaxTmax$index    
    
    timeConc$afterCmax <- c(rep(FALSE, times = CmaxIndex), rep(TRUE, times = nrow(timeConc) - CmaxIndex))
    
    # remove excluded values
    
    timeConc <- timeConc[timeConc$afterCmax & !timeConc$excPoints, ]
    
    if (identical(nrow(timeConc), as.integer(0))) { return(list(numPoints = numPoints, result = result)) }
    
    # find index of shortest allowed dataset and count total number of models that will be created
    
    nVals <- nrow(timeConc)
    
	minIndex <- nVals - (minPoints - 1)
    
	# object to collect output
    # matrix of 
    #[1] "Lambdaz"       
    #[2] "intercept"     
    #[3] "r2"            
    #[4] "adjr2"         
    #[5] "rhoXY"         
    #[6] "tPhaseHalfLife"
    #[7] "LambdazLower"  
    #[8] "LambdazUpper"  
    #[9] "numPoints" 
    #[10] i

	vals <- matrix(as.numeric(0), nrow = minIndex, ncol = 10)
    
    vals[, 10] <- seq.int(1, minIndex, by = 1)
    
    ### Regression comparison decision rules
    
    # 1: lambdaz must be greater than MINLAMBDAZ (0)
    # 2: adjr2 must be at least MINADJR2 (0.8)
    # 3: tPhaseHalfLife must be less than lambdaz calculatedInterval (LambdazUpper - LambdazLower)

    MAXDIFFRSQ <- 1e-4
    
    MINLAMBDAZ <- 0
    
    MINADJR2   <- 0.8
    
    # perform regression for each data subset
    
    for (i in vals[, 10]) {
        
    	# each model with data subsets from Cmax onwards is calculated
        
        lambdazOut <- lambdaZStatistics(Conc = timeConc$Conc, Time = timeConc$Time, numPoints = nVals - i + 1, 
            Safe = Safe, checkCmax = FALSE, minPoints = minPoints)
      
        vals[i, 1:9] <- unlist(lambdazOut)
      
    }

    # ignore adjr2 val [4] by replacing with NA based on regression comparison decision rules 
        # 1: [1] lambdaz must be greater than MINLAMBDAZ (0)
        # 2: [4] adjr2 must be at least MINADJR2 (0.8)
        # 3: [6] tPhaseHalfLife must be less than calculatedInterval [8] LambdazUpper - [7] LambdazLower
        
    vals[vals[, 1] <= MINLAMBDAZ | vals[, 4] < MINADJR2 | vals[, 6] > vals[, 8] - vals[, 7], 4] <- NA
    
    # find max value if present and return
    
    if (!all(is.na(vals[, 4])))
    {
        maxadjrs <- max(vals[, 4], na.rm = TRUE)
        
        valsWithinLimIndex <- which((maxadjrs - vals[, 4]) <= MAXDIFFRSQ)
        
        if (length(valsWithinLimIndex) > 0) {
            
            numPointsIndex <- min(valsWithinLimIndex)
            
            numPoints <- nVals - numPointsIndex + 1
            
            result[1:9] <- vals[numPointsIndex, 1:9, drop = TRUE]
        }
    }
    
    # The number of points should INCLUDE omitted points so that index corresponds to the input dataset
    
	return(list(numPoints = numPoints, result = result))
    
}


#' Use Fixed Number of Trailing Points for Terminal Rate Constant (Lambdaz) Calculation.
#'
#' Returns input numPoints and results for Terminal Rate Constant (Lambdaz) calculation
#' fixedPoints calls \code{\link{lambdaZStatistics}} to perform regressions using the specified number of trailing non-zero rows
#' Rows prior to Cmax may be used.
#' Exclusion of rows prior to the end of infusion is not currently implemented.
#' Zero or missing values will error. 
#'
#' @param Conc Vector of concentrations.
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates.
#' @param minPoints Minimum number of points to use for the Lambdaz calculation(s), 3 by default. Single positive integer.
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @param minPoints Single numeric declaring minimum acceptable number of rows for linear model.
#' @title Fixed Points for Calculating Terminal Rate Constant
#' @return List consisting of numPoints, a single numeric value, and result, a length 9 numeric vector.
#' @export
#' @author Mango Solutions
#' @keywords math
#' @examples
#'      Theoph1 <- subset(Theoph, Subject == 1)
#'      fixedPoints(Conc = Theoph1$conc, Time = Theoph1$Time, numPoints = 3)

fixedPoints <- function(Conc, Time, numPoints, excPoints = FALSE, minPoints = 3, Safe = TRUE) {
    
    if (Safe) {
        
        # data checks, or error
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "fixedPoints")
        
        checkOrderedVector(Time, description = "Time", functionName = "fixedPoints")
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "fixedPoints")
      
        checkSingleNumeric(numPoints, description = "numPoints", functionName = "fixedPoints") 
       
        checkSingleNumeric(minPoints, description = "minPoints", functionName = "fixedPoints")
    }
    
    if (minPoints < 2) { stop("Error in fixedPoints: minPoints must be at least 2") }   
 
    timeConc <- try(cleanConcTime(Conc = Conc, Time = Time, excPoints = excPoints, addT0 = FALSE, checkT0 = FALSE), silent = TRUE)
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("call to cleanConcTime failed, message is: ", timeConc))
    }
    
    # initialize outputs
    
    result <- as.numeric(rep(NA, 9))
    
    names(result) <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
    
    # remove excluded values
    
    timeConc <- timeConc[!timeConc$excPoints, ]
    
    if (identical(nrow(timeConc), as.integer(0))) { return(list(numPoints = numPoints, result = result)) }
    
    # lambdaz must be calculated using at least minPoints (3) data rows
    # Cmax
    
    if (any(nrow(timeConc) < minPoints) || any(numPoints < minPoints)) {
        
        if (any(nrow(timeConc) < minPoints)) {
            
            warning("There are fewer non-zero, non-excluded points in the data than minPoints")
        }
        
        if (any(numPoints < minPoints)) {
            
            warning("numPoints is less than than minPoints")
        }
        
        numPoints <- as.numeric(NA)
        
    } else {
        
        result[1:9] <- unlist(lambdaZStatistics(Conc = timeConc$Conc, Time = timeConc$Time, numPoints = numPoints, checkCmax = FALSE))
    }
    
    return(list(numPoints = numPoints, result = result))
    
}


#' Use Selection to Calculate Lambdaz
#'
#' Value is NA if selection would be less than minPoints.
#' The number of points will not usually be related to the index of the points used, 
#' since any rows may be selected by the user
#'
#' @title Use Selection to Calculate Lambdaz
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param usePoints Logical vector selecting points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param minPoints Single numeric declaring minimum number of points requred for lambdaz calculation.
#' @return Single numeric value
#' @export
#' @author Mango Solutions
#' @examples
#'      usedPoints(Conc = c(435, 363, 113, 47,0), Time = 0:4, usePoints = c(TRUE, TRUE, TRUE, FALSE, FALSE))
#'      usedPoints(Conc = c(435, 363, 113, 47,0), Time = 0:4, usePoints = rep(TRUE, 5), 
#'          excPoints = c(FALSE, FALSE, TRUE, FALSE, FALSE))
#' @keywords math

usedPoints <- function(Conc, Time, usePoints, excPoints = FALSE, minPoints = 3, Safe = TRUE)
{
    
    if (Safe) {
        
        # data checks, or error
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "usedPoints")
        
        checkOrderedVector(Time, description = "Time", functionName = "usedPoints")
        
        checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "usedPoints")
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "usedPoints")
       
        checkSingleNumeric(minPoints, description = "minPoints", functionName = "usedPoints")
    }
    
    if (minPoints < 2) { stop("Error in usedPoints: minPoints must be at least 2") }   
    
    # check data
    
    timeConc <- try(cleanConcTime(Conc = Conc, Time = Time, excPoints = excPoints, usePoints = usePoints, 
        addT0 = FALSE, checkT0 = FALSE), silent = TRUE)
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("call to cleanConcTime failed, message is: ", timeConc))
    }
    
    # initialize output
    
    numPoints <- as.numeric(NA)
    
    result <- as.numeric(rep(NA, 9))
    
    names(result) <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
    
    # remove excluded values
    
    timeConc <- timeConc[timeConc$usePoints & !timeConc$excPoints, ]
    
    # if all rows are gone, there are insufficient rows for lambdaz calculation
    
    if (identical(nrow(timeConc), as.integer(0))) { return(list(numPoints = numPoints, result = result)) }
    
    # test using usePoints, ignoring Cmax restrictions and excluding excPoints

    numPoints <- as.numeric(nrow(timeConc))
    
    if (numPoints < minPoints) {
        
        warning("There are fewer non-zero, non-excluded points in the data than minPoints")
        
        numPoints <- as.numeric(NA)
        
    } else {
        
        result[1:9] <- unlist(lambdaZStatistics(Conc = timeConc$Conc, Time = timeConc$Time, numPoints = numPoints, checkCmax = FALSE))
    }
    
    return(list(numPoints = numPoints, result = result))
	
}
