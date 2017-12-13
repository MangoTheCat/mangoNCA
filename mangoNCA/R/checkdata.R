# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 24/05/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Check Two Numeric Vectors have the Same Length
#'
#' Utility function to check whether two vectors are numeric and of the same length.
#' Stops with an error message if either object is not a numeric vector or if
#' the vectors are of different lengths.  Single pair of numeric NAs will be treated as having the same length.
#'
#' @title Check For Numeric Vectors Same Length
#' @param x first object, should be a numeric vector
#' @param y second object, should be a numeric vector
#' @param xDescription character string giving a descriptive name for the
#'   first vector.  Used in error messages.  Defaults to "x".
#' @param yDescription character string giving a descriptive name for the
#'   second vector.  Used in error messages.  Defaults to "y"
#' @param functionName character string giving the name of the function
#'   calling this function.  Used in error messages.  If not supplied, the default determines
#'   which function invoked this one 
#' @keywords error
#' @note If x is not a numeric vector, the error message will be of the form "Error in [functionName]: [xDescription] is not a numeric vector",
#'  and similarly if y is not numeric (replacing yDescription where appropriate).  If the two vectors are not of the same length,
#'  the error will be "Error in [functionName]: lengths of [xDescription] and [yDescription] do not match"   
#' @examples 
#' # standard use 
#' checkNumericSameLength(Theoph$conc, Theoph$Time)
#' # generate error 
#' \dontrun{
#'  checkNumericSameLength(1:10, 1:9)
#' }
#' @return returns NULL invisibly if no error was generated.
#' @noRd

checkNumericSameLength <- function(x, y, 
        xDescription = "x", yDescription = "y",
        functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) 
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        xDescription, " is missing with no default", sep = ""))
    }
    
    if( missing(y) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        yDescription, " is missing with no default", sep = ""))
    }
    
    # Note a vector of NA values is a logical by default, hence the more complex check
    
    if( !((is.numeric(x) || (is.logical(x) && all(is.na(x)))) && is.vector(x)) )
    {
        stop(paste("Error in ", functionName, ": ",
                        xDescription, " is not a numeric vector", sep = ""))
    }
    
    if( !((is.numeric(y) || (is.logical(y) && all(is.na(y)))) && is.vector(y)) )
    {
        stop(paste("Error in ", functionName, ": ",
                        yDescription, " is not a numeric vector", sep = ""))
    }
    
    if( length(x) != length(y) )
    {
        stop(paste("Error in ", functionName, ": ",
                        " lengths of ", xDescription, " and ", yDescription, 
                        " do not match", sep = ""))
    }
    
    invisible(NULL)
    
}


#' Check if Vector is Ordered
#'
#' Utility function that checks that an object is a vector whose non-missing elements are sorted.  If the object
#' is not a vector, or is a vector but is elements are not sorted in ascending order, an exception will be generated
#' (R's ordering of vectors via \code{order} is used).
#' @param x Vector / object to check
#' @param description Single character string describing object to be checked
#' @param functionName Name of parent function (character)
#' @title Check if an object is an ordered vector
#' @note The following error messages are generated:
#' \enumerate{
#'      \item If \code{x} is not a vector, the error message will be \code{Error in [functionName] : [description] is not a vector}
#'      \item \code{x} is not ordered, the error message will be \code{Error in [functionName] : [description] is not ordered  }
#'  }
#' @return NULL invisibly
#' @author Mango Solutions
#' @keywords error
#' @examples 
#' \dontrun{
#'   checkOrderedVector(1:10)
#'   checkOrderedVector(10:1)
#'   checkOrderedVector(mtcars)
#' }
#' @noRd

checkOrderedVector <- function(x, description = "x", functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) )
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if(!is.vector(x))
    {
        stop(paste("Error in ", functionName, ": ",description, " is not a vector", sep=""))
    }
    
    # xNoNA : a vector equivalent to x with NAs stripped out
    
    xNoNA <- na.omit(x)
    
    if( !identical(order(xNoNA), seq_along(xNoNA)) )
    {
        stop(paste( "Error in ", functionName, ": ", description, 
                    " is not ordered.  Actual value is ", paste(x, collapse =  " ") , sep = "") )
    }
    
    invisible(NULL)
}


#' Check if Object is Single Numeric Value
#'
#' Utility function that checks that an object x is numeric and has 1 element.  If it isn't, an 
#' exception is generated with a message that also displays the object contents. 
#' Numeric NA is single numeric, logical is not.
#'
#' @param x Object to check
#' @param description Description of the object, used in the message to print
#' @param functionName Name of parent function
#' @title Check that Object is a Numeric of Length 1
#' @return NULL invisibly if the object passes the check.  Otherewise, an exception is generated
#' @author Mango Solutions
#' @examples 
#' \dontrun{
#'  checkSingleNumeric(mtcars)
#'  checkSingleNumeric(1)
#'  checkSingleNumeric("1")
#' }
#' @noRd

checkSingleNumeric <- function(x, description = "x", 
  functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) )
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if(length(x) != 1 || !is.numeric(x) || !is.vector(x))
    {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if(!is.vector(x))
        {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        }
        else
        {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a numeric of length 1.  Value is:", y, sep = " "))
    }
    
    invisible(NULL)
    
}


#' Check if Object is Single Logical Value
#'
#' Utility function that checks that an object x is logical and has 1 element.  If it isn't, an 
#' exception is generated with a message that also displays the object contents. 
#' Logical NA is single logical, numeric is not.
#'
#' @param x Object to check
#' @param description Description of the object, used in the message to print
#' @param functionName Name of parent function
#' @title Check that Object is Logical of Length 1
#' @return NULL invisibly if the object passes the check.  Otherewise, an exception is generated.
#' @author Mango Solutions
#' @examples 
#' \dontrun{
#'  checkSingleLogical(mtcars)
#'  checkSingleLogical(TRUE)
#'  checkSingleLogical(1)
#' }
#' @noRd

checkSingleLogical <- function(x, description = "x", 
  functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) )
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if(length(x) != 1 || !is.logical(x) || !is.vector(x))
    {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if(!is.vector(x))
        {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        }
        else
        {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a logical of length 1.  Value is:", y, sep = " "))
    }
    
    invisible(NULL)
    
}



#' Check if Object is Single Character Value
#'
#' Utility function that checks that an object x is character and has 1 element.  If it isn't, an 
#' exception is generated with a message that also displays the object contents. 
#' Character NA is single character, logical NA is not.
#'
#' @param x Object to check
#' @param description Description of the object, used in the message to print
#' @param functionName Name of parent function
#' @title Check that Object is Character of Length 1
#' @return NULL invisibly if the object passes the check.  Otherewise, an exception is generated.
#' @author Mango Solutions
#' @examples 
#' \dontrun{
#'  checkSingleCharacter(mtcars)
#'  checkSingleCharacter(NA)
#'  checkSingleCharacter('text')
#' }
#' @noRd

checkSingleCharacter <- function(x, description = "x", 
  functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) )
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if(length(x) != 1 || !is.character(x) || !is.vector(x))
    {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if(!is.vector(x))
        {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        }
        else
        {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a character of length 1.  Value is:", y, sep = " "))
    }
    
    invisible(NULL)
    
}


#' Check PeakTrough does not contain more than one TROUGHCODE or PEAKCODE
#'
#' Utility function that checks that PeakTrough does not contain more than one TROUGHCODE or PEAKCODE, otherwise an exception will be generated.
#' PEAKCODE is 2 and TROUGHCODE is 1. All other values should be 0, but values which are not PEAKCODE or THROUGHCODE are ignored.
#' @param PeakTrough Object to check
#' @param functionName Name of parent function
#' @title Check PeakTrough does not contain more than one TROUGHCODE or PEAKCODE
#' @return NULL invisibly
#' @author Mango Solutions
#' @keywords error
#' @examples 
#' \dontrun{
#'   checkPeakTrough(0:5)
#'   checkOrderedVector(c(1, 0, 2, 0, 1))
#' }
#' @noRd

checkPeakTrough <- function(PeakTrough, functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) )
{
    if( missing(PeakTrough) ) 
    {
        stop(paste("Error in ", functionName, ": PeakTrough is missing with no default", sep = ""))
    }
    
    if(!is.vector(PeakTrough))
    {
        stop(paste("Error in ", functionName, ": PeakTrough, is not a vector", sep = ""))
    }
    
    # PeakTroughNoNA : a vector equivalent to PeakTrough with NAs stripped out
    
    PeakTroughNoNA <- na.omit(PeakTrough)
    
    PEAKCODE <- 2
    TROUGHCODE <- 1
    
    PeakIndex <- which( PeakTrough == PEAKCODE )
    TroughIndex <- which( PeakTrough == TROUGHCODE )
    
    if( length(PeakIndex) > 1 || length(TroughIndex) > 1 )
    {
        stop(paste( "Error in ", functionName, ": PeakTrough is miscoded.  Actual value is ", paste(PeakTrough, collapse =  " ") , sep = "") )
    }
    
    invisible(NULL)
    
}


#' Remove Trailing Zero Rows
#'
#' Utility function that strips trailing zero rows from Conc.
#' This behaviour is required by \code{lambdaZStatistics} and \code{selectPoints}.
#' This function calls \code{cleanConcTime} first.
#' A length zero data frame may be returned.
#'
#' @param Conc Vector of Conc values to check
#' @param Time Vector of Time values to check
#' @param excPoints logical vector to maintain
#' @param usePoints logical vector to maintain
#' @param addT0 Single logical value declaring if T = 0 should be added if missing instead of generating an error (FALSE by default).
#' @param checkT0 Single logical value declaring if T = 0 should be checked
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @title Strip Zero Rows after Clast from Conc
#' @return data frame containing cleaned data with columns Conc and Time
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  stripTrailingZeros(Conc = c(0, 435, 363, 113, 47, 0, 0), Time = 0:6)
#' }
#' @noRd

stripTrailingZeros <- function(Conc, Time, usePoints = NULL, excPoints = FALSE, addT0 = FALSE, checkT0 = TRUE, Safe = TRUE)
{
    if (Safe) {
        
        # redundant checking
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "stripTrailingZeros")
        
        checkOrderedVector(Time, description = "Time", functionName = "stripTrailingZeros")
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Conc", functionName = "stripTrailingZeros")
        
        if (!is.null(usePoints)) { checkLogicalSameLength(excPoints, Conc, "usePoints", "Conc", functionName = "stripTrailingZeros") }
        
        checkSingleLogical(checkT0, description = "checkT0", functionName = "stripTrailingZeros")
        
        checkSingleLogical(addT0, description = "addT0", functionName = "stripTrailingZeros")
    }
    
    # check data for missing rows, and modify if addT0 is TRUE
    
    timeConc <- try(cleanConcTime(Conc, Time, usePoints = usePoints, excPoints = excPoints, addT0 = addT0, checkT0 = checkT0), silent = TRUE) 
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("call to cleanConcTime failed, message is: ", timeConc))
    }
    
    if (nrow(timeConc) < 1 ) { return(timeConc) }
    
    # identify clast and tlast, discarding anything beyond that
    
    cLastTLast <- ClastTlast(Conc = timeConc$Conc, Time = timeConc$Time)
    
    if (is.na(cLastTLast$tlast) || is.na(cLastTLast$clast) ) {
        
        warning("missing Tlast or Clast after call to ClastTlast in stripTrailingZeros")
        
        cLastTLast$index <- 0
    }
    
    # timeConc : data.frame with columns consisting of the last numPoints values of Time and Conc
    
    timeConc <- head(timeConc, n = cLastTLast$index)
    
    if ( nrow(timeConc) == 0 ) {
        
        warning("after removing trailing zeros there were no rows remaining in stripTrailingZeros")
    }
    
    return(timeConc)
	
}



#' Check Concentration-Time Data for missing values, negative values and missing T = 0
#'
#' Utility function that checks for: 
#'  \enumerate{
#'      \item missing rows, 
#'      \item negative values of Conc and Time,
#'      \item absence of row at T = 0.
#'  }
#' If addT0 is TRUE, \code{cleanConcTime} adds data.frame(Conc = 0, Time = 0) if T = 0 is missing, 
#' and removes missing rows and negative values of Conc and Time.
#' This is the expected behaviour of WinNonlin (p268 of User's Guide).
#' A data frame with no rows may be returned with warning.
#' If addT0 is FALSE, an error is generated if any of these data errors are present.
#' Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @param Conc Vector of Conc values to check.
#' @param Time Vector of Time values to check.
#' @param usePoints Optional Logical Vector of row selection values to check.
#' @param excPoints Optional Logical Vector of row selection values to check.
#' @param addT0 Single logical value declaring if T = 0 should be added if missing instead of generating an error (FALSE by default).
#' @param checkT0 Single logical value declaring if T = 0 should be checked
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @title Clean Conc and Time data
#' @return data frame containing cleaned data with columns Conc and Time
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  cleanConcTime(Conc = c(0, 435, 363, 113, 47), Time = 0:4)
#'  cleanConcTime(Conc = c(435, 363, 113, 47, NA), Time = 1:5)
#'  cleanConcTime(Conc = c(435, 363, 113, 47, NA), Time = 1:5, addT0 = TRUE, checkT0 = TRUE)
#' }
#' @noRd

cleanConcTime <- function(Conc, Time, usePoints = NULL, excPoints = FALSE, addT0 = FALSE, checkT0 = TRUE, Safe = TRUE)
{
    
    if (Safe) {
        
        # redundant checking
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "cleanConcTime")
        
        checkOrderedVector(Time, description = "Time", functionName = "cleanConcTime")
        
        checkSingleLogical(addT0, description = "Add Row at T = 0?", functionName = "cleanConcTime")
        
        checkSingleLogical(checkT0, description = "Check Row at T = 0?", functionName = "cleanConcTime")
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "cleanConcTime")
        
        cleanData <- data.frame(Conc = Conc, Time = Time, excPoints)
        
        # add row flags if used
        
        if (!is.null(usePoints)) { 
            
            checkLogicalSameLength(usePoints, Conc, "usePoints", "Conc", functionName = "cleanConcTime")
            
            cleanData$usePoints <- usePoints
        }
    }
    
    # if data missing values are premitted, remove without error
    
    if (addT0) {
        
        cleanData <- na.omit(cleanData)
    }
    
    # Check for NA and < 0, fix if addT0 is TRUE, otherwise generate error
    
    naFail(cleanData)
    
    if (any(cleanData$Time < 0)) {
        
        if (!addT0) { 
            
            stop("values of Time < 0 in cleanData")
        
        } else {
            
            warning("removing rows where Time < 0 in cleanData")
            
            cleanData <- cleanData[!cleanData$Time < 0, ]
        
        }
    }
    
    if (any(cleanData$Conc < 0))
    {
        if (!addT0) { 
            
            stop("values of Conc < 0 in cleanData") 
            
        } else {
            
            warning("removing rows where Conc < 0 in cleanData")
            
            cleanData <- cleanData[!cleanData$Conc < 0, ]
            
        }
    }

    
    if( nrow(cleanData) == 0 ) {
        
        warning("after removing NAs there were no rows remaining in cleanConcTime")
        
        return(cleanData)
    }

    if (checkT0) {
        
        # look for row where T = 0, add if addT0 is TRUE, otherwise error
        
        findT0 <- which(cleanData$Time == 0)
        
        if (any(is.na(findT0))) {
            
            stop("Error in cleanConcTime: while finding location of T = 0, the index was NA")
        }
        
        if (length(findT0) == 0)
        {
            if(!addT0) { 
                
                stop("Missing row where T = 0") 
                
            } else {
                
                warning("Adding Conc = 0, Time = 0 to data")
                
                t0df <- data.frame(Conc = 0, Time = 0)
                
                # if T0 is added no not include it in lambdaz calculation
                
                if (!is.null(usePoints)) { 
                    
                    t0df$usePoints <- FALSE
                }
        
                if (!is.null(excPoints)) { 
                    
                    t0df$excPoints <- TRUE
                }
                
                cleanData <- rbind(t0df, cleanData)
            }
        }
    }
    
    return(cleanData)
	
}



#' Logical Test that Number of Rows following Cmax is greater than minPoints
#'
#' Utility function that tests there are more than minPoints trailing rows following Cmax (TRUE or FALSE).
#' Test using usePoints if present ignoring Cmax restrictions but excluding excPoints.
#' Otherwise count rows and test that there are more remaining rows than minPoints following Cmax.
#' 
#' @param Conc Vector of Conc values to check.
#' @param Time Vector of Time values to check.
#' @param usePoints \code{NULL}, otherwise logical vector of points to use, ignoring Cmax restrictions
#' @param excPoints vector of \code{FALSE}, \code{TRUE} values are excluded
#' @param minPoints Single positive integer describing minimum number of points required following Cmax (default 3).
#' @param checkCmax Single logical value declaring whether to retain only rows after Cmax
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @title Test if number of rows is greater than or equal to minPoints, otherwise FALSE
#' @return Logical
#' @author Mango Solutions
#' @examples 
#'  testTrailPoints(Conc = c(435, 363, 113, 47,0), Time = 0:4)
#'  testTrailPoints(Conc = c(435, 363, 113, 47,0), Time = 0:4)
#'  testTrailPoints(Conc = c(435, 363, 113, 0, 0),  Time = 0:4)
#' @noRd

testTrailPoints <- function(Conc, Time, usePoints = NULL, excPoints = FALSE, minPoints = 3, checkCmax = TRUE, Safe = TRUE)
{

    if (Safe) {
        
        checkNumericSameLength(Time, Conc, "Time", "Conc", functionName = "testTrailPoints")
        
        checkOrderedVector(Time, description = "Time", functionName = "testTrailPoints")
        
        if (!is.null(usePoints)) {
            
            checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "testTrailPoints")
        }
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "testTrailPoints")
        
        checkSingleNumeric(minPoints, description = "minPoints", functionName = "testTrailPoints")
        
        checkSingleLogical(checkCmax, description = "checkCmax", functionName = "testTrailPoints")
    }
    
    # clean and remove trailing zeros
    
    timeConc <- try(cleanConcTime(Conc = Conc, Time = Time, usePoints = usePoints, excPoints = excPoints, checkT0 = FALSE, Safe = Safe), silent = TRUE)
    
    if (is(timeConc, "try-error")) {
        
        stop(paste("call to cleanConcTime failed, message is: ", timeConc))
    }
    
    # if all rows are gone, there are insufficient rows for lambdaz calculation!
    
    if (identical(nrow(timeConc), as.integer(0))) { return(FALSE) }
    
    # flag rows before & including Cmax
    
    CmaxTmax <- CmaxTmax(timeConc$Conc, timeConc$Time)
    
	CmaxIndex <- CmaxTmax$index
    
    if (checkCmax) {
        
        timeConc$afterCmax <- c(rep(FALSE, times = CmaxIndex), rep(TRUE, times = nrow(timeConc) - CmaxIndex))
        
    } else {
        
        timeConc$afterCmax <- rep(TRUE, times = nrow(timeConc))
    }
    
    # test using usePoints if present, ignoring Cmax restrictions but excluding excPoints
    
    if (!is.null(usePoints)) { 
        
        selPoints <- nrow(timeConc[timeConc$usePoints & !timeConc$excPoints, ])
        
    } else {
        
        # otherwise count rows and test that there are more remaining rows than minPoints following Cmax
        
        selPoints <- nrow(timeConc[timeConc$afterCmax & !timeConc$excPoints, ])
    }
    
    # selPoints may be NA
    
    test <- selPoints >= minPoints
    
    # remove NAs, NULL, length(test) != 1, etc.
    
    if( !isTRUE(test) ) { test <- FALSE }
    
    return(test)
}



#' Check object contains no missing values
#'
#' Utility function that fails with a useful error message if the object contains missing values
#'
#' @param object An object to be checked
#' @param description Description of the object, used in the message to print
#' @param functionName Name of parent function
#' @title Check object contains no missing values
#' @return Object or error
#' @author Mango Solutions
#' @examples
#'      theoData <- head(Theoph)
#'      naFail(theoData, description = "head(Theoph)", functionName = "fun")
#' \dontrun{
#'      theoData[2, 3] <- NA
#'      naFail(theoData, description = "head(Theoph)", functionName = "fun")
#' }
#' @noRd

naFail <- function(object, description = "object", functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) 
{
    ok <- complete.cases(object)
    if( all(ok) ) {
        return(object) }
    else stop("Error in ", functionName, ": missing values in ", description)
}



#' Check Logical Vector is the Same Length as another vector
#'
#' Utility function to check whether a vector is logical and of the same length as a reference vector.
#' Stops with an error message if either object is not a logical vector or if
#' the vectors are of different lengths.  NA logicals are forbidden.
#'
#' @title Check For logical Vectors Same Length
#' @param x first object, should be a logical vector
#' @param y second object, should be a vector
#' @param xDescription character string giving a descriptive name for the
#'   first vector.  Used in error messages.  Defaults to "x".
#' @param yDescription character string giving a descriptive name for the
#'   second vector.  Used in error messages.  Defaults to "y"
#' @param functionName character string giving the name of the function
#'   calling this function.  Used in error messages.  If not supplied, the default determines
#'   which function invoked this one 
#' @keywords error
#' @examples 
#' # standard use 
#' checkLogicalSameLength(Theoph$conc > 5, Theoph$Time)
#' # generate error 
#' \dontrun{
#'  checkLogicalSameLength(c(TRUE, FALSE), 1:3)
#' }
#' @return returns NULL invisibly if no error was generated.
#' @noRd

checkLogicalSameLength <- function(x, y, 
        xDescription = "x", yDescription = "y",
        functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) 
{
    if( missing(x) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        xDescription, " is missing with no default", sep = ""))
    }
    
    if( missing(y) ) 
    {
        stop(paste("Error in ", functionName, ": ",
                        yDescription, " is missing with no default", sep = ""))
    }
    
    if(!is.logical(x) || !is.vector(x))
    {
        stop(paste("Error in ", functionName, ": ",
                        xDescription, " is not a logical vector", sep = ""))
    }
    
    if( !is.vector(y) )
    {
        stop(paste("Error in ", functionName, ": ",
                        yDescription, " is not a vector", sep = ""))
    }
    
    if( !identical(length(x), length(y)) )
    {
        stop(paste("Error in ", functionName, ": ",
                        " lengths of ", xDescription, " and ", yDescription, 
                        " do not match", sep = ""))
    }
    
    if (any(is.na(x)))
    {
        stop(paste("Error in ", functionName, ": ", " missing values in ", xDescription, sep = ""))
    }
    
    invisible(NULL)
    
}



#' Check structure of lambdaZStatistics output
#'
#' Utility function that fails with a useful error message if the object is not of the expected structure.
#' lambdaZStats should be a length 9 vector with names:
#' \enumerate{
#'      \item Lambdaz
#'      \item intercept
#'      \item r2
#'      \item adjr2
#'      \item rhoXY
#'      \item tPhaseHalfLife
#'      \item LambdazLower
#'      \item LambdazUpper
#'      \item numPoints
#' }
#'
#' @param lambdaZStats An unlisted lambdaZStats output object to be checked
#' @param functionName Name of parent function
#' @title Check structure of lambdaZStatistics output
#' @return Object or error
#' @author Mango Solutions
#' @noRd

checkLambdaZStats <- function(lambdaZStats, functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {

    if (missing(lambdaZStats)) { stop(paste("Error in", functionName, " lambdaZStats is missing", collapse = ":")) }
    
    checkNumeric <- is.numeric(lambdaZStats) && identical(length(lambdaZStats), as.integer(9))
    
    if (!checkNumeric) { stop(paste("Error in ", functionName, "lambdaZStats has unexpected structure, length 9 numeric vector expected", collapse = ", ")) }
    
    lzNames <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
    
    checkNames <- setequal(names(lambdaZStats), lzNames)
    
    if (!checkNames) { stop(paste("Error in ", functionName, "lambdaZStats has unexpected structure, names should be", paste(lzNames, collapse = ", "))) }

    return(invisible(NULL))
}
