# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Check Two Numeric Vectors have the Same Length
#'
#' Utility function to check whether two vectors are numeric and of the same length.
#' Stops with an error message if either object is not a numeric vector or if
#' the vectors are of different lengths.  
#' Single pair of numeric NAs will be treated as having the same length.
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
#' @note If x is not a numeric vector, the error message will be of the form 
#'  "Error in [functionName]: [xDescription] is not a numeric vector",
#'  and similarly if y is not numeric (replacing yDescription where appropriate). 
#'  If the two vectors are not of the same length,
#'  the error will be "Error in [functionName]: 
#'  lengths of [xDescription] and [yDescription] do not match"   
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
        functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {
    if (missing(x) ) {
        stop(paste("Error in ", functionName, ": ",
                xDescription, " is missing with no default", sep = ""))
    }
    
    if (missing(y) ) {
        stop(paste("Error in ", functionName, ": ",
                yDescription, " is missing with no default", sep = ""))
    }
    
    # Note a vector of NA values is a logical by default, hence the more complex check
    
    if (!((is.numeric(x) || (is.logical(x) && all(is.na(x)))) && is.vector(x)) ) {
        stop(paste("Error in ", functionName, ": ",
                xDescription, " is not a numeric vector", sep = ""))
    }
    
    if (!((is.numeric(y) || (is.logical(y) && all(is.na(y)))) && is.vector(y)) ) {
        stop(paste("Error in ", functionName, ": ",
                yDescription, " is not a numeric vector", sep = ""))
    }
    
    if (length(x) != length(y) ) {
        stop(paste("Error in ", functionName, ": ",
                " lengths of ", xDescription, " and ", yDescription, 
                " do not match", sep = ""))
    }
    
    return(invisible(NULL))
    
}


#' Check if Vector is Ordered
#'
#' Utility function that checks that an object is a vector whose non-missing elements are sorted.  
#' If the object is not a vector, or is a vector but is elements are not sorted in ascending order, 
#' an exception will be generated (R's ordering of vectors via \code{order} is used).
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

checkOrderedVector <- function(x, description = "x", 
    functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) ) {
    
    if (missing(x)) {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if (!is.vector(x)) {
        stop(paste("Error in ", functionName, ": ",description, " is not a vector", sep=""))
    }
    
    # xNoNA : a vector equivalent to x with NAs stripped out
    
    xNoNA <- na.omit(x)
    
    if ( !identical(order(xNoNA), seq_along(xNoNA)) ) {
        stop(paste( "Error in ", functionName, ": ", description, 
                    " is not ordered.  Actual value is ", paste(x, collapse =  " ") , sep = "") )
    }
    
    return(invisible(NULL))
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
    functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {
    if (missing(x)) {
        stop(paste("Error in ", functionName, ": ",
                description, " is missing with no default", sep = ""))
    }
    
    if (length(x) != 1 || !is.numeric(x) || !is.vector(x)) {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if(!is.vector(x)) {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        } else {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a numeric of length 1.  Value is:", y, sep = " "))
    }
    
    return(invisible(NULL))
    
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
  functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) ) {
    if (missing(x)) {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if (length(x) != 1 || !is.logical(x) || !is.vector(x)) {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if (!is.vector(x)) {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        } else {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a logical of length 1.  Value is:", y, sep = " "))
    }
    
    return(invisible(NULL))
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
  functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]]) ) {
    if (missing(x)) {
        stop(paste("Error in ", functionName, ": ",
                        description, " is missing with no default", sep = ""))
    }
    
    if (length(x) != 1 || !is.character(x) || !is.vector(x)) {
        # if the object is not a vector at all, we obtain a string  
        # representation of it by capturing the output of the "show" function 
        # (since this can always be done, whereas coercion to strings cannot)
        
        if (!is.vector(x)) {
            y <- paste("\n", capture.output(show(x)), collapse = "\n")    
        } else {
            y <- paste(x, collapse = " ")    
        }
        
        stop(paste("Error in ", functionName, ": ", description, 
            "is not a character of length 1.  Value is:", y, sep = " "))
    }
    
    return(invisible(NULL))
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

checkPeakTrough <- function(PeakTrough, 
    functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {
    
    if (missing(PeakTrough)) {
        stop(paste("Error in ", functionName, ": PeakTrough is missing with no default", sep = ""))
    }
    
    if (!is.vector(PeakTrough)) {
        stop(paste("Error in ", functionName, ": PeakTrough, is not a vector", sep = ""))
    }
    
    # PeakTroughNoNA : a vector equivalent to PeakTrough with NAs stripped out
    
    PeakTroughNoNA <- na.omit(PeakTrough)
    
    PEAKCODE <- 2
    TROUGHCODE <- 1
    
    PeakIndex <- which( PeakTrough == PEAKCODE )
    TroughIndex <- which( PeakTrough == TROUGHCODE )
    
    if (length(PeakIndex) > 1 || length(TroughIndex) > 1) {
        stop(paste( "Error in ", functionName, ": PeakTrough is miscoded.  Actual value is ", paste(PeakTrough, collapse =  " ") , sep = "") )
    }
    
    return(invisible(NULL))
}


#' @title Strip Zero Rows after Clast from conc
#'
#' Utility function that strips trailing zero rows from conc.
#' This behaviour is required by \code{lambdaZStatistics} and \code{selectPoints}.
#' This function calls \code{cleanconctime} first.
#' A length zero data frame may be returned.
#'
#' @inheritParams AUCInfObs
#' @param excpoints logical vector to maintain
#' @param usepoints logical vector to maintain
#' @param addt0 Single logical value declaring if T = 0 should be added if missing instead of generating an error (FALSE by default).
#' @param checkT0 Single logical value declaring if T = 0 should be checked
#' @return data frame containing cleaned data with columns conc and time
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  stripTrailingZeros(conc = c(0, 435, 363, 113, 47, 0, 0), time = 0:6)
#' }
#' @noRd

stripTrailingZeros <- function(conc, time, usepoints = NULL, excpoints = FALSE, 
    addt0 = FALSE, checkT0 = TRUE) {
    
    checkNumericSameLength(time, conc, "time", "conc", 
        functionName = "stripTrailingZeros")
    
    checkOrderedVector(time, description = "time", 
        functionName = "stripTrailingZeros")
    
    if (identical(excpoints, FALSE)) { 
        excpoints <- rep(FALSE, times = length(time)) }
    
    checkLogicalSameLength(excpoints, conc, "excpoints", "conc", 
        functionName = "stripTrailingZeros")
    
    if (!is.null(usepoints)) { 
        checkLogicalSameLength(excpoints, conc, "usepoints", "conc", 
            functionName = "stripTrailingZeros") }
    
    checkSingleLogical(checkT0, description = "checkT0", 
        functionName = "stripTrailingZeros")
    
    checkSingleLogical(addt0, description = "addt0", 
        functionName = "stripTrailingZeros")
    
    # check data for missing rows, and modify if addt0 is TRUE
    
    timeconc <- try(cleanconctime(conc = conc, time = time, 
            usepoints = usepoints, excpoints = excpoints, 
            addt0 = addt0, checkT0 = checkT0), 
        silent = TRUE) 
    
    if (is(timeconc, "try-error")) {
        
        stop(paste("call to cleanconctime failed, message is: ", timeconc))
    }
    
    if (nrow(timeconc) < 1 ) { return(timeconc) }
    
    # identify clast and tlast, discarding anything beyond that
    
    cLastTLast <- ClastTlast(conc = timeconc$conc, time = timeconc$time)
    
    if (is.na(cLastTLast$tlast) || is.na(cLastTLast$clast) ) {
        
        warning("missing Tlast or Clast after call to ClastTlast in stripTrailingZeros")
        
        cLastTLast$index <- 0
    }
    
    # timeconc : data.frame with columns consisting of the last lamznpt values of time and conc
    
    timeconc <- head(timeconc, n = cLastTLast$index)
    
    if ( nrow(timeconc) == 0 ) {
        
        warning("after removing trailing zeros there were no rows remaining in stripTrailingZeros")
    }
    
    return(timeconc)
	
}



#' Check concentration-time Data for missing values, negative values and missing T = 0
#'
#' Utility function that checks for: 
#'  \enumerate{
#'      \item missing rows, 
#'      \item negative values of conc and time,
#'      \item absence of row at T = 0.
#'  }
#' If addt0 is TRUE, \code{cleanconctime} adds data.frame(conc = 0, time = 0) 
#'     if T = 0 is missing, 
#' and removes missing rows and negative values of conc and time.
#' This is the expected behaviour of WinNonlin (p268 of User's Guide).
#' A data frame with no rows may be returned with warning.
#' If addt0 is FALSE, an error is generated if any of these data errors are present.
#' Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @inheritParams AUCInfObs
#' @param usepoints Optional Logical Vector of row selection values to check.
#' @param excpoints Optional Logical Vector of row selection values to check.
#' @param addt0 Single logical value declaring if T = 0 should be added 
#'     if missing instead of generating an error (FALSE by default).
#' @param checkT0 Single logical value declaring if T = 0 should be checked
#' @title Clean conc and time data
#' @return data frame containing cleaned data with columns conc and time
#' @author Mango Solutions
#' @examples
#' \dontrun{
#'  cleanconctime(conc = c(0, 435, 363, 113, 47), time = 0:4)
#'  cleanconctime(conc = c(435, 363, 113, 47, NA), time = 1:5)
#'  cleanconctime(conc = c(435, 363, 113, 47, NA), time = 1:5, 
#'      addt0 = TRUE, checkT0 = TRUE)
#' }
#' @noRd

cleanconctime <- function(conc, time, usepoints = NULL, excpoints = FALSE, 
    addt0 = FALSE, checkT0 = TRUE) {
    
    checkNumericSameLength(time, conc, "time", "conc", functionName = "cleanconctime")
    
    checkOrderedVector(time, description = "time", functionName = "cleanconctime")
    
    checkSingleLogical(addt0, description = "Add Row at T = 0?", functionName = "cleanconctime")
    
    checkSingleLogical(checkT0, description = "Check Row at T = 0?", functionName = "cleanconctime")
    
    if (identical(excpoints, FALSE)) { excpoints <- rep(FALSE, times = length(time)) }
    
    checkLogicalSameLength(excpoints, conc, "excpoints", "concentration", "cleanconctime")
    
    if (!is.null(usepoints)) {
        
        checkLogicalSameLength(usepoints, conc, "usepoints", "conc", functionName = "cleanconctime")
    }
    
    # clean as data frame
    cleanData <- data.frame(conc = conc, time = time, excpoints = excpoints)
    
    # add row flags if used
    
    if (!is.null(usepoints)) {
        
        cleanData$usepoints <- usepoints
    }
    
    
    # if data missing values are permitted, remove without error
    
    if (addt0) {
        
        cleanData <- na.omit(cleanData)
    }
    
    # Check for NA and < 0, fix if addt0 is TRUE, otherwise generate error
    
    naFail(cleanData)
    
    if (any(cleanData$time < 0)) {
        
        if (!addt0) { 
            
            stop("values of time < 0 in cleanData")
        
        } else {
            
            warning("removing rows where time < 0 in cleanData")
            
            cleanData <- cleanData[!cleanData$time < 0, ]
        
        }
    }
    
    if (any(cleanData$conc < 0)) {
        if (!addt0) {
            
            stop("values of conc < 0 in cleanData") 
            
        } else {
            
            warning("removing rows where conc < 0 in cleanData")
            
            cleanData <- cleanData[!cleanData$conc < 0, ]
            
        }
    }

    
    if (nrow(cleanData) == 0) {
        
        warning("after removing NAs there were no rows remaining in cleanconctime")
        
        return(cleanData)
    }

    if (checkT0) {
        
        # look for row where T = 0, add if addt0 is TRUE, otherwise error
        
        findT0 <- which(cleanData$time == 0)
        
        if (any(is.na(findT0))) {
            
            stop("Error in cleanconctime: while finding location of T = 0, the index was NA")
        }
        
        if (length(findT0) == 0) {
            if (!addt0) {
                
                stop("Missing row where T = 0") 
                
            } else {
                
                warning("Adding conc = 0, time = 0 to data")
                
                t0df <- data.frame(conc = 0, time = 0)
                
                # if T0 is added no not include it in lambdaz calculation
                
                if (!is.null(usepoints)) {
                    
                    t0df$usepoints <- FALSE
                }
        
                if (!is.null(excpoints)) {
                    
                    t0df$excpoints <- TRUE
                }
                
                cleanData <- rbind(t0df, cleanData)
            }
        }
    }
    
    return(cleanData)
	
}



#' Logical Test that Number of Rows following Cmax is greater than minpoints
#'
#' Utility function that tests there are more than minpoints trailing rows following Cmax (TRUE or FALSE).
#' Test using usepoints if present ignoring Cmax restrictions but excluding excpoints.
#' Otherwise count rows and test that there are more remaining rows than minpoints following Cmax.
#' 
#' @inheritParams AUCInfObs
#' @param usepoints \code{NULL}, otherwise logical vector of points to use, ignoring Cmax restrictions
#' @param excpoints vector of \code{FALSE}, \code{TRUE} values are excluded
#' @param minpoints Single positive integer describing minimum number of points required following Cmax (default 3).
#' @param checkcmax Single logical value declaring whether to retain only rows after Cmax
#' @title Test if number of rows is greater than or equal to minpoints, otherwise FALSE
#' @return Logical
#' @author Mango Solutions
#' @examples 
#'  testTrailPoints(conc = c(435, 363, 113, 47,0), time = 0:4)
#'  testTrailPoints(conc = c(435, 363, 113, 47,0), time = 0:4)
#'  testTrailPoints(conc = c(435, 363, 113, 0, 0),  time = 0:4)
#' @noRd

testTrailPoints <- function(conc, time, usepoints = NULL, excpoints = FALSE, 
    minpoints = 3, checkcmax = TRUE) {

    checkNumericSameLength(time, conc, "time", "conc", functionName = "testTrailPoints")
    
    checkOrderedVector(time, description = "time", functionName = "testTrailPoints")
    
    if (!is.null(usepoints)) {
        
        checkLogicalSameLength(usepoints, conc, "usepoints", "concentration", "testTrailPoints")
    }
    
    if (identical(excpoints, FALSE)) { excpoints <- rep(FALSE, times = length(time)) }
    
    checkLogicalSameLength(excpoints, conc, "excpoints", "concentration", "testTrailPoints")
    
    checkSingleNumeric(minpoints, description = "minpoints", functionName = "testTrailPoints")
    
    checkSingleLogical(checkcmax, description = "checkcmax", functionName = "testTrailPoints")
    
    # clean and remove trailing zeros
    
    timeconc <- try(cleanconctime(conc = conc, time = time, 
            usepoints = usepoints, excpoints = excpoints, 
            checkT0 = FALSE), 
        silent = TRUE)
    
    if (is(timeconc, "try-error")) {
        
        stop(paste("call to cleanconctime failed, message is: ", timeconc))
    }
    
    # if all rows are gone, there are insufficient rows for lambdaz calculation!
    
    if (identical(nrow(timeconc), as.integer(0))) { return(FALSE) }
    
    # flag rows before & including Cmax
    
    CmaxTmax <- CmaxTmax(timeconc$conc, timeconc$time)
    
	CmaxIndex <- CmaxTmax$index
    
    if (checkcmax) {
        
        timeconc$afterCmax <- c(rep(FALSE, times = CmaxIndex), 
            rep(TRUE, times = nrow(timeconc) - CmaxIndex))
        
    } else {
        
        timeconc$afterCmax <- rep(TRUE, times = nrow(timeconc))
    }
    
    # test using usepoints if present, ignoring Cmax restrictions 
    # but excluding excpoints
    
    if (!is.null(usepoints)) { 
        
        selPoints <- nrow(timeconc[timeconc$usepoints & !timeconc$excpoints, ])
        
    } else {
        
        # otherwise count rows and test that there are more 
        # remaining rows than minpoints following Cmax
        
        selPoints <- nrow(timeconc[timeconc$afterCmax & !timeconc$excpoints, ])
    }
    
    # selPoints may be NA
    
    test <- selPoints >= minpoints
    
    # remove NAs, NULL, length(test) != 1, etc.
    
    if( !isTRUE(test) ) { test <- FALSE }
    
    return(test)
}



#' Check object contains no missing values
#'
#' Utility function that fails with a useful error message 
#'  if the object contains missing values
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

naFail <- function(object, description = "object", 
    functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {
    ok <- complete.cases(object)
    if (all(ok)) {
        return(object)
    } else { 
        stop("Error in ", functionName, ": missing values in ", description) }
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
        functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {
    
    if (missing(x)) {
        stop(paste("Error in ", functionName, ": ",
                xDescription, " is missing with no default", sep = ""))
    }
    
    if (missing(y)) {
        stop(paste("Error in ", functionName, ": ",
                yDescription, " is missing with no default", sep = ""))
    }
    
    if (!is.logical(x) || !is.vector(x)) {
        stop(paste("Error in ", functionName, ": ",
                xDescription, " is not a logical vector", sep = ""))
    }
    
    if (!is.vector(y)) {
        stop(paste("Error in ", functionName, ": ",
                yDescription, " is not a vector", sep = ""))
    }
    
    if (!identical(length(x), length(y))) {
        stop(paste("Error in ", functionName, ": ",
                " lengths of ", xDescription, " and ", yDescription, 
                " do not match", sep = ""))
    }
    
    if (any(is.na(x))) {
        stop(paste("Error in ", functionName, ": ", " missing values in ", xDescription, sep = ""))
    }
    
    return(invisible(NULL))
}



#' Check structure of lambdaZStatistics output
#'
#' Utility function that fails with a useful error message if the object is not of the expected structure.
#' lambdaZStats should be a length 9 vector with names:
#' \enumerate{
#'      \item Lambdaz
#'      \item intercept
#'      \item R2
#'      \item R2ADJ
#'      \item CORRXY
#'      \item LAMZHL
#'      \item LAMZLL
#'      \item LAMZUL
#'      \item lamznpt
#' }
#'
#' @param lambdaZStats An unlisted lambdaZStats output object to be checked
#' @param functionName Name of parent function
#' @title Check structure of lambdaZStatistics output
#' @return Object or error
#' @author Mango Solutions
#' @noRd

checkLambdaZStats <- function(lambdaZStats, 
    functionName = as.character(sys.calls()[[max(1, sys.parent())]][[1]])) {

    if (missing(lambdaZStats)) { 
        stop(paste("Error in", functionName, " lambdaZStats is missing", collapse = ":")) }
    
    checkNumeric <- is.numeric(lambdaZStats) && identical(length(lambdaZStats), as.integer(9))
    
    if (!checkNumeric) { 
        stop(paste("Error in ", functionName, 
                "lambdaZStats has unexpected structure, length 9 numeric vector expected", 
                collapse = ", ")) }
    
    lzNames <- c("LAMZ", "intercept", "R2", "R2ADJ", "CORRXY", "LAMZHL", 
        "LAMZLL", "LAMZUL", "lamznpt")
    
    checkNames <- setequal(names(lambdaZStats), lzNames)
    
    if (!checkNames) { 
        stop(paste("Error in ", functionName, 
                "lambdaZStats has unexpected structure, names should be", 
                paste(lzNames, collapse = ", "))) }

    return(invisible(NULL))
}
