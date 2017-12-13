# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 24/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests predictConc function from predictConc.R

test.predictConc <- function()
{
    if (!exists("predConc", mode = "function")) { predictConc <- MangoNca:::predictConc }
    
    time <- 0:10
    conc <- c(0, 10:1)
    
    # TEST 1 - 3 : predTime is an element of time - normal and boundary cases
    
    test1 <- predictConc(Conc = conc, Time = time, predTime = 10, numPoints = 3, addT0 = TRUE)
    checkEquals(test1, 1, msg = " || TEST 1 : predTime is the final element of Time" )
    
    test2 <- predictConc(Conc = conc, Time =  time, predTime = 2, numPoints = 3, addT0 = TRUE)
    checkEquals(test2, 9, msg = " || TEST 2 : predTime is the 3rd element of Time")
    
    test3 <- predictConc(Conc = conc, Time = time, predTime = 0, numPoints = 3)
    checkEquals(test3, 0, msg = " || TEST 3 : predTime is the 1st element of Time ")
    
    # TEST 4-6 : predTime is not an element of Time, but is inside the lower and upper limits
    
    test4 <- predictConc(Conc = conc, Time = time, predTime = 1.75, numPoints = 3)
    checkEquals(test4, 9.25, msg = " || TEST 4 : predTime lies between first two elements of Time")
    
    test5 <- predictConc(Conc = conc, Time = time, predTime = 9.25, numPoints = 3)
    checkEquals(test5, 1.75, msg = " || TEST 5 : predTime lies between last 2 elements of Time ")
    
    
    test6 <- predictConc(Conc = conc, Time = time, predTime = 5.2, numPoints = 3)
    checkEquals(test6, 5.8, msg = " || TEST 6 : predTime lies between middle elements of Time" )
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- predictConc(Conc = Theoph2$conc, Time = Theoph2$Time, predTime = 18, numPoints = 3)
    checkEquals(test7,  1.980731707, tol = 1e-7, msg = " || TEST 7 Theoph data set, predTime between last 2 elements of Time")
    
    # TEST 8 : NA should be returned if predTime < earliest time
    
    test8 <- predictConc(Conc =  4:1,Time = 0:3, predTime = -1, numPoints = 3, addT0 = FALSE)
    checkTrue(is.na(test8), msg = " || TEST 8 : predTime < Time[1]")
    
    # TEST 9 : check for predTime > tlast
    
    test9 <- predictConc(Conc = Theoph2$conc, Time = Theoph2$Time, predTime = 26, numPoints = 5)
    
    checkEquals(test9, 0.7573122121, msg = " || TEST 9 : predTime > last time, checking extrapolation" )
    
    # TEST 10 : NA should be returned if predTime is numeric NA
    
    test10 <- predictConc(Conc = c(1, 6, 5, 4, 2, 1), Time = 0:5, predTime = as.numeric(NA), numPoints = 3, addT0 = FALSE)
    checkTrue(is.na(test10), msg = " || TEST 10 : NA should be returned if predTime is numeric NA")    
    
    # TEST 11: Test for lambdaZStats
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    lzVec <- c(
                Lambdaz = 1,
                intercept = exp(2),
                r2 = 1,
                adjr2 = 1,
                rhoXY = -1,
                tPhaseHalfLife = log(2),
                LambdazLower = 2,
                LambdazUpper = 2.75,
                numPoints = 4)
    
    test11 <- predictConc(Conc = c(0, conc), Time = c(0, time), predTime = Inf, lambdaZStats = lzVec)
    checkEquals(test11, 0, msg = " || TEST 11: Test for lambdaZStats")
    
    # TEST 12: Test for excPoints
    
    # TEST 13: Test for usePoints
    
    
}

test.predictConc_errorHandling <- function()
{
    # for compatibility with Coverage functionality
    if (!exists("predConc", mode = "function")) { predictConc <- MangoNca:::predictConc }
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : missing numPoints
    
    Theoph2 <- Theoph[Theoph$Subject == 2, ]
    
    test1 <- try( predictConc(Conc = Theoph2$conc, Time = Theoph2$Time, predTime = 26), silent = TRUE )
    checkTrue( isErrorWithMessage(test1, "Error in checkSingleNumeric(numPoints, description = \"numPoints\", functionName = \"predictConc\") : \n  Error in  predictConc :  numPoints is not a numeric of length 1.  Value is: \n NULL\n"),
            msg = " || TEST 1 : missing numPoints")
   
    # TEST 2 : predTime is not a length 1 
    
    test2 <- try( predictConc(Conc = 3:1, Time = 0:2, predTime = c(1, 2), numPoints = 3), silent = TRUE )
    checkTrue( isErrorWithMessage(test2, "Error in checkSingleNumeric(predTime, description = \"predTime\", functionName = \"predictConc\") : \n  Error in  predictConc :  predTime is not a numeric of length 1.  Value is: 1 2\n"),
            msg = "  || TEST 2 : predTime is not of length 1")
    
    # TEST 3 : predTime is not numeric
    
    test3 <- try( predictConc(Conc = 3:1, Time = 0:2, predTime = "1", numPoints = 3), silent = TRUE )
    checkTrue(isErrorWithMessage(test3, "Error in checkSingleNumeric(predTime, description = \"predTime\", functionName = \"predictConc\") : \n  Error in  predictConc :  predTime is not a numeric of length 1.  Value is: 1\n"),
                msg = " || TEST 3 : predTime is not numeric ")
                
    # TEST 4 : predTime is missing
    
    test4 <- try(predictConc(Conc = c(1, 6, 5, 4, 2, 1), Time = 0:5, numPoints = 3), silent = TRUE )
    checkTrue( isErrorWithMessage(test4, "Error in checkSingleNumeric(predTime, description = \"predTime\", functionName = \"predictConc\") : \n  Error in predictConc: predTime is missing with no default\n"),
            msg = "  || TEST 4 : predTime is missing")
            
    # TEST 5 : predTime is not numeric NA
    
    test5 <- try(predictConc(Conc = c(1, 6, 5, 4, 2, 1), Time = 0:5, predTime = NA, numPoints = 3), silent = TRUE )
    checkTrue( isErrorWithMessage(test5, "Error in checkSingleNumeric(predTime, description = \"predTime\", functionName = \"predictConc\") : \n  Error in  predictConc :  predTime is not a numeric of length 1.  Value is: NA\n"),
            msg = "  || TEST 5 : predTime is not numeric NA")
    
}
