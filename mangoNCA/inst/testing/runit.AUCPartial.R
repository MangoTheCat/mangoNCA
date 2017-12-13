# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 03/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests AUCPartial function from AUCPartial.R

test.AUCPartial <- function()
{
    if (!exists("AUCPartial", mode = "function")) { AUCPartial <- MangoNca:::AUCPartial }
    
    time <- 0:10
    conc <- c(0, 10:1)
    
    # TEST 1 - 3 : endTime is an element of time - normal and boundary cases
    
    test1 <- AUCPartial(Conc = conc, Time = time, endTime = 10, numPoints = 3, addT0 = TRUE)
    checkEquals(test1, AUCLast(Time = time, Conc = conc ), msg = " || TEST 1 : endTime is the final element of Time" )
    
    test2 <- AUCPartial(Conc = conc, Time =  time, endTime = 2, numPoints = 3, addT0 = TRUE)
    checkEquals(test2, AUCLast(Time = 0:2, Conc = c(0, 10:9)), msg = " || TEST 2 : endTime is the second element of Time")
    
    test3 <- AUCPartial(Conc = conc, Time = time, endTime = 3, numPoints = 3)
    checkEquals(test3, AUCLast(Time = 0:3, Conc = c(0, 10:8)), msg = " || TEST 3 : endTime is the third element of Time ")
    
    # TEST 4-6 : endTime is not an element of Time, but is inside the lower and upper limits
    
    test4 <- AUCPartial(Conc = conc, Time = time, endTime = 1.75, numPoints = 3)
    checkEquals(test4, AUCLast(Conc = c(0, 10, 9.25), Time = c(0, 1, 1.75)), msg = " || TEST 4 : endTime lies between first two elements of Time")
    
    test5 <- AUCPartial(Conc = conc, Time = time, endTime = 9.25, numPoints = 3)
    checkEquals(test5, AUCLast(Conc = c(0, 10:2, 1.75), Time = c(0:9, 9.25)), msg = " || TEST 5 : endTime lies between last 2 elements of Time ")
    
    
    test6 <- AUCPartial(Conc = conc, Time = time, endTime = 5.2, numPoints = 3)
    checkEquals(test6, AUCLast(Conc = c(0, 10:6, 5.8), Time = c(0:5, 5.2)), msg = " || TEST 6 : endTime lies between middle elements of Time" )
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- AUCPartial(Conc = Theoph2$conc, Time = Theoph2$Time, endTime = 18, numPoints = 3)
    checkEquals(test7, AUCLast(Time = Theoph2$Time[1:10], Conc = Theoph2$conc[1:10]) + 14.97220, tol = 1e-7, msg = " || TEST 7 Theoph data set, endTime between last 2 elements of Time")
    
    # TEST 8 : NA should be returned if endTime < earliest time
    
    test8 <- AUCPartial(Conc =  4:1,Time = 0:3, endTime = -1, numPoints = 3, addT0 = FALSE)
    checkTrue(is.na(test8), msg = " || TEST 8 : endTime < Time[1]")
    
    # TEST 9 : check for tau > tlast
    
    test9 <- AUCPartial(Conc = Theoph2$conc, Time = Theoph2$Time, endTime = 26, numPoints = 5)
    
    checkEquals(test9, 92.93229975, msg = " || TEST 9 : endTime > last time, checking extrapolation" )
    
    # TEST 10 : endTime is T = 0
    
    test10 <- AUCPartial(Conc = conc, Time = time, endTime = 0, numPoints = 3, addT0 = TRUE)
    checkEquals(test10, 0, msg = " || TEST 10 : endTime is the first element of Time" )
    
}

test.AUCPartial_errorHandling <- function()
{
    if (!exists("AUCPartial", mode = "function")) { AUCPartial <- MangoNca:::AUCPartial }
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : missing numPoints
    
    Theoph2 <- Theoph[Theoph$Subject == 2, ]
    
    test1 <- try( AUCPartial(Conc = Theoph2$conc, Time = Theoph2$Time, endTime = 26), silent = TRUE )
    checkTrue( isErrorWithMessage(test1, "Error in checkSingleNumeric(numPoints, description = \"numPoints\", functionName = \"AUCPartial\") : \n  Error in  AUCPartial :  numPoints is not a numeric of length 1.  Value is: \n NULL\n"),
            msg = " || TEST 1 : missing numPoints")
   
    # TEST 2 : endTime is not a length 1 
    
    test2 <- try( AUCPartial(Conc = 3:1, Time = 0:2, endTime = c(1, 2), numPoints = 3), silent = TRUE )
    checkTrue( isErrorWithMessage(test2, "Error in checkSingleNumeric(endTime, description = \"endTime\", functionName = \"AUCPartial\") : \n  Error in  AUCPartial :  endTime is not a numeric of length 1.  Value is: 1 2\n"),
            msg = "  || TEST 2 : endTime is not of length 1")
    
    # TEST 3 : endTime is not numeric
    
    test3 <- try( AUCPartial(Conc = 3:1, Time = 0:2, endTime = "1", numPoints = 3), silent = TRUE )
    checkTrue(isErrorWithMessage(test3, "Error in checkSingleNumeric(endTime, description = \"endTime\", functionName = \"AUCPartial\") : \n  Error in  AUCPartial :  endTime is not a numeric of length 1.  Value is: 1\n"),
                msg = " || TEST 3 : endTime is not numeric ")
}
