# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 18/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function testTrailPoints from checkdata.R

test.testTrailPoints <- function()
{
    if (!exists("testTrailPoints", mode = "function")) { testTrailPoints <- MangoNca:::testTrailPoints }
    
    # TEST 1 : standard check minPoints after Cmax
    
    test1 <- testTrailPoints(Conc = 4:1, Time = 1:4, minPoints = 3)
    checkTrue( test1, msg = " || TEST1: standard check minPoints after Cmax" )  
    
    # TEST 2 : minPoints > number of points after Cmax

    test2 <- testTrailPoints(Conc = 4:1, Time = 1:4, minPoints = 4)
    checkTrue( !test2, msg = " || TEST2: minPoints > number of points after Cmax" ) 
    
    # TEST 3 : All NA vector used
    
    NAVector <- rep(as.numeric(NA), 4)
    test3 <- try(testTrailPoints(Conc = NAVector, Time = 1:4, minPoints = 1), silent = TRUE)
    checkEquals( paste(test3), "Error in testTrailPoints(Conc = NAVector, Time = 1:4, minPoints = 1) : \n  call to cleanConcTime failed, message is:  Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n\n", 
        msg = " || TEST 3 : All NA vector used")
    
    # TEST 4 : actual data set Patient 1007, validated against WinNonlin
    
    dataFile <- system.file("data", "wnl_data.csv", package = "MangoNca")
    dataInput <- read.csv(dataFile)
    
    # Provide raw data for Patient 1007
    
    dataP1033 <- dataInput[dataInput$SUBJECT == 1033, ]
    
    test4 <- testTrailPoints(Conc = dataP1033$CONC, dataP1033$TIME)
    checkTrue( test4, msg = " || TEST 4 : actual data set Patient 1007, validated against WinNonlin" )    
    
    # TEST 5 : actual data set, validated against trapezium function
   
    Theoph1 <- subset(Theoph, Subject == 1)
    test5 <- testTrailPoints(Conc = Theoph1$conc, Time = Theoph1$Time)
    checkTrue(test5, msg = " || TEST 5 : actual data set, validated against trapezium function" )
    
    # TEST 6 : zero length data frame
   
    zeroDf <- data.frame(Conc = numeric(0), Time = numeric(0), excPoints = logical(0))
    test6 <- testTrailPoints(Conc = zeroDf$Conc, Time = zeroDf$Time, excPoints = zeroDf$excPoints)
    checkTrue(!test6, msg = " || TEST 6 : zero length data frame" )
    
}
    

test.testTrailPoints_errorHandling <- function()
{

    if (!exists("testTrailPoints", mode = "function")) { testTrailPoints <- MangoNca:::testTrailPoints }
    isErrorWithMessage <- MangoNca:::isErrorWithMessage

    # TEST E1 : Vector is non-numeric
    
    Theoph6 <- subset(Theoph, Subject == 6)
    testE1 <- try(testTrailPoints(Conc = Theoph6["conc"], Time = Theoph6["Time"]), silent = TRUE)
    checkTrue( isErrorWithMessage(testE1, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"testTrailPoints\") : \n  Error in testTrailPoints: Time is not a numeric vector\n"), 
            msg = " || TEST E1 : Vector is non-numeric")
    
    # TEST E2 : Differing length numeric vectors
    
    testE2 <- try(testTrailPoints(Conc = Theoph6[1:10, "conc"], Time = Theoph6[1:9, "Time"]), silent = TRUE)
    checkTrue( isErrorWithMessage(testE2, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"testTrailPoints\") : \n  Error in testTrailPoints:  lengths of Time and Conc do not match\n"), 
            msg = " || TEST E2 : Differing length numeric vectors")

    # TEST E3 : NULL values
    
    testE3 <- try(testTrailPoints(Conc = NULL, Time = NULL), silent = TRUE)
    checkTrue( isErrorWithMessage(testE3, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"testTrailPoints\") : \n  Error in testTrailPoints: Time is not a numeric vector\n"), 
            msg = " || TEST E3 : NULL values")
            
    # TEST E4 : Numeric matrix used, this should fail as inputs must be vectors
    
    testE4 <-try( testTrailPoints(Conc = matrix(4:1), Time = 1:4, minPoints = 2), silent = TRUE) 
    checkTrue( isErrorWithMessage(testE4, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"testTrailPoints\") : \n  Error in testTrailPoints: Conc is not a numeric vector\n"), 
            msg = " || TEST E4 : Numeric matrix used, this should fail as inputs must be vectors")
            
    # TEST E5 : minPoints length > 1, exception generated
    
    testE5 <- try( testTrailPoints(Conc = 5:1, Time = 1:5, minPoints = 1:5), silent = TRUE )
    checkTrue( isErrorWithMessage(testE5, "Error in checkSingleNumeric(minPoints, description = \"minPoints\", functionName = \"testTrailPoints\") : \n  Error in  testTrailPoints :  minPoints is not a numeric of length 1.  Value is: 1 2 3 4 5\n"), 
            msg = " || TEST E5 : minPoints length > 1, exception generated")
            
    # TEST E6 : Non-numeric vector used, exception generated
    
    testE6 <- try( testTrailPoints(Conc = c("BLQ", 8, 5, 4, 4), Time = 1:5, minPoints = 3), silent = TRUE)
    checkTrue( isErrorWithMessage(testE6, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"testTrailPoints\") : \n  Error in testTrailPoints: Conc is not a numeric vector\n"), 
        msg = " || TEST E6 : Non-numeric vector used, exception generated")
    
}


test.stripTrailingZeros <- function()
{
    if (!exists("stripTrailingZeros", mode = "function")) { stripTrailingZeros <- MangoNca:::stripTrailingZeros }
    
    # TEST 1: simple example
    
    conc <- c(43, 23, 0, 14, 9, 0, 0)
    
    test1 <- stripTrailingZeros(Conc = conc, Time = (0:6) * 10, excPoints = FALSE)
    checkEquals(test1, data.frame(Conc = conc[1:5], Time = (0:4) * 10, excPoints = FALSE), 
        msg = "TEST 1: simple example")
    
    # TEST 2: simple error example
    
    test2 <- try(stripTrailingZeros(Conc = conc, Time = (0:6) * 10, excPoints = c(FALSE, TRUE)), silent = TRUE)
    checkEquals(paste(test2), "Error in checkLogicalSameLength(excPoints, Conc, \"excPoints\", \"Conc\",  : \n  Error in stripTrailingZeros:  lengths of excPoints and Conc do not match\n", 
        msg = "TEST 2: simple error example")
    
    # TEST 3: NAs are not allowed
    
    test3 <- try(stripTrailingZeros(Conc = rep(NA, 4), Time = 0:3, excPoints = rep(FALSE, 4)), silent = TRUE)
    checkEquals(paste(test3), "Error in stripTrailingZeros(Conc = rep(NA, 4), Time = 0:3, excPoints = rep(FALSE,  : \n  call to cleanConcTime failed, message is:  Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n\n", 
        msg = "TEST 3: NAs are not allowed")
    
}
