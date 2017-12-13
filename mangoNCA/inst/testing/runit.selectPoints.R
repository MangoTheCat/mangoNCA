# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


# tests selectPoints from selectPoints.R

test.selectPoints <- function()
{
    
    # TEST 1 : contrived data set, does not pass regression comparison decision rules
  
    test1 <- selectPoints(Conc = c(2, rep(1, 9)), Time = 0:9)
    checkEquals( test1$numPoints, as.numeric(NA), msg = " || TEST 1 : contrived data set, does not pass regression comparison decision rules" )
    
    # TEST 2 : contrived data set, does not pass regression comparison decision rules
    
    test2 <- selectPoints(Conc = c(rep(2, 9), 0), Time = 1:10)
    checkEquals( test2$numPoints, as.numeric(NA), msg = " || TEST 2 : does not pass regression comparison decision rules" )
    
    # TEST 3 : contrived data set, multiple 0 elements at the end
    
    test3 <- selectPoints(Conc = c(0, 8:2, 0, 0), Time = 1:10)
    checkEquals( test3$numPoints, 4, msg = " || TEST 3 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 4 : contrived data set, handle huge numbers
    
    test4 <- selectPoints(Conc = c(0, 1.4e+12, 1.3e+08, 4.2e+04, 1.8e+03, 7.7e+02), Time = c(0, 10, 100, 200, 400, 600))
    checkEquals( test4$numPoints, 3, msg = " || TEST 4 : handle huge numbers" )
    
    # TEST 5 : contrived data set, handle tiny numbers
    
    test5 <- selectPoints(Conc = c(0, 3.4e+03, 2.5e+01, 5.9e-01, 1.7e-03, 7.4e-10, 1.3e-11), Time = c(0, 10, 100, 200, 400, 600, 1200))
    checkEquals( test5$numPoints, 5, msg = " || TEST 5 : handle tiny numbers" )
    
    # TEST 6 : actual data set
   
    Theoph1 <- subset(Theoph, Subject == 1)
    test6 <- selectPoints(Conc = Theoph1$conc, Time = Theoph1$Time, minPoints = 2)
    checkEquals(test6$numPoints, 3, msg = " || TEST 6 : actual data set" )
    
    # TEST 7 : actual data set Patient 1007, validated against WinNonlin
    
    dataFile <- system.file("data", "wnl_data.csv", package = "MangoNca")
    dataInput <- read.csv(dataFile)
    
    # Provide raw data for Patient 1007
    
    dataP1007 <- dataInput[dataInput$SUBJECT == 1007, ]
    
    test7 <- selectPoints(Conc = dataP1007$CONC, dataP1007$TIME)
    checkEquals(test7$numPoints, 3, msg = " || TEST 7 : actual data set Patient 1007, validated against WinNonlin" )
    
    # TEST 8 : actual data set Patient 1037, validated against WinNonlin
    
    dataP1037 <- dataInput[dataInput$SUBJECT == 1037, ]
    
    test8 <- selectPoints(Conc = dataP1037$CONC, dataP1037$TIME)
    checkEquals(test8$numPoints, 4, msg = " || TEST 8 : actual data set Patient 1037, validated against WinNonlin" )
    
    # TEST 9 : fewer than minPoints rows after Cmax generates NA
    
    test9 <- selectPoints(Conc = c(4, 5, 4:2), Time = 0:4, minPoints = 60)
    checkTrue( is.na( test9$numPoints ), msg = " || TEST 9 : fewer than minPoints rows after Cmax generates NA")
    
    # TEST 10 : length 0 vectors
    
    test10 <- selectPoints(numeric(0), numeric(0))
    checkTrue(is.na( test10$numPoints ), msg = " || TEST 10 : length 0 vectors")
    
    # TEST 11 : no calculable lambdaz due to adjR2 < 0.8
    
    c2 <- c(0, 3.94, 1.41, 1.71, 0.57)
    t2 <- c(0, 20.76923, 33.53846, 45.30769, 68.84615)
    test11 <- selectPoints(Conc = c2, Time = t2)
    checkEquals(test11$numPoints, as.numeric(NA), msg = " || TEST 11 : no calculable lambdaz due to adjR2 < 0.8" )
    
}

test.selectPoints_errorHandling <- function()
{
    
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST E1 : non-numeric vector due to miscoded value in Conc  
    
    testE1 <- try(selectPoints(Conc = c(4, 65, 45, 17, 2, "BLA"), Time = 0:5), silent = TRUE)
    checkTrue( isErrorWithMessage(testE1, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"selectPoints\") : \n  Error in selectPoints: Conc is not a numeric vector\n"), 
            msg = " || TEST E1 : non-numeric vector due to miscoded value in Conc")
            
    # TEST E2 : Check unsorted time values
    
    testE2 <- try(selectPoints(Time = c(1:4, 3:6), Conc = c(2, 8, 9, 7, 5, 5, 4, 4)), silent = TRUE )
    checkTrue( isErrorWithMessage(testE2, "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"selectPoints\") : \n  Error in selectPoints: Time is not ordered.  Actual value is 1 2 3 4 3 4 5 6\n"),
        msg = " || TEST E2 : Check unsorted time values")
        
    # TEST E3 : Time is not a vector
    
    Theoph7 <- subset(Theoph, Subject == 7)
    
    testE3 <- try(selectPoints(Conc = Theoph7["conc"], Time = Theoph7["Time"]), silent = TRUE)
    checkTrue( isErrorWithMessage(testE3, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"selectPoints\") : \n  Error in selectPoints: Time is not a numeric vector\n"), 
            msg = " || TEST E3 : Time is not a vector")
    
    # TEST E4 : actual data set, invalid minPoints
   
    testE4 <- try(selectPoints(Conc = Theoph7$conc, Time = Theoph7$Time, minPoints = 1), silent = TRUE)
    checkTrue( isErrorWithMessage(testE4, "Error in selectPoints(Conc = Theoph7$conc, Time = Theoph7$Time, minPoints = 1) : \n  Error in selectPoints: minPoints must be at least 2\n"), 
            msg = " || TEST E4 : actual data set, invalid minPoints")

    # TEST E5 : NA present in concentration values
    
    testE5 <- try(selectPoints(Conc = c(3, NA, 3, 2, 1), Time =  c(0, 1, 2, 3, 5)), silent = TRUE)
    checkTrue( isErrorWithMessage(testE5, "Error in selectPoints(Conc = c(3, NA, 3, 2, 1), Time = c(0, 1, 2, 3, 5)) : \n  call to cleanConcTime failed, message is:  Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n\n"), 
            msg = " || TEST E5 : NA present in concentration values")
    
    # TEST E6 : NA present in time values
    
    testE6 <- try(selectPoints(Conc = c(4, 3, 2, 1.5, 1), Time =  c(0, NA, 1, 2, 3.1)), silent = TRUE)
    checkTrue( isErrorWithMessage(testE6, "Error in selectPoints(Conc = c(4, 3, 2, 1.5, 1), Time = c(0, NA, 1, 2,  : \n  call to cleanConcTime failed, message is:  Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n\n"), 
            msg = " || TEST E6 : NA present in time values") 
}
    
