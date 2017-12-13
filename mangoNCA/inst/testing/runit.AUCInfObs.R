# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.AUCInfObs <- function()
{
    # TEST 1 : contrived data
    
    test1 <- AUCInfObs(Time = 0:10, Conc = c(0, 0, exp(8:2), 0, 0), numPoints = 4) # 4715.2
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    clast <- exp(2)
    
    AUCextra <- clast / lambdaz
    
    # 4707.811 is equal to AUCLast(Time = 1:10, Conc = c(0, exp(8:2) ,0, 0), addT0 = TRUE)

    checkEquals( test1, 4707.811 + AUCextra, tol = 1e-7, 
            msg = " || TEST 1 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 2 : Theoph data
    
    Theoph3 <- subset(Theoph, Subject == 3 )
    
    test2 <- AUCInfObs(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 3)
    
    lambdaz <- 0.1024443141094   # lm(log(conc) ~ Time, data = Theoph3[9:11, ])
    tlast <- 24.17
    inter <- exp(2.5297115014586)
    
    clast <- tail(Theoph3$conc, 1)
    
    AUCextra <- clast / lambdaz
    
    checkEquals(test2,  99.2865 + AUCextra, tol = 1e-6, 
            msg = " || TEST 2 : theoph data set calculation") 
    
    # TEST 3 : failed lambda z calculation, should get NA 
    
    test3 <- AUCInfObs( Conc = 0:3, Time =  c(0, 2:4), numPoints = 3 )
    checkEquals(test3, as.numeric(NA), msg = " || TEST 3 : NA value returned because lambda z regression fit failed")
    
    # TEST 4 : Moment curve test 1
    
    test4 <- AUCInfObs(Time = 0:10, Conc = c(0, 0, exp(8:2), 0, 0), numPoints = 4, calculation = "moment")
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    clast <- exp(2)
    
    AUMCextra <- clast* tlast / lambdaz + clast / lambdaz^2
    
    checkEquals( test4, 12105.3403019096 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 4 : contrived data set (AUMC), multiple 0 elements at the end" )
    
    # TEST 5 : Moment curve test 2

    test5 <- AUCInfObs(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 5, calculation = "moment")
    
    lambdaz <- 0.0945762934220462  # lm(log(conc) ~ Time, data = Theoph3[7:11, ])
    tlast <- 24.17
    inter <- exp(2.37478277690206) 
    
    clast <- tail(Theoph3$conc, 1)
    
    AUMCextra <- clast * tlast / lambdaz + clast / lambdaz^2
    
    checkEquals(test5, 803.18587 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 5 : theoph data set AUMC calculation correct") 
            
    
    # TEST 6 : invalid numPoints

    test6 <- AUCInfObs(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 2, calculation = "moment")
    checkTrue( is.na(test6), msg = " || TEST 6 : invalid numPoints")
}

test.AUCInfObs_errorHandling <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : different length vectors generates error
    
    test1 <- try(AUCInfObs(Time = 1:4, Conc = 3:1), silent = TRUE )
    checkTrue( isErrorWithMessage( test1, 
                "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\", \"AUCInfObs\") : \n  Error in AUCInfObs:  lengths of Time and Concentration do not match\n" ), 
            msg = " || TEST 1 : different length vectors generates error" )
    
    # TEST 2 : Time not ordered
    
    test2 <- try(AUCInfObs(Time = c(1:4, 3), Conc = c(4:1, 1), numPoints = 1), silent = TRUE)
    checkTrue( isErrorWithMessage(test2,
                "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"AUCInfObs\") : \n  Error in AUCInfObs: Time is not ordered.  Actual value is 1 2 3 4 3\n"),
            msg = " || TEST 2 : Time not ordered")

    # TEST 3 : numPoints invalid
    
    test3 <- try(AUCInfObs(Time = c(1:5), Conc = c(4:1, 1), numPoints = 3:4, addT0 = TRUE), silent = TRUE)
    checkTrue( isErrorWithMessage(test3,
                "Error in checkSingleNumeric(numPoints, description = \"numPoints\") : \n  Error in  AUCInfObs :  numPoints is not a numeric of length 1.  Value is: 3 4\n"),
            msg = " || TEST 3 : numPoints invalid")
}
