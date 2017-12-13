# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.AUCInfPred <- function()
{
    
    # TEST 1 : contrived data
    
    test1 <- AUCInfPred(Time = 0:10, Conc = c(0, 0, exp(8:2), 0, 0), numPoints = 4)
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUCextra <- cEst / lambdaz
    
    # 4707.811 is equal to AUCLast(Time = 1:10, Conc = c(0, exp(8:2) ,0, 0))

    checkEquals( test1, 4707.811 + AUCextra, tol = 1e-7, 
            msg = " || TEST 1 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 2 : Theoph data
    
    Theoph3 <- subset(Theoph, Subject == 3 )
    
    test2 <- AUCInfPred(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 5)
    
    lambdaz <- 0.0945763   
    tlast <- 24.17
    inter <- exp( 2.3747828) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUCextra <- cEst / lambdaz
    
    checkEquals(test2,  99.2865 + AUCextra, tol = 1e-7, 
            msg = " || TEST 2 : theoph data set calculation correct") 
    
    # TEST 3 : failed lambda z calculation, should get NA 
    
    test3 <- AUCInfPred( Conc = 0:3, Time =  c(0, 2:4), numPoints = 3 )
    checkEquals(test3, as.numeric(NA), msg = " || TEST 3 : NA value returned because lambda z regression fit failed")
    
    # TEST 4 : Moment curve test 1
    
    test4 <- AUCInfPred(Time = 0:10, Conc = c(0, 0, exp(8:2), 0, 0), numPoints = 4, calculation = "moment")
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUMCextra <- cEst* tlast / lambdaz + cEst / lambdaz^2
    
    checkEquals( test4, 12105.3403019096 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 4 : contrived data set (AUMC), multiple 0 elements at the end" )
    
    # TEST 5 : Moment curve test 2

    test5 <- AUCInfPred(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 5, calculation = "moment")
    
    lambdaz <- 0.0945762934220462    
    tlast <- 24.17
    inter <- 10.7486780713157 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUMCextra <- cEst * tlast / lambdaz + cEst / lambdaz^2
    
    checkEquals(test5, 803.18587 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 5 : theoph data set AUMC calculation correct") 
            
    # TEST 6 : invalid numPoints

    test6 <- AUCInfPred(Time = Theoph3$Time, Conc = Theoph3$conc , numPoints = 2, calculation = "moment")
    checkTrue( is.na(test6), msg = " || TEST 6 : invalid numPoints")
}

test.AUCInfPred_errorHandling <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : different length vectors generates error
    
    test1 <- try(AUCInfPred(Time = 1:4, Conc = 3:1), silent = TRUE )
    checkTrue( isErrorWithMessage( test1, 
                    "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\", \"AUCInfPred\") : \n  Error in AUCInfPred:  lengths of Time and Concentration do not match\n" ) )
    
    # TEST 2 : Time not ordered
    
    test2 <- try(AUCInfPred(Time = c(1:4, 3), Conc = c(4:1, 1), numPoints = 1), silent = TRUE)
    checkTrue( isErrorWithMessage(test2,
                "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"AUCInfPred\") : \n  Error in AUCInfPred: Time is not ordered.  Actual value is 1 2 3 4 3\n"),
            msg = " || TEST 2 : Time not ordered")

    # TEST 3 : numPoints invalid
    
    test3 <- try(AUCInfPred(Time = c(1:5), Conc = c(4:1, 1), numPoints = 3:4, addT0 = TRUE), silent = TRUE)
    checkTrue( isErrorWithMessage(test3,
                "Error in checkSingleNumeric(numPoints, description = \"numPoints\") : \n  Error in  AUCInfPred :  numPoints is not a numeric of length 1.  Value is: 3 4\n"),
            msg = " || TEST 3 : numPoints invalid")
}
