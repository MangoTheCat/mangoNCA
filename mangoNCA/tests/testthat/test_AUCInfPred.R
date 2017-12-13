
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUCInfPred")

test_that("AUCInfPred",  {
    
    # TEST 1 : contrived data
    
    test1 <- suppressWarnings(AUCInfPred(time = 0:10, 
        conc = c(0, 0, exp(8:2), 0, 0), lamznpt = 4))
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUCextra <- cEst / lambdaz
    
    # 4707.811 is equal to AUCLast(time = 1:10, conc = c(0, exp(8:2) ,0, 0))

    expect_equal( test1, 4707.811 + AUCextra, tol = 1e-7)
    
    # TEST 2 : Theoph data
    
    Theoph3 <- subset(Theoph, Subject == 3 )
    
    test2 <- AUCInfPred(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 5)
    
    lambdaz <- 0.0945763   
    tlast <- 24.17
    inter <- exp( 2.3747828) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUCextra <- cEst / lambdaz
    
    expect_equal(test2,  99.2865 + AUCextra, tol = 1e-7, 
            msg = " || TEST 2 : theoph data set calculation correct") 
    
    # TEST 3 : failed lambda z calculation, should get NA 
    
    test3 <- AUCInfPred( conc = 0:3, time =  c(0, 2:4), lamznpt = 3 )
    expect_equal(test3, as.numeric(NA))
    
    # TEST 4 : Moment curve test 1
    
    test4 <- suppressWarnings(AUCInfPred(time = 0:10, 
        conc = c(0, 0, exp(8:2), 0, 0), lamznpt = 4, calculation = "moment"))
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUMCextra <- cEst* tlast / lambdaz + cEst / lambdaz^2
    
    expect_equal(test4, 12105.3403019096 + AUMCextra, tol = 1e-7)
    
    # TEST 5 : Moment curve test 2

    test5 <- AUCInfPred(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 5, calculation = "moment")
    
    lambdaz <- 0.0945762934220462    
    tlast <- 24.17
    inter <- 10.7486780713157 
    
    cEst <- exp(-lambdaz * tlast) * inter
    AUMCextra <- cEst * tlast / lambdaz + cEst / lambdaz^2
    
    expect_equal(test5, 803.18587 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 5 : theoph data set AUMC calculation correct") 
            
    # TEST 6 : invalid lamznpt

    test6 <- suppressWarnings(AUCInfPred(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 2, calculation = "moment"))
    expect_true(is.na(test6))
})

test_that("AUCInfPred", {
    
    # TEST 1 : different length vectors generates error
    
    expect_error(AUCInfPred(time = 1:4, conc = 3:1), 
        regex = "lengths of time and concentration do not match")
    
    # TEST 2 : time not ordered
    
    expect_error(AUCInfPred(time = c(1:4, 3), conc = c(4:1, 1), lamznpt = 1), 
        regex = "time is not ordered.  Actual value is 1 2 3 4 3")

    # TEST 3 : lamznpt invalid
    
    expect_error(AUCInfPred(time = c(1:5), conc = c(4:1, 1), lamznpt = 3:4, addt0 = TRUE), 
        regex = "lamznpt is not a numeric of length 1.  Value is: 3 4")
})
