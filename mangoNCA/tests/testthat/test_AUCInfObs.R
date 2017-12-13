
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUCInfObs")

test_that("AUCInfObs",  {
    # TEST 1 : contrived data
    
    test1 <- suppressWarnings(AUCInfObs(time = 0:10, conc = c(0, 0, exp(8:2), 0, 0), lamznpt = 4)) # 4715.2
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    clast <- exp(2)
    
    AUCextra <- clast / lambdaz
    
    # 4707.811 is equal to AUCLast(time = 1:10, conc = c(0, exp(8:2) ,0, 0), addt0 = TRUE)

    expect_equal( test1, 4707.811 + AUCextra, tol = 1e-7, 
            msg = " || TEST 1 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 2 : Theoph data
    
    Theoph3 <- subset(Theoph, Subject == 3 )
    
    test2 <- AUCInfObs(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 3)
    
    lambdaz <- 0.1024443141094   # lm(log(conc) ~ time, data = Theoph3[9:11, ])
    tlast <- 24.17
    inter <- exp(2.5297115014586)
    
    clast <- tail(Theoph3$conc, 1)
    
    AUCextra <- clast / lambdaz
    
    expect_equal(test2,  99.2865 + AUCextra, tol = 1e-6, 
            msg = " || TEST 2 : theoph data set calculation") 
    
    # TEST 3 : failed lambda z calculation, should get NA 
    
    test3 <- AUCInfObs( conc = 0:3, time =  c(0, 2:4), lamznpt = 3 )
    expect_equal(test3, as.numeric(NA), msg = " || TEST 3 : NA value returned because lambda z regression fit failed")
    
    # TEST 4 : Moment curve test 1
    
    test4 <- suppressWarnings(AUCInfObs(time = 0:10, 
        conc = c(0, 0, exp(8:2), 0, 0), lamznpt = 4, 
        calculation = "moment"))
    
    lambdaz <- 1 
    tlast <- 8
    inter <- exp(10) 
    
    clast <- exp(2)
    
    AUMCextra <- clast* tlast / lambdaz + clast / lambdaz^2
    
    expect_equal( test4, 12105.3403019096 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 4 : contrived data set (AUMC), multiple 0 elements at the end" )
    
    # TEST 5 : Moment curve test 2

    test5 <- AUCInfObs(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 5, calculation = "moment")
    
    lambdaz <- 0.0945762934220462  # lm(log(conc) ~ time, data = Theoph3[7:11, ])
    tlast <- 24.17
    inter <- exp(2.37478277690206) 
    
    clast <- tail(Theoph3$conc, 1)
    
    AUMCextra <- clast * tlast / lambdaz + clast / lambdaz^2
    
    expect_equal(test5, 803.18587 + AUMCextra, tol = 1e-7, 
            msg = " || TEST 5 : theoph data set AUMC calculation correct") 
            
    
    # TEST 6 : invalid lamznpt

    test6 <- suppressWarnings(
        AUCInfObs(time = Theoph3$Time, conc = Theoph3$conc , lamznpt = 2, calculation = "moment"))
    expect_true(is.na(test6))
})

test_that("AUCInfObs error",  {
    
    # TEST 1 : different length vectors generates error
    
    expect_error(AUCInfObs(time = 1:4, conc = 3:1), 
            regex = "lengths of time and concentration do not match")
    
    # TEST 2 : time not ordered
    
    expect_error(AUCInfObs(time = c(1:4, 3), conc = c(4:1, 1), 
            lamznpt = 1), 
        regex = "time is not ordered.  Actual value is 1 2 3 4 3")

    # TEST 3 : lamznpt invalid
    
    expect_error(AUCInfObs(time = c(1:5), conc = c(4:1, 1), 
            lamznpt = 3:4, addt0 = TRUE), 
        regex = "lamznpt is not a numeric of length 1.  Value is: 3 4")
})
