
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test predictConc")

# tests predictConc function from predictConc.R

test_that("predictConc working", {
    
    time <- 0:10
    conc <- c(0, 10:1)
    
    # TEST 1 - 3 : predtime is an element of time - normal and boundary cases
    # predtime is the final element of time
    test1 <- mangoNCA:::predictConc(conc = conc, time = time, predtime = 10, lamznpt = 3, addt0 = TRUE)
    expect_equal(test1, 1)
    # predtime is the 3rd element of time
    test2 <- mangoNCA:::predictConc(conc = conc, time =  time, predtime = 2, lamznpt = 3, addt0 = TRUE)
    expect_equal(test2, 9)
    # predtime is the 1st element of time
    test3 <- mangoNCA:::predictConc(conc = conc, time = time, predtime = 0, lamznpt = 3)
    expect_equal(test3, 0)
    
    # TEST 4-6 : predtime is not an element of time, but is inside the lower and upper limits
    # predtime lies between first two elements of time
    test4 <- mangoNCA:::predictConc(conc = conc, time = time, predtime = 1.75, lamznpt = 3)
    expect_equal(test4, 9.25)
    # predtime lies between last 2 elements of time
    test5 <- mangoNCA:::predictConc(conc = conc, time = time, predtime = 9.25, lamznpt = 3)
    expect_equal(test5, 1.75)
    
    # predtime lies between middle elements of time
    test6 <- mangoNCA:::predictConc(conc = conc, time = time, predtime = 5.2, lamznpt = 3)
    expect_equal(test6, 5.8)
    
    # TEST 7 : alternative data set
    # Theoph data set, predtime between last 2 elements of time
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- mangoNCA:::predictConc(conc = Theoph2$conc, time = Theoph2$Time, predtime = 18, lamznpt = 3)
    expect_equal(test7,  1.980731707, tol = 1e-7)
    
    # TEST 8 : NA should be returned if predtime < earliest time
    
    test8 <- mangoNCA:::predictConc(conc =  4:1,time = 0:3, predtime = -1, lamznpt = 3, addt0 = FALSE)
    expect_true(is.na(test8))
    
    # TEST 9 : check for predtime > tlast
    
    test9 <- mangoNCA:::predictConc(conc = Theoph2$conc, time = Theoph2$Time, predtime = 26, lamznpt = 5)
    
    expect_equal(test9, 0.7573122121)
    
    # TEST 10 : NA should be returned if predtime is numeric NA
    
    test10 <- mangoNCA:::predictConc(conc = c(1, 6, 5, 4, 2, 1), time = 0:5, predtime = as.numeric(NA), lamznpt = 3, addt0 = FALSE)
    expect_true(is.na(test10))
    
    # TEST 11: Test for lambdaZStats
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    lzVec <- c(
                LAMZ = 1,
                intercept = exp(2),
                R2 = 1,
                R2ADJ = 1,
                CORRXY = -1,
                LAMZHL = log(2),
                LAMZLL = 2,
                LAMZUL = 2.75,
                lamznpt = 4)
    
    test11 <- mangoNCA:::predictConc(conc = c(0, conc), time = c(0, time), predtime = Inf, lambdaZStats = lzVec)
    expect_equal(test11, 0, msg = " || TEST 11: Test for lambdaZStats")
    
    # TEST 12: Test for excpoints
    
    # TEST 13: Test for usepoints
    
    
})

test_that("predictConc error", {

    # TEST 1 : missing lamznpt
    
    Theoph2 <- Theoph[Theoph$Subject == 2, ]
    
    expect_error(predictConc(conc = Theoph2$conc, 
            time = Theoph2$Time, 
            predtime = 26), 
        regex = "lamznpt is not a numeric of length 1.  Value is:")
   
    # TEST 2 : predtime is not a length 1 
    
    expect_error(mangoNCA:::predictConc(conc = 3:1, 
            time = 0:2, predtime = c(1, 2), lamznpt = 3),
        regex = "predtime is not a numeric of length 1.  Value is: 1 2")
    
    # TEST 3 : predtime is not numeric
    
    expect_error(mangoNCA:::predictConc(conc = 3:1, 
        time = 0:2, predtime = "1", lamznpt = 3), 
        regex = "predtime is not a numeric of length 1.  Value is:")
            
    # TEST 4 : predtime is missing
    
    expect_error(mangoNCA:::predictConc(conc = c(1, 6, 5, 4, 2, 1), 
        time = 0:5, lamznpt = 3), 
        regex = "predtime is missing with no default")
            
    # TEST 5 : predtime is not numeric NA
    
    expect_error(mangoNCA:::predictConc(conc = c(1, 6, 5, 4, 2, 1), 
            time = 0:5, predtime = NA, lamznpt = 3), 
        regex = "predtime is not a numeric of length 1.  Value is: NA")
    
})
