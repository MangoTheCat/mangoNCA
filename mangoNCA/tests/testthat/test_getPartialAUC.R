
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test getPartialAUC")

# tests getPartialAUC function from getPartialAUC.R

test_that("getPartialAUC", {
    # simple data
    t0 <- 0:9
    c0 <- 10:1
    
    # Test Data
    
    t1 <- c(0, 1, 1.5, 2, 3.08, 5, 8.92, 22.25, 48.67, 168.25, 263.45, 336.25)

    c1 <- c(37.155, 121.957, 136.175, 112.603, 127.944, 107.873, 153.631, 
        116.352, 123.991, 42.004, 28.211, 26.0)
    
    # TEST 1 - 3 : endtime is an element of time - normal and boundary cases
    
    test1 <- getPartialAUC(conc = c0, time = t0, starttime = 0, endtime = 9)
    expect_equal(test1[["AUCINT"]], AUCLast(time = t0, conc = c0 ) )
    
    test2 <- getPartialAUC(conc = c1, time =  t1, starttime = 1, endtime = 1.5)
    expect_equal(test2[["AUCINT"]], 64.533)
    
    test3 <- getPartialAUC(conc = c0, time = t0, starttime = 2, endtime = 4)
    expect_equal(test3[["AUCINT"]], AUCLast(time = 0:4, conc = 10:6 ) - AUCLast(time = 0:2, conc = 10:8 ))
    
    # TEST 4-5 : endtime is not an element of time, but is inside the lower and upper limits
    
    test4 <- getPartialAUC(conc = c0, time = t0, starttime = 0, endtime = 1.75)
    expect_equal(test4[["AUCINT"]], AUCLast(conc = c(10, 9, 8.25), time = c(0, 1, 1.75)))
    
    test5 <- getPartialAUC(conc = c0, time = t0, starttime = 2.2, endtime = 8.3)
    expect_equal(test5[["AUCINT"]], AUCLast(conc = c(10:2, 1.7), time = c(0:8, 8.3)) - AUCLast(conc = c(10:8, 7.8), time = c(0:2, 2.2)))
    
    # TEST 6 : endtime is not an element of t1, but is inside the lower and upper limits
    
    test6 <- getPartialAUC(conc = c1, time = t1, starttime = 10, endtime = 50)
    expect_equal(test6[["AUCINT"]], 4974.378865, tol = 1e-7 )
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- getPartialAUC(conc = Theoph2$conc, time = Theoph2$Time, starttime = 0, endtime = 18)
    expect_equal(test7[["AUCINT"]], AUCLast(time = Theoph2$Time[1:10], conc = Theoph2$conc[1:10]) + 14.97220, tol = 1e-7)
    
    # TEST 8 : NA should be returned if endtime < earliest time
    
    test8 <- getPartialAUC(conc =  c1, time = t1, starttime = -1, endtime = 1)
    expect_true(is.na(test8[["AUCINT"]]))
    
    # TEST 9-13 : check for endtime > tlast, calculable lambdaz
    
    test9 <- getPartialAUC(conc = Theoph2$conc, time = Theoph2$Time, starttime = 0, endtime = 26)
    expect_equal(test9[["AUCINT"]], 92.91137853, tol = 1e-7 )
    
    test10 <- getPartialAUC(conc = c1, time = t1, starttime = 500, endtime = 600)
    expect_equal(test10[["AUCINT"]], 680.757494, tol = 1e-7 )

    test11 <- getPartialAUC(conc = c(0, 3.94, 1.41, 1.71, 0.98, 0.57), 
        time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), starttime = 500, endtime = 600)
        expect_equal(test11[["AUCINT"]], 3.404974791e-08, tol = 1e-7 )
    
    test12 <- suppressWarnings(getPartialAUC(conc = c(0, 3.94, 1.41, 1.71, 0.98, 0.57), 
        time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), starttime = 100, endtime = 100))
        expect_equal(test12[["AUCINT"]], 0, tol = 1e-7 )
    
    test13 <- getPartialAUC(conc = c(0, 394000000, 141000000, 171000000, 98000000, 57000000), 
        time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), starttime = 70.00000000001, endtime = 70.00000000002)
        expect_equal(test13[["AUCINT"]], 0.0005226135254, tol = 1e-7 )

    # TEST 14 : check for endtime < tlast, lambdaz NA
    
    c2 <- c(0, 3.94, 1.41, 1.71, 0.57)
    t2 <- c(0, 20.76923, 33.53846, 45.30769, 68.84615)
    
    test14 <- getPartialAUC(conc = c2, time = t2, starttime = 3, endtime = 26)
    expect_equal(test14[["AUCINT"]], 57.96040244, tol = 1e-7 )
    
    # TEST 15 : check for endtime < tlast, lambdaz NA
    
    test15 <- getPartialAUC(conc = c2, time = t2, starttime = 50, endtime = 600)
    expect_equal(test15[["AUCINT"]], as.numeric(NA) )
})

test_that("getPartialAUC error", {
    
    # TEST 1 : missing values in conc
    
    time <- 0:9
    conc <- c(10:4, NA, 2:1)
    
    test1 <- getPartialAUC(conc = conc, time = time, starttime = 2, endtime = 8)
    expect_equal(test1[["ERROR"]], "Error in try(if (any(is.na(conc))) { : Missing values in conc\n")
   
    # TEST 2 : endtime is not a length 1 
    
    test2 <- getPartialAUC(conc = 2:1, time = 1:2, starttime = 0, endtime = c(1, 2))
    expect_equal(test2[["ERROR"]], "Error in checkSingleNumeric(endtime, description = \"endtime\", \"getPartialAUC\") : \n  Error in  getPartialAUC :  endtime is not a numeric of length 1.  Value is: 1 2\n")
    
    # TEST 3 : starttime is not numeric
    
    test3 <- getPartialAUC(conc = 2:1, time = 1:2, starttime = "1", endtime = 3)
    expect_equal(test3[["ERROR"]], "Error in checkSingleNumeric(starttime, description = \"starttime\", \"getPartialAUC\") : \n  Error in  getPartialAUC :  starttime is not a numeric of length 1.  Value is: 1\n")
    
})
