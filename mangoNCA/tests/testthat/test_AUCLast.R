
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests AUCLast from AUCLast.R

context("test AUCLast")

test_that("AUCLast", {
    # TEST 1 : basic contrived data set, no missing data
  
    test1 <- AUCLast(conc = rep(2, 100), time = 0:99, addt0 = FALSE)
    expect_equal( test1, 2 * 99, msg = " || TEST 1 : Contrived data set AUC correct  " )
    
    # TEST 2 : contrived data set, last concentration element is 0
    
    test2 <- AUCLast(conc = c(rep(2, 9), 0), time = 0:9, addt0 = FALSE)
    expect_equal( test2, 8 * 2, msg = " || TEST 2 : contrived data set, last concentration element is 0" )
    
    # TEST 3 : contrived data set, multiple 0 elements at the end
    
    test3 <- AUCLast(conc = c(0, 8:2, 0, 0), time = 0:9, addt0 = FALSE)
    expect_equal( test3, 34, msg = " || TEST 3 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 4 : NA present in time values
    
    test4 <- suppressWarnings(AUCLast(conc = 3:1, time =  c(NA, 1, 2), addt0 = TRUE))
    expect_equal(test4, 2.5) # mangoNCA:::trapezium area 1 (from 0:1) + 1.5 (from 1:2)
    
    # TEST 5 : NA present in concentration values
    
    test5 <- suppressWarnings(AUCLast(conc = c(NA, 2:1), time =  1:3, addt0 = TRUE))
    expect_equal(test5, 3.5) # mangoNCA:::trapezium area 2 (from 0:2) + 1.5 (from 2:3)
    
    # TEST 6 : actual data set
   
    Theoph1 <- subset(Theoph, Subject == 1)
    
    test6 <- AUCLast(conc = Theoph1$conc, time = Theoph1$Time)
    expect_equal(test6, 148.92305, tol = 1e-6)
    
    # TEST 7 : unsorted time values
    
    expect_error(AUCLast(conc = 1:10, time = 9:0), 
        regex = "time is not ordered.  Actual value is 9 8 7 6 5 4 3 2 1 0")

    # TEST 8 : 1 concentration has area 0
    
    test8 <- AUCLast(conc = 1, time = 0, addt0 = FALSE)
    expect_equal(test8, 0)
    
    # TEST 9 : 2 concentrations including T = 0
    
    test9 <- AUCLast(conc = 1:2, time = 0:1, addt0 = TRUE)
    expect_equal(test9, 1.5)
    
    # TEST 10 : time is not a vector
    
    expect_error(AUCLast(conc = Theoph["conc"], time = Theoph["Time"]), 
        regex = "time is not a numeric vector")
    
    # TEST 11 : Negative values of conc
    
    test11a <- suppressWarnings(AUCLast(conc = c(-1, 1), time = 0:1, addt0 = TRUE))
    expect_equal( test11a, 0.5, msg = " || TEST 11a : Negative values of conc with addt0 TRUE removed and T = 0 added" )
    
    expect_error(AUCLast(conc = c(-1, 1), time = 0:1, addt0 = FALSE), 
        regex = "values of conc < 0 in cleanData")
    
    # TEST 12 : basic contrived data set, no missing data, add T = 0
  
    test12 <- suppressWarnings(AUCLast(conc = rep(2, 100), time = 1:100, addt0 = TRUE))
    expect_equal( test12, 2 * 100 - 2/2, msg = " || TEST 12 : Contrived data set AUC correct" )
    
    # TEST 13 : basic contrived data set repeated time values in time
  
    test13 <- AUCLast(conc = rep(2, 6), time = c(0, 1, 2, 2, 2, 3), addt0 = TRUE)
    expect_equal( test13, 6, msg = " || TEST 13 : basic contrived data set repeated time values in time" )
    
    # TEST 14 : basic contrived data set repeated time values in time
  
    test14 <- AUCLast(conc = 1:6, time = c(0, 1, 2, 2, 2, 3), addt0 = TRUE, inter = "Linear")
    expect_equal( test14, 9.5, msg = " || TEST 14 : basic contrived data set repeated time values in time" )
    
    # TEST 15 : WNL example Linear/Log interpolation
    
    #c15 <- c(18.01, 16.96, 14.16, 10.49, 5.803, 1.881, 0.715, 0.191, 0.096, 0.052, 0.028, 0.015, 0.008)
    #t15 <- c(0.0167, 0.033, 0.083, 0.167, 0.33, 0.67, 1, 2, 4, 6, 8, 10, 12)
    #testFile <- system.file("data", "wnl_analysis_linlog.csv", package = "mangoNCA")
    #testData <- read.csv(testFile)
    #testData <- read.csv("C:\\Users\\ccampbell\\Documents\\IMC_RapidNCA\\R\\mangoNCA\\trunk\\data\\wnl_analysis_linlog.csv")
    
    #test15 <- AUCLast(conc = c15, time = t15, addt0 = TRUE, inter = "Linear Log")
    #expect_equal(test15, testFile[1, "AUClast..hr.ug.mL."], msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    # TEST 16 : WNL example Linear/Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    # Clayton’s example data set subject 5
    
    c16 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.99, 0.73, 0.42, 0.19)
    t16 <- c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8)
    
    test16 <- AUCLast(conc = c16, time = t16, inter = "Linear Log")
    expect_equal(round(test16, digits = 5), 6.8176, msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    test16a <- AUCLast(conc = c16[-6], time = t16[-6], inter = "Linear Log")
    expect_equal(round(test16a, digits = 4), 6.9976, msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    # TEST 17 : invalid interpolation method
  
    expect_error(AUCLast(conc = 1:6, time = c(0, 1, 2, 2, 2, 3), addt0 = TRUE, inter = "abc"), 
        regex = "argument inter should have value 'Linear', 'Lin up Log down' or 'Linear Log' in AUCLast")
    
    # TEST 18: 0, 0 Linear
    
    test18 <- AUCLast(0, 0)
    expect_equal(test18, 0, msg = " || TEST 18 : 0, 0 Linear")

    # TEST 19: 0, 0 Linear Log
    
    test19 <- AUCLast(0, 0, inter = "Linear Log")
    expect_equal(test19, 0, msg = " || TEST 19 : 0, 0 Linear Log")

    # TEST 20: 0, 0 Lin up Log down
    
    test20 <- AUCLast(0, 0, inter = "Lin up Log down")
    expect_equal(test20, 0, msg = " || TEST 20 : 0, 0 Lin up Log down") 
    
    # TEST 21: WinNonlin verification 1108
    
    C1108 <- c(0L, 4L, 11L, 17L, 17L, 13L, 8L, 4L, 2L, 1L)
    
    T1108 <- c(0L, 1L, 4L, 9L, 16L, 25L, 36L, 49L, 64L, 81L)
    
    test21 <- AUCLast(conc = C1108, time = T1108, addt0 = FALSE, inter = "Linear")
    
    expect_equal(test21, 612.5, msg = " || TEST 21 : WinNonlin verification 1108") 
    
    # TEST 22: WinNonlin verification 1108 addt0
    
    C1108 <- c(4L, 11L, 17L, 17L, 13L, 8L, 4L, 2L, 1L)
    
    T1108 <- c(1L, 4L, 9L, 16L, 25L, 36L, 49L, 64L, 81L)
    
    test22 <- suppressWarnings(AUCLast(conc = C1108, time = T1108, addt0 = TRUE, 
            inter = "Linear"))
    
    expect_equal(test22, 612.5, msg = " || TEST 22 : WinNonlin verification 1108 addt0") 
    
})
