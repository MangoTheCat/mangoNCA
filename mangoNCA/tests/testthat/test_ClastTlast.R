
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("check ClastTlast")
# Tests the ClastTlast function 

test_that("ClastTlast", {
    # use theophiline data
    # Theoph1 will be a data.frame with the Theoph data for subject 1 
    
    Theoph1 <- subset(  Theoph, Subject == 1)
    
    # TEST 1 : standard data set, no NA or 0 values
    
    test1 <-  ClastTlast( conc = Theoph1$conc, time = Theoph1$Time )
    expect_equal( test1, list(clast = 3.28, tlast = 24.37, index = 11) )
    
    # TEST 2 : data set with mix of 0s and NAs, and only one measurable concentration
    
    test2 <- ClastTlast(conc = c(0, 0, NA, 0, 0, 1, NA, NA, 0, 0, NA), time = 1:11 )
    expect_equal(test2, list(clast = 1, tlast = 6, index = 6))
    
    # TEST 3 : data with mix of 0s, NAs, and several measurable concentrations
    
    test3 <- ClastTlast(conc = c(0, 0, NA, 0, 0, 1, NA, 2, 0, 0, NA), time = 1:11 )
    expect_equal(test3, list(clast = 2, tlast = 8, index = 8))
    
    # TEST 4 : only first concentration is measurable
    
    test4 <- ClastTlast(conc = c(1, 0, NA, 0, 0, 0), time = 1:6 )
    expect_equal(test4, list(clast = 1, tlast = 1, index = 1))
    
    # TEST 5 : only last concentration is measurable
    
    test5 <- ClastTlast(conc = c(NA, 0, NA, 0, 0, 1), time = 1:6 )
    expect_equal(test5, list(clast = 1, tlast = 6, index = 6))
    
    # TEST 6 : no measurable concentrations
    
    naResult <- list(clast = as.numeric(NA), tlast = as.numeric(NA), index = as.numeric(NA))
    test6 <- ClastTlast(conc = c(NA, 0, NA, 0, 0, 0), time = 1:6 )
    expect_equal(test6, naResult)
        
    # TEST 7 : 0 length vectors
    
    test7 <-  ClastTlast(conc = numeric(0), time = numeric(0))
    expect_equal(test7, naResult)
    
    # TEST 8 : concentrations add up to 0
    
    test8 <-  ClastTlast(conc = c(0, NA, 1, NA, -1), time = 6:10)
    expect_equal(test8, naResult)
                
    # TEST 9 : basic data check
    
    expect_error(ClastTlast( conc = 1, time = "b" ), 
        regex = "time is not a numeric vector")
    
    # TEST 10 : unsorted time generates exception
    expect_error(ClastTlast( conc = 1:2, time = 2:1 ), 
        regex = "time vector is not sorted")
    
})

