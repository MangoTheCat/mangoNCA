
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("clearance")

# Tests the clearance function 

test_that("clearance", {
    
    # TEST 1 : Nonzero AUCInf
    
    test1 <-  clearance(AUCInf = 20000, dose = 20 * 1000)
    expect_equal( test1, 1.0 )
    
    # TEST 2 : AUCInf NA
    
    test2 <- clearance(AUCInf = as.numeric(NA), dose = 15 * 1000)
    expect_equal(test2, as.numeric(NA))
    
    # TEST 3 : negative AUCInf
    
    test3 <- clearance(AUCInf = -50000, dose = 20 * 1000)
    expect_equal(test3, as.numeric(NA))
    
    # TEST 4a : Infinite AUC
    
    test4a <- clearance(AUCInf = Inf, dose = 1000 * 1000)
    expect_equal(test4a, as.numeric(0))
    
    # TEST 4b : Infinite AUC
    
    test4b <- clearance(AUCInf = -Inf, dose = 1000 * 1000)
    expect_equal(test4b, as.numeric(NA))
        
    # TEST 5 : 0 length vector
    
    expect_error(clearance(AUCInf = numeric(0), dose = pi), 
        regex = "Error in  clearance :  AUCInf is not a numeric of length 1")
        
    # TEST 6 : AUC is zero
    
    expect_error(clearance(AUCInf = FALSE, dose = 1), 
        regex = "Error in  clearance :  AUCInf is not a numeric of length 1.  Value is: FALSE")
                
    # TEST 7 : basic data check
    
    expect_error(clearance(AUCInf = "BLQ", dose = 1), 
        regex = "Error in  clearance :  AUCInf is not a numeric of length 1.  Value is: BLQ")
})

