
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test checkOrderedVector")

# tests the checkOrderedVector function in checkdata.R

test_that("checkOrderedVector", {
    
    # TEST 1 : base case - ordered vector, returns NULL
    # base case - ordered vector, returns NULL
    test1 <- checkOrderedVector( 1:5 )
    expect_true(is.null(test1))
    
    # TEST 2 : ordered vector, with NAs
    
    test2 <- checkOrderedVector(c(NA, 1)) 
    expect_true(is.null(test2))
    
    # TEST 3 : length 0 vector is ordered
    test3 <- checkOrderedVector(numeric(0))
    expect_true(is.null(test3))
    
    # TEST 4 : unordered vector with no NA
    expect_error(checkOrderedVector(c(1.00001, 1), functionName = "func"), 
        regex = "x is not ordered.  Actual value is 1.00001 1")
    
    # TEST 5 : unordered vector with NA
    expect_error(checkOrderedVector(c(NA, 0, 1.00001, NA, 1), functionName = "func", description = "y"), 
        regex = "y is not ordered.  Actual value is NA 0 1.00001 NA 1")
    
    # TEST 6 : non-vector
    expect_error(checkOrderedVector( Theoph, functionName = "func", description = "Theoph"), 
        regex = "Theoph is not a vector")
})
