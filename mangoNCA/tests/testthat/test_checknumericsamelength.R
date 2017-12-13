
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test checkLogicalSameLength")

# tests the function checkNumericSameLength from checkdata.R

test_that("checkLogicalSameLength", {
    
    # TEST 1 : standard case
    
    test1 <- checkNumericSameLength(1:10, 1:10)
    expect_null(test1)
    
    # TEST 2 : 0 length vectors
    
    test2 <- checkNumericSameLength( numeric(0), numeric(0) )
    expect_null(test2)
    
    # TEST 3 : All NA vector used
    
    test3 <- checkNumericSameLength( NA, NA )
    expect_null(test3)
    
    # TEST 4 : Numeric matrix used, this should fail as inputs must be vectors
    
    expect_error(checkNumericSameLength( matrix(0, 1, 1), 1, functionName = "try"), 
        regex = "x is not a numeric vector")
    
    # TEST 5 : Opposite order for numeric matrix
    
    expect_error(checkNumericSameLength(1, matrix(0, 1, 1), functionName = "try"), 
        regex = "y is not a numeric vector")
    
    # TEST 6 : Non-numeric vector used
    
    expect_error(checkNumericSameLength('1', 1, functionName = "try"), 
        regex = "x is not a numeric vector")
        
    # TEST 7 : Other vector is non-numeric
    
    expect_error(checkNumericSameLength(1, mtcars, functionName = "fun"), 
        regex = "y is not a numeric vector")
    
    # TEST 8 : Differing length numeric vectors
    
    expect_error(checkNumericSameLength(1, numeric(0), "a", "b", functionName = "fun"), 
        regex = "lengths of a and b do not match")

    # TEST 9 : NULL should not work!
    expect_error(checkNumericSameLength(NULL, NULL, functionName = "fun"), 
        regex = "x is not a numeric vector")

})

test_that("checkLogicalSameLength", {
    
    # TEST 1 : standard case
    
    test1 <- checkLogicalSameLength( rep(c(TRUE, FALSE), each = 5), 1:10 )
    expect_null(test1)
    
    # TEST 2 : 0 length vectors
    
    test2 <- checkLogicalSameLength( logical(0), logical(0) )
    expect_null(test2)
    
    # TEST 3 : NAs are forbidden in Logical Vectors
    
    test3 <- try(checkLogicalSameLength(NA, NA, functionName = "fun"), silent = TRUE)
    expect_equal(paste(test3),  "Error in checkLogicalSameLength(NA, NA, functionName = \"fun\") : \n  Error in fun:  missing values in x\n", msg = " || TEST3: NAs are forbidden in Logical Vectors")
    
    # TEST 4 : Logical matrix used, this should fail as inputs must be vectors
    
    expect_error(checkLogicalSameLength( matrix(TRUE, 1, 1), 1, functionName = "try"), 
        regex = "x is not a logical vector")
    
    # TEST 5 : Opposite order for logical matrix
    
    expect_error(checkLogicalSameLength(TRUE, matrix(0, 1, 1), functionName = "try"), 
        regex = "y is not a vector")
    
    # TEST 6 : Non-logical vector used
    
    expect_error(checkLogicalSameLength('TRUE', 1, functionName = "try"), 
        regex = "x is not a logical vector")
        
    # TEST 7 : Other vector is non-logical
    
    expect_error(checkLogicalSameLength(rep(TRUE, times = nrow(mtcars) * ncol(mtcars)), mtcars, functionName = "fun"), 
        regex = "y is not a vector")
    
    # TEST 8 : Differing length logical vectors
    
    expect_error(checkLogicalSameLength(logical(0), 1, "a", "b", functionName = "fun"), 
        regex = "lengths of a and b do not match")

    # TEST 9 : NULL should not work!
    expect_error(checkLogicalSameLength(NULL, NULL, functionName = "fun"), 
        regex = "x is not a logical vector")

})
