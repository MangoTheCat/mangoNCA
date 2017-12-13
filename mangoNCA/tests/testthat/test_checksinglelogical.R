
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("checkSingleLogical")

# tests the function checkSingleLogical from checkdata.R

test_that("checkSingleLogical", {
    
    # TEST 1 : standard case
    
    test1 <- checkSingleLogical( FALSE )
    expect_true( is.null( test1 ) )  
    
    # TEST 2 : 0 length vectors
    
    expect_error(checkSingleLogical(logical(0), functionName = "try"), 
        regex = "x is not a logical of length 1")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleLogical(as.logical(NA))
    expect_true( is.null( test3 ))
    
    # TEST 4 : Logical matrix used, this should fail as inputs must be vectors
    
    expect_error(checkSingleLogical(matrix(TRUE), functionName = "try"), 
        regex = "x is not a logical of length 1")
    
    # TEST 5 : length > 1
    
    expect_error(checkSingleLogical(c(TRUE, FALSE), functionName = "try"), 
        regex = "x is not a logical of length 1")
    
    # TEST 6 : Non-Logical vector used
    
    expect_error(checkSingleLogical('TRUE', functionName = "try"), 
        regex = "x is not a logical of length 1")
    
    # TEST 7 : Object and description are not length 1
    
    test7 <- checkSingleLogical(F, "F", functionName = "try")
    expect_true( is.null( test7 ) )  
    
    # TEST 8 : Logical Value
    
    expect_error(checkSingleLogical(0, functionName = "try"), 
        regex = "x is not a logical of length 1.  Value is: 0")

    # TEST 9 : Missing First Argument
    
    expect_error(checkSingleLogical(, functionName = "try"), 
        regex = "x is missing with no default")

})
