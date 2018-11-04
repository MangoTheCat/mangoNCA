
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test checkSingleNumeric")

# tests the function checkSingleNumeric from checkdata.R

test_that("checkSingleNumeric", {

    # TEST 1 : standard case
    
    test1 <- checkSingleNumeric( 1 )
    expect_true( is.null( test1 ) )  
    
    # TEST 2 : 0 length vectors
    
    expect_error(checkSingleNumeric(numeric(0), functionName = "try"), 
        regex = "x is not a numeric of length 1.  Value is:")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleNumeric(as.numeric(NA))
    expect_true( is.null( test3 ))
    
    # TEST 4 : Numeric matrix used, this should fail as inputs must be vectors
    
    expect_error(checkSingleNumeric(matrix(1), 1, functionName = "try"), 
        regex = "1 is not a numeric of length 1")
    
    # TEST 5 : length > 1
    
    expect_error(checkSingleNumeric(1:5, functionName = "try"), 
        regex = "x is not a numeric of length 1.  Value is: 1 2 3 4 5")
    
    # TEST 6 : Non-numeric vector used
    
    expect_error(checkSingleNumeric('1', 1, functionName = "try"), 
        regex = "1 is not a numeric of length 1.  Value is:")
    
    # TEST 7 : Object and description are not length 1
    
    expect_error(checkSingleNumeric(1:2, c("multiple", "elements"), functionName = "try"), 
        regex = "elements is not a numeric of length 1")
    
    # TEST 8 : Logical Value
    
    expect_error(checkSingleNumeric(TRUE, functionName = "try"), 
        regex = "x is not a numeric of length 1")

    # TEST 9 : Missing First Argument
    
    expect_error(checkSingleNumeric(, functionName = "try"),
        regex = "x is missing with no default")

})

