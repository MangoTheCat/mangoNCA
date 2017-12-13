
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("checkSingleCharacter")

# tests the function checkSingleCharacter from checkdata.R

test_that("checkSingleCharacter", {
    
    # TEST 1 : standard case
    
    test1 <- checkSingleCharacter("text")
    expect_true( is.null( test1 ) )  
    
    # TEST 2 : 0 length vectors
    
    expect_error(checkSingleCharacter(character(0), functionName = "try"), 
        regex = "x is not a character of length 1.  Value is:")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleCharacter(as.character(NA))
    expect_true( is.null( test3 ))
    
    # TEST 4 : Character matrix used, this should fail as inputs must be vectors
    
    expect_error(checkSingleCharacter(matrix("a"), functionName = "try"), 
        regex = "x is not a character of length 1")
    
    # TEST 5 : length > 1
    
    expect_error(checkSingleCharacter(LETTERS[1:2], functionName = "try"), 
        regex = "x is not a character of length 1")
    
    # TEST 6 : Non-Character vector used
    
    expect_error(checkSingleCharacter(TRUE, functionName = "try"), 
        regex = "x is not a character of length 1")
    
    # TEST 7 : Object and description are not length 1
    
    test7 <- checkSingleCharacter("F", "F", functionName = "try")
    expect_true( is.null( test7 ) )  
    
    # TEST 8 : Character Value
    
    expect_error(checkSingleCharacter(0, functionName = "try"), 
        regex = "x is not a character of length 1")

    # TEST 9 : Missing First Argument
    
    expect_error(checkSingleCharacter(, functionName = "try"), 
        regex = "x is missing with no default")

})
