# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 24/05/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function checkSingleCharacter from checkdata.R

test.checkSingleCharacter <- function()
{
    if (!exists("checkSingleCharacter", mode = "function")) { checkSingleCharacter <- MangoNca:::checkSingleCharacter }
    
    # TEST 1 : standard case
    
    test1 <- checkSingleCharacter("text")
    checkTrue( is.null( test1 ), msg = " || TEST1: standard check - length 1 Character " )  
    
    # TEST 2 : 0 length vectors
    
    test2 <- try(checkSingleCharacter(character(0), functionName = "try"), silent = TRUE)
    checkTrue(is(test2, "try-error"), msg = " || TEST2: exception generated" )
    checkEquals(as.character(test2), 
        "Error in checkSingleCharacter(character(0), functionName = \"try\") : \n  Error in  try :  x is not a character of length 1.  Value is: \n",
        msg = " || TEST2: length 0 vector check message")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleCharacter(as.character(NA))
    checkTrue( is.null( test3 ), msg = " || TEST3: NA vectors still Character ")
    
    # TEST 4 : Character matrix used, this should fail as inputs must be vectors
    
    test4 <-try(checkSingleCharacter(matrix("a"), functionName = "try"), silent = TRUE) 
    checkTrue(is(test4, "try-error"), msg = " || TEST4(a): exception generated" )    
    checkEquals(as.character(test4), 
        "Error in checkSingleCharacter(matrix(\"a\"), functionName = \"try\") : \n  Error in  try :  x is not a character of length 1.  Value is: \n      [,1]\n\n [1,] \"a\" \n",
        msg =  " || TEST4(b) check error message" )
    
    # TEST 5 : length > 1
    
    test5 <- try(checkSingleCharacter(LETTERS[1:2], functionName = "try"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST5(a) exception generated" )
    checkEquals(as.character(test5), 
        "Error in checkSingleCharacter(LETTERS[1:2], functionName = \"try\") : \n  Error in  try :  x is not a character of length 1.  Value is: A B\n",
        msg =  " || TEST5(b) check error message" )
    
    # TEST 6 : Non-Character vector used
    
    test6 <- try(checkSingleCharacter(TRUE, functionName = "try"), silent = TRUE)
    checkTrue(is(test6, "try-error"), msg = " || TEST 6(a) exception generated" )
    checkEquals(as.character(test6), 
        "Error in checkSingleCharacter(TRUE, functionName = \"try\") : \n  Error in  try :  x is not a character of length 1.  Value is: TRUE\n", 
        msg =  "|| TEST 6(b) error message as expected" )
    
    # TEST 7 : Object and description are not length 1
    
    test7 <- checkSingleCharacter("F", "F", functionName = "try")
    checkTrue( is.null( test7 ), msg = " || TEST 7: F is character " )  
    
    # TEST 8 : Character Value
    
    test8 <- try( checkSingleCharacter(0, functionName = "try"), silent = TRUE)
    checkTrue(is(test8, "try-error"), msg = " || TEST 8 : Character Value" )
    checkEquals(as.character(test8), 
        "Error in checkSingleCharacter(0, functionName = \"try\") : \n  Error in  try :  x is not a character of length 1.  Value is: 0\n", 
        msg =  "|| TEST 8 : Numeric Value error message as expected" )

    # TEST 9 : Missing First Argument
    
    test9 <- try( checkSingleCharacter(, functionName = "try"), silent = TRUE)
    checkTrue(is(test9, "try-error"), msg = " || TEST 9(a) Missing First Argument" )
    checkEquals(as.character(test9), 
        "Error in checkSingleCharacter(, functionName = \"try\") : \n  Error in try: x is missing with no default\n", 
        msg =  " || TEST 9(a) Missing First Argument error message as expected" )

}

