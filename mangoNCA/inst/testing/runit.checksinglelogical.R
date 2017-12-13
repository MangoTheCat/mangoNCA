# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 03/02/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function checkSingleLogical from checkdata.R

test.checkSingleLogical <- function()
{
    if (!exists("checkSingleLogical", mode = "function")) { checkSingleLogical <- MangoNca:::checkSingleLogical }
    
    # TEST 1 : standard case
    
    test1 <- checkSingleLogical( FALSE )
    checkTrue( is.null( test1 ), msg = " || TEST1: standard check - length 1 Logical " )  
    
    # TEST 2 : 0 length vectors
    
    test2 <- try(checkSingleLogical(logical(0), functionName = "try"), silent = TRUE)
    checkTrue(is(test2, "try-error"), msg = " || TEST2: exception generated" )
    checkEquals(as.character(test2), 
        "Error in checkSingleLogical(logical(0), functionName = \"try\") : \n  Error in  try :  x is not a logical of length 1.  Value is: \n",
        msg = " || TEST2: length 0 vector check message")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleLogical(as.logical(NA))
    checkTrue( is.null( test3 ), msg = " || TEST3: NA vectors still Logical ")
    
    # TEST 4 : Logical matrix used, this should fail as inputs must be vectors
    
    test4 <-try(checkSingleLogical(matrix(TRUE), functionName = "try"), silent = TRUE) 
    checkTrue(is(test4, "try-error"), msg = " || TEST4(a): exception generated" )    
    checkEquals(as.character(test4), 
        "Error in checkSingleLogical(matrix(TRUE), functionName = \"try\") : \n  Error in  try :  x is not a logical of length 1.  Value is: \n      [,1]\n\n [1,] TRUE\n" ,
        msg =  " || TEST4(b) check error message" )
    
    # TEST 5 : length > 1
    
    test5 <- try(checkSingleLogical(c(TRUE, FALSE), functionName = "try"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST5(a) exception generated" )
    checkEquals(as.character(test5), 
        "Error in checkSingleLogical(c(TRUE, FALSE), functionName = \"try\") : \n  Error in  try :  x is not a logical of length 1.  Value is: TRUE FALSE\n",
        msg =  " || TEST5(b) check error message" )
    
    # TEST 6 : Non-Logical vector used
    
    test6 <- try(checkSingleLogical('TRUE', functionName = "try"), silent = TRUE)
    checkTrue(is(test6, "try-error"), msg = " || TEST 6(a) exception generated" )
    checkEquals(as.character(test6), 
        "Error in checkSingleLogical(\"TRUE\", functionName = \"try\") : \n  Error in  try :  x is not a logical of length 1.  Value is: TRUE\n", 
        msg =  "|| TEST 6(b) error message as expected" )
    
    # TEST 7 : Object and description are not length 1
    
    test7 <- checkSingleLogical(F, "F", functionName = "try")
    checkTrue( is.null( test7 ), msg = " || TEST 7: shortcut F is logical " )  
    
    # TEST 8 : Logical Value
    
    test8 <- try( checkSingleLogical(0, functionName = "try"), silent = TRUE)
    checkTrue(is(test8, "try-error"), msg = " || TEST 8 : Logical Value" )
    checkEquals(as.character(test8), 
        "Error in checkSingleLogical(0, functionName = \"try\") : \n  Error in  try :  x is not a logical of length 1.  Value is: 0\n", 
        msg =  "|| TEST 8 : Numeric Value error message as expected" )

    # TEST 9 : Missing First Argument
    
    test9 <- try( checkSingleLogical(, functionName = "try"), silent = TRUE)
    checkTrue(is(test9, "try-error"), msg = " || TEST 9(a) Missing First Argument" )
    checkEquals(as.character(test9), 
        "Error in checkSingleLogical(, functionName = \"try\") : \n  Error in try: x is missing with no default\n", 
        msg =  " || TEST 9(a) Missing First Argument error message as expected" )

}

