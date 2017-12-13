# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function checkSingleNumeric from checkdata.R

test.checkSingleNumeric <- function()
{
    if (!exists("checkSingleNumeric", mode = "function")) { checkSingleNumeric <- MangoNca:::checkSingleNumeric }
    
    # TEST 1 : standard case
    
    test1 <- checkSingleNumeric( 1 )
    checkTrue( is.null( test1 ), msg = " || TEST1: standard check - length 1 numeric " )  
    
    # TEST 2 : 0 length vectors
    
    test2 <- try(checkSingleNumeric(numeric(0), functionName = "try"), silent = TRUE)
    checkTrue(is(test2, "try-error"), msg = " || TEST2: exception generated" )
    checkEquals(as.character(test2), "Error in checkSingleNumeric(numeric(0), functionName = \"try\") : \n  Error in  try :  x is not a numeric of length 1.  Value is: \n",
        msg = " || TEST2: length 0 vector check message")
    
    # TEST 3 : All NA vector used
    
    test3 <- checkSingleNumeric(as.numeric(NA))
    checkTrue( is.null( test3 ), msg = " || TEST3: NA vectors still numeric ")
    
    # TEST 4 : Numeric matrix used, this should fail as inputs must be vectors
    
    test4 <-try(checkSingleNumeric(matrix(1), 1, functionName = "try"), silent = TRUE) 
    checkTrue(is(test4, "try-error"), msg = " || TEST4(a): exception generated" )    
    checkEquals(as.character(test4), "Error in checkSingleNumeric(matrix(1), 1, functionName = \"try\") : \n  Error in  try :  1 is not a numeric of length 1.  Value is: \n      [,1]\n\n [1,]    1\n" ,
            msg =  " || TEST4(b) check error message" )
    
    # TEST 5 : length > 1
    
    test5 <- try(checkSingleNumeric(1:5, functionName = "try"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST5(a) exception generated" )
    checkEquals(as.character(test5), "Error in checkSingleNumeric(1:5, functionName = \"try\") : \n  Error in  try :  x is not a numeric of length 1.  Value is: 1 2 3 4 5\n",
        msg =  " || TEST5(b) check error message" )
    
    # TEST 6 : Non-numeric vector used
    
    test6 <- try(checkSingleNumeric('1', 1, functionName = "try"), silent = TRUE)
    checkTrue(is(test6, "try-error"), msg = " || TEST6 exception generated" )
    checkEquals(as.character(test6), "Error in checkSingleNumeric(\"1\", 1, functionName = \"try\") : \n  Error in  try :  1 is not a numeric of length 1.  Value is: 1\n", 
        msg =  "|| TEST6 error message as expected" )
    
    # TEST 7 : Object and description are not length 1
    
    test7 <- try(checkSingleNumeric(1:2, c("multiple", "elements"), functionName = "try"), silent = TRUE)
    checkTrue( is(test7, "try-error"), msg = " || TEST 7(a) : Other vector is not length 1" )
    checkEquals(as.character(test7), "Error in checkSingleNumeric(1:2, c(\"multiple\", \"elements\"), functionName = \"try\") : \n  Error in  try :  multiple is not a numeric of length 1.  Value is: 1 2Error in  try :  elements is not a numeric of length 1.  Value is: 1 2\n",
            msg =  "|| TEST 7(b) : Object and description are not length 1error message as expected" )
    
    # TEST 8 : Logical Value
    
    test8 <- try( checkSingleNumeric(TRUE, functionName = "try"), silent = TRUE)
    checkTrue(is(test8, "try-error"), msg = " || TEST 8 : Logical Value" )
    checkEquals(as.character(test8), "Error in checkSingleNumeric(TRUE, functionName = \"try\") : \n  Error in  try :  x is not a numeric of length 1.  Value is: TRUE\n", 
            msg =  "|| TEST 8 : Logical Value error message as expected" )

    # TEST 9 : Missing First Argument
    
    test9 <- try( checkSingleNumeric(, functionName = "try"), silent = TRUE)
    checkTrue(is(test9, "try-error"), msg = " || TEST 9(a) Missing First Argument" )
    checkEquals(as.character(test9), 
            "Error in checkSingleNumeric(, functionName = \"try\") : \n  Error in try: x is missing with no default\n" , msg =  " || TEST 9(a) Missing First Argument error message as expected" )

}

