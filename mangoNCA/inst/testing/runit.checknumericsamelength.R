# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 17/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function checkNumericSameLength from checkdata.R
# TODO: it is difficult to test that the function name of the calling function is automatically detected in 
# checkNumericSameLength, as running the test suite from ant, running the test suite from inside the R gui 
# (via runRNCATests), and running the test function on its own, all seem to generate different results

test.checkNumericSameLength <- function()
{
    if (!exists("checkNumericSameLength", mode = "function")) { checkNumericSameLength <- MangoNca:::checkNumericSameLength }
    
    # TEST 1 : standard case
    
    test1 <- checkNumericSameLength( 1:10, 1:10 )
    checkTrue( is.null( test1 ), msg = " || TEST1: standard check - 2 length 10 numerics " )  
    
    # TEST 2 : 0 length vectors
    
    test2 <- checkNumericSameLength( numeric(0), numeric(0) )
    checkTrue( is.null( test2 ), msg = " || TEST2: length vectors checked correctly" )
    
    # TEST 3 : All NA vector used
    
    test3 <- checkNumericSameLength( NA, NA )
    checkTrue( is.null( test3 ), msg = " || TEST3: NA vectors still numeric ")
    
    # TEST 4 : Numeric matrix used, this should fail as inputs must be vectors
    
    test4 <-try( checkNumericSameLength( matrix(0, 1, 1), 1, functionName = "try"), silent = TRUE) 
  
    checkTrue(is(test4, "try-error"), msg = " || TEST4(a): exception generated" )    
    checkEquals(as.character(test4), "Error in checkNumericSameLength(matrix(0, 1, 1), 1, functionName = \"try\") : \n  Error in try: x is not a numeric vector\n" ,
            msg =  " || TEST4(b) error message as expected" )
    
    # TEST 5 : Opposite order for numeric matrix
    
    test5 <- try( checkNumericSameLength( 1, matrix(0, 1, 1),functionName = "try"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST5(a) exception generated" )
    checkEquals(as.character(test5), "Error in checkNumericSameLength(1, matrix(0, 1, 1), functionName = \"try\") : \n  Error in try: y is not a numeric vector\n" ,
            msg =  " || TEST5(b) error message as expected" )
    
    # TEST 6 : Non-numeric vector used
    
    test6 <- try( checkNumericSameLength('1', 1, functionName = "try"), silent = TRUE)
    checkTrue(is(test6, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test6), "Error in checkNumericSameLength(\"1\", 1, functionName = \"try\") : \n  Error in try: x is not a numeric vector\n" , msg =  "|| error message as expected" )
        
    # TEST 7 : Other vector is non-numeric
    
    test7 <- try( checkNumericSameLength(1, mtcars, functionName = "fun"), silent = TRUE)
    checkTrue(is(test7, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test7), "Error in checkNumericSameLength(1, mtcars, functionName = \"fun\") : \n  Error in fun: y is not a numeric vector\n" ,
            msg =  "|| error message as expected" )
    
    # TEST 8 : Differing length numeric vectors
    
    test8 <- try( checkNumericSameLength(1, numeric(0), "a", "b", functionName = "fun"), silent = TRUE)
    checkTrue(is(test8, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test8), "Error in checkNumericSameLength(1, numeric(0), \"a\", \"b\", functionName = \"fun\") : \n  Error in fun:  lengths of a and b do not match\n" , 
            msg =  "|| error message as expected" )

    # TEST 9 : NULL should not work!
    test9 <- try( checkNumericSameLength(NULL, NULL, functionName = "fun"), silent = TRUE)
    checkTrue(is(test9, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test9), 
            "Error in checkNumericSameLength(NULL, NULL, functionName = \"fun\") : \n  Error in fun: x is not a numeric vector\n" , msg =  "|| error message as expected" )

}

test.checkLogicalSameLength <- function()
{
    if (!exists("checkLogicalSameLength", mode = "function")) { checkLogicalSameLength <- MangoNca:::checkLogicalSameLength }
    
    # TEST 1 : standard case
    
    test1 <- checkLogicalSameLength( rep(c(TRUE, FALSE), each = 5), 1:10 )
    checkTrue( is.null( test1 ), msg = " || TEST1: standard check - 2 length 10 logicals " )  
    
    # TEST 2 : 0 length vectors
    
    test2 <- checkLogicalSameLength( logical(0), logical(0) )
    checkTrue( is.null( test2 ), msg = " || TEST2: length vectors checked correctly" )
    
    # TEST 3 : NAs are forbidden in Logical Vectors
    
    test3 <- try(checkLogicalSameLength(NA, NA, functionName = "fun"), silent = TRUE)
    checkEquals(paste(test3),  "Error in checkLogicalSameLength(NA, NA, functionName = \"fun\") : \n  Error in fun:  missing values in x\n", msg = " || TEST3: NAs are forbidden in Logical Vectors")
    
    # TEST 4 : Logical matrix used, this should fail as inputs must be vectors
    
    test4 <-try( checkLogicalSameLength( matrix(TRUE, 1, 1), 1, functionName = "try"), silent = TRUE) 
  
    checkTrue(is(test4, "try-error"), msg = " || TEST4(a): exception generated" )    
    checkEquals(paste(test4), "Error in checkLogicalSameLength(matrix(TRUE, 1, 1), 1, functionName = \"try\") : \n  Error in try: x is not a logical vector\n" ,
            msg =  " || TEST4(b) error message as expected" )
    
    # TEST 5 : Opposite order for logical matrix
    
    test5 <- try( checkLogicalSameLength( TRUE, matrix(0, 1, 1), functionName = "try"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST5(a) exception generated" )
    checkEquals(paste(test5), "Error in checkLogicalSameLength(TRUE, matrix(0, 1, 1), functionName = \"try\") : \n  Error in try: y is not a vector\n" ,
            msg =  " || TEST5(b) error message as expected" )
    
    # TEST 6 : Non-logical vector used
    
    test6 <- try( checkLogicalSameLength('TRUE', 1, functionName = "try"), silent = TRUE)
    checkTrue(is(test6, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test6), "Error in checkLogicalSameLength(\"TRUE\", 1, functionName = \"try\") : \n  Error in try: x is not a logical vector\n" , msg =  "|| error message as expected" )
        
    # TEST 7 : Other vector is non-logical
    
    test7 <- try( checkLogicalSameLength(rep(TRUE, times = nrow(mtcars) * ncol(mtcars)), mtcars, functionName = "fun"), silent = TRUE)
    checkTrue(is(test7, "try-error"), msg = " || exception generated" )
    checkEquals(paste(test7), "Error in checkLogicalSameLength(rep(TRUE, times = nrow(mtcars) * ncol(mtcars)),  : \n  Error in fun: y is not a vector\n" ,
            msg =  "|| error message as expected" )
    
    # TEST 8 : Differing length logical vectors
    
    test8 <- try( checkLogicalSameLength(logical(0), 1, "a", "b", functionName = "fun"), silent = TRUE)
    checkTrue(is(test8, "try-error"), msg = " || exception generated" )
    checkEquals(paste(test8), "Error in checkLogicalSameLength(logical(0), 1, \"a\", \"b\", functionName = \"fun\") : \n  Error in fun:  lengths of a and b do not match\n" , 
            msg =  "|| error message as expected" )

    # TEST 9 : NULL should not work!
    test9 <- try( checkLogicalSameLength(NULL, NULL, functionName = "fun"), silent = TRUE)
    checkTrue(is(test9, "try-error"), msg = " || exception generated" )
    checkEquals(as.character(test9), 
            "Error in checkLogicalSameLength(NULL, NULL, functionName = \"fun\") : \n  Error in fun: x is not a logical vector\n" , msg =  "|| error message as expected" )

}
