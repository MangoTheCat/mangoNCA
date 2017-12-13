# SVN revision: $Rev: 23459 $
# Date of last change: $LastChangedDate: 2010-12-07 11:23:47 +0000 (Tue, 07 Dec 2010) $
# Last changed by: $LastChangedBy: fgochez $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests the checkOrderedVector function in checkdata.R

test.checkOrderedVector <- function() 
{
    if (!exists("checkOrderedVector", mode = "function")) { checkOrderedVector <- MangoNca:::checkOrderedVector }
    
    # TEST 1 : base case - ordered vector, returns NULL
    
    test1 <- checkOrderedVector( 1:5 )
    checkTrue(is.null(test1), msg = " || TEST 1 : base case - ordered vector, returns NULL")
    
    # TEST 2 : ordered vector, with NAs
    
    test2 <- checkOrderedVector(c(NA, 1)) 
    checkTrue(is.null(test2), msg = " || TEST 2 : ordered vector, with NAs")
    
    # TEST 3 : length 0 vector is ordered
    
    test3 <- checkOrderedVector(numeric(0))
    checkTrue(is.null(test3), msg = " || TEST 3 : length 0 vector is ordered")
    
    # TEST 4 : unordered vector with no NA
    
    test4 <- try( checkOrderedVector(c(1.00001, 1), functionName = "func"), silent = TRUE )
    checkEquals(as.character(test4),
            "Error in checkOrderedVector(c(1.00001, 1), functionName = \"func\") : \n  Error in func: x is not ordered.  Actual value is 1.00001 1\n",  
            msg = "  || unordered vector with no NA" )
    
    # TEST 5 : unordered vector with NA
    
    test5 <- try( checkOrderedVector(c(NA, 0, 1.00001, NA, 1), functionName = "func", description = "y"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST 5 : exception generated since vector is unordered")
    checkEquals(as.character(test5),
            "Error in checkOrderedVector(c(NA, 0, 1.00001, NA, 1), functionName = \"func\",  : \n  Error in func: y is not ordered.  Actual value is NA 0 1.00001 NA 1\n", 
            msg = "  || unordered vector with no NA" )
    
    
    # TEST 6 : non-vector
    
    test6 <- try( checkOrderedVector( Theoph, functionName = "func", description = "Theoph"), silent = TRUE )
    checkTrue(is(test6, "try-error"), msg = " || TEST 6 : exception generated since vector is unordered")
    checkEquals(as.character(test6),
            "Error in checkOrderedVector(Theoph, functionName = \"func\", description = \"Theoph\") : \n  Error in func: Theoph is not a vector\n", 
            msg = "  || unordered vector with no NA" )
    
}
