# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 18/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests the checkPeakTrough function in checkdata.R

test.checkPeakTrough <- function() 
{
    if (!exists("checkPeakTrough", mode = "function")) { checkPeakTrough <- MangoNca:::checkPeakTrough }
    
    # TEST 1 : base case - ordered vector, returns NULL
    
    test1 <- checkPeakTrough( 1:5 )
    checkTrue(is.null(test1), msg = " || TEST 1 : base case - ordered vector, returns NULL")
    
    # TEST 2 : ordered vector, with NAs
    
    test2 <- checkPeakTrough(c(NA, 1)) 
    checkTrue(is.null(test2), msg = " || TEST 2 : ordered vector, with NAs")
    
    # TEST 3 : length 0 vector is ordered
    
    test3 <- checkPeakTrough(numeric(0))
    checkTrue(is.null(test3), msg = " || TEST 3 : length 0 vector is ordered")
    
    # TEST 4 : repeated PEAKCODE with no NA
    
    test4 <- try( checkPeakTrough(c(1, 0, 2, 0, 0, 2), functionName = "func"), silent = TRUE )
    checkEquals(as.character(test4),
            "Error in checkPeakTrough(c(1, 0, 2, 0, 0, 2), functionName = \"func\") : \n  Error in func: PeakTrough is miscoded.  Actual value is 1 0 2 0 0 2\n",  
            msg = "  || repeated PEAKCODE with no NA" )
    
    # TEST 5 : repeated TROUGHCODE with NA
    
    test5 <- try( checkPeakTrough(c(1, NA, 1, 2, 0, NA, 0), functionName = "func"), silent = TRUE )
    checkTrue(is(test5, "try-error"), msg = " || TEST 5 : repeated TROUGHCODE with NA")
    checkEquals(as.character(test5),
            "Error in checkPeakTrough(c(1, NA, 1, 2, 0, NA, 0), functionName = \"func\") : \n  Error in func: PeakTrough is miscoded.  Actual value is 1 NA 1 2 0 NA 0\n", 
            msg = "  || repeated TROUGHCODE with NA error message" )
    
    
    # TEST 6 : non-vector
    
    test6 <- try( checkPeakTrough( Theoph["Time"], functionName = "func"), silent = TRUE )
    checkTrue(is(test6, "try-error"), msg = " || TEST 6 : exception generated since non-vector")
    checkEquals(as.character(test6), "Error in checkPeakTrough(Theoph[\"Time\"], functionName = \"func\") : \n  Error in func: PeakTrough, is not a vector\n", 
            msg = "  || unordered vector with no NA" )
    
}
