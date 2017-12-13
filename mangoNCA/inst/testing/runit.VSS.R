# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 03/02/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.VSS <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
        
    # TEST 1 : Trivial Example
    
    test1 <- VSS(MRT = 100, CL = 0.5, Safe = TRUE)
    checkEquals( test1, 50, msg = " || TEST 1 : Trivial Example\n" )

    # TEST 2 : Trivial Example unsafe
        
    test2 <- VSS(MRT = 200, CL = 0.25, Safe = FALSE)
    checkEquals( test2, 50, msg = " || TEST 2 : Trivial Example unsafe\n" )
    
    # TEST 3 : MRT is NA
    
    test3 <- try(VSS(MRT = NA, CL = 0.5, Safe = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage( test3,  "Error in checkSingleNumeric(MRT, description = \"MRT\", \"VSS\") : \n  Error in  VSS :  MRT is not a numeric of length 1.  Value is: NA\n"))
    
}

