# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 04/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.VZ <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
        
    # TEST 1 : Trivial Example
    
    test1 <- VZ(lambdaz = 0.001, AUCInf = 1000, Dose = 6, Safe = TRUE)
    checkEquals( test1, 6, msg = " || TEST 1 : Trivial Example\n" )

    # TEST 2 : Trivial Example unsafe
        
    test2 <- VZ(lambdaz = 0.002, AUCInf = 2000, Dose = 40, Safe = FALSE)
    checkEquals( test2, 10, msg = " || TEST 2 : Trivial Example unsafe\n" )
    
    # TEST 3 : lambdaz is NA
    
    test3 <- try(VZ(lambdaz = NA, AUCInf = 1000, Dose = 10, Safe = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage( test3,  "Error in checkSingleNumeric(lambdaz, description = \"lambdaz\", functionName = \"VZ\") : \n  Error in  VZ :  lambdaz is not a numeric of length 1.  Value is: NA\n"))
    
}

