# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 25/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# Tests the clearance function 

test.clearance <- function()
{
    
    # TEST 1 : Nonzero AUCInf
    
    test1 <-  clearance(AUCInf = 20000, Dose = 20 * 1000)
    checkEquals( test1, 1.0, msg = " || TEST 1 : Nonzero AUCInf" )
    
    # TEST 2 : AUCInf NA
    
    test2 <- clearance(AUCInf = as.numeric(NA), Dose = 15 * 1000)
    checkEquals(test2, as.numeric(NA), msg = " || TEST 2 : AUCInf NA")
    
    # TEST 3 : negative AUCInf
    
    test3 <- clearance(AUCInf = -50000, Dose = 20 * 1000)
    checkEquals(test3, as.numeric(NA), msg = " || TEST 3 : negative AUCInf is NA")
    
    # TEST 4a : Infinite AUC
    
    test4a <- clearance(AUCInf = Inf, Dose = 1000 * 1000)
    checkEquals(test4a, as.numeric(0), msg = " || TEST 4a : Infinite AUCInf is 0")
    
    # TEST 4b : Infinite AUC
    
    test4b <- clearance(AUCInf = -Inf, Dose = 1000 * 1000)
    checkEquals(test4b, as.numeric(NA), msg = " || TEST 4 : negative Inf AUCInf is NA")
        
    # TEST 5 : 0 length vector
    
    test5 <-  try(clearance(AUCInf = numeric(0), Dose = pi), silent = TRUE )
    checkTrue( is(test5, "try-error"), msg = " || TEST 5 : 0 length vector" )
    checkEquals(as.character(test5), "Error in checkSingleNumeric(AUCInf, description = \"AUCInf\", functionName = \"clearance\") : \n  Error in  clearance :  AUCInf is not a numeric of length 1.  Value is: \n", 
        msg = " || TEST 5 : 0 length vector yields correct exception message ")
        
    # TEST 6 : AUC is zero
    
    test6 <-  try(clearance(AUCInf = FALSE, Dose = 1), silent = TRUE )
    checkTrue( is(test6, "try-error"), msg = " || TEST 6 : AUC is zero" )
    checkEquals(as.character(test6),  "Error in checkSingleNumeric(AUCInf, description = \"AUCInf\", functionName = \"clearance\") : \n  Error in  clearance :  AUCInf is not a numeric of length 1.  Value is: FALSE\n", 
        msg = " || TEST 6 : 0 AUC is zero yields correct exception message")
                
    # TEST 7 : basic data check
    
    test7 <- try( clearance(AUCInf = "BLQ", Dose = 1), silent = TRUE )
    checkTrue( is(test7, "try-error"), msg = " || TEST 7 : basic data check yields exception" )
    checkEquals( as.character(test7), "Error in checkSingleNumeric(AUCInf, description = \"AUCInf\", functionName = \"clearance\") : \n  Error in  clearance :  AUCInf is not a numeric of length 1.  Value is: BLQ\n",
        msg = " || TEST 7 : basic data check yields correct exception message"  )
}  

