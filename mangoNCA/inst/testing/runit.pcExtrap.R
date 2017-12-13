# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 16/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# Tests the pcExtrap function 

test.pcExtrap <- function()
{
    if (!exists("pcExrap", mode = "function")) { pcExtrap <- MangoNca:::pcExtrap }
    
    # TEST 1 : standard data set, no NA or 0 values
    
    test1 <-  pcExtrap(Whole = 100, Measured = 60)
    checkEquals(test1, 40, msg = " || TEST 1 : standard data set, no NA or 0 values")
    
    # TEST 2 : Whole is 0
    
    test2 <- pcExtrap(Whole = 0, Measured = 60)
    checkEquals(test2, as.numeric(NA), msg = " || TEST 2 : Whole is 0")
    
    # TEST 3 : Measured is 0
    
    test3 <- pcExtrap(Whole = 1000, Measured = 0)
    checkEquals(test3, 100, msg = " || TEST 3 : Measured is 0")
    
    # TEST 4 : Infinite Whole
    
    test4 <- pcExtrap(Whole = Inf, Measured = 10000)
    checkEquals(test4, as.numeric(NA), msg = " || TEST 4 : Infinite Whole")
        
    # TEST 5 : 0 length vector
    
    test5 <-  try(pcExtrap(Whole = numeric(0), Measured = pi), silent = TRUE )
    checkTrue( is(test5, "try-error"), msg = " || TEST 5 : 0 length vector" )
    checkEquals(as.character(test5), "Error in checkSingleNumeric(Whole, description = \"Whole\") : \n  Error in  pcExtrap :  Whole is not a numeric of length 1.  Value is: \n", 
        msg = " || TEST 5 : 0 length vector yields correct exception message ")
        
    # TEST 6 : Complex Whole
    
    test6 <-  try(pcExtrap(Whole = 1+0i, Measured = pi), silent = TRUE )
    checkTrue( is(test6, "try-error"), msg = " || TEST 6 : Complex Whole" )
    checkEquals(as.character(test6),  "Error in checkSingleNumeric(Whole, description = \"Whole\") : \n  Error in  pcExtrap :  Whole is not a numeric of length 1.  Value is: 1+0i\n", 
        msg = " || TEST 6 : Complex Whole yields correct exception message")
                
    # TEST 7 : basic data check
    
    test7 <- try( pcExtrap(Whole = "pi", Measured = pi), silent = TRUE )
    checkTrue( is(test7, "try-error"), msg = " || TEST 7 : basic data check yields exception" )
    checkEquals( as.character(test7), "Error in checkSingleNumeric(Whole, description = \"Whole\") : \n  Error in  pcExtrap :  Whole is not a numeric of length 1.  Value is: pi\n",
        msg = " || TEST 7 : basic data check yields correct exception message"  )
}  

