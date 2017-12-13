# SVN revision: $Rev: 22683 $
# Date of last change: $LastChangedDate: 2010-11-16 15:15:02 +0000 (Tue, 16 Nov 2010) $
# Last changed by: $LastChangedBy: fgochez $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# Tests the ClastTlast function 

test.ClastTlast <- function()
{
    # use theophiline data
    # Theoph1 will be a data.frame with the Theoph data for subject 1 
    
    Theoph1 <- subset(  Theoph, Subject == 1)
    
    # TEST 1 : standard data set, no NA or 0 values
    
    test1 <-  ClastTlast( Conc = Theoph1$conc, Time = Theoph1$Time )
    checkEquals( test1, list(clast = 3.28, tlast = 24.37, index = 11), msg = " || TEST 1 : standard data set, no NA or 0 values" )
    
    # TEST 2 : data set with mix of 0s and NAs, and only one measurable concentration
    
    test2 <- ClastTlast(Conc = c(0, 0, NA, 0, 0, 1, NA, NA, 0, 0, NA), Time = 1:11 )
    checkEquals(test2, list(clast = 1, tlast = 6, index = 6), msg = " || TEST 2 : data set with mix of 0s and NAs, and only one measurable concentration")
    
    # TEST 3 : data with mix of 0s, NAs, and several measurable concentrations
    
    test3 <- ClastTlast(Conc = c(0, 0, NA, 0, 0, 1, NA, 2, 0, 0, NA), Time = 1:11 )
    checkEquals(test3, list(clast = 2, tlast = 8, index = 8), msg = " || TEST 3 : data with mix of 0s, NAs, and several measurable concentrations")
    
    # TEST 4 : only first concentration is measurable
    
    test4 <- ClastTlast(Conc = c(1, 0, NA, 0, 0, 0), Time = 1:6 )
    checkEquals(test4, list(clast = 1, tlast = 1, index = 1), msg = " || TEST 4 : only first concentration is measurable")
    
    # TEST 5 : only last concentration is measurable
    
    test5 <- ClastTlast(Conc = c(NA, 0, NA, 0, 0, 1), Time = 1:6 )
    checkEquals(test5, list(clast = 1, tlast = 6, index = 6), msg = " || TEST 5 : only last concentration is measurable")
    
    # TEST 6 : no measurable concentrations
    
    naResult <- list(clast = as.numeric(NA), tlast = as.numeric(NA), index = as.numeric(NA))
    test6 <- ClastTlast(Conc = c(NA, 0, NA, 0, 0, 0), Time = 1:6 )
    checkEquals(test6, naResult, msg = " || TEST 6 : no measurable concentrations")
        
    # TEST 7 : 0 length vectors
    
    test7 <-  ClastTlast(Conc = numeric(0), Time = numeric(0))
    checkEquals(test7, naResult, msg = " || TEST 7 : 0 length vectors")
    
    # TEST 8 : concentrations add up to 0
    
    test8 <-  ClastTlast(Conc = c(0, NA, 1, NA, -1), Time = 6:10)
    checkEquals(test8, naResult, msg = " || TEST 8 : concentrations add up to 0")
                
    # TEST 9 : basic data check
    
    test9 <- try( ClastTlast( Conc = 1, Time = "b" ), silent = TRUE )
    checkTrue( is(test9, "try-error"), msg = " || TEST 9 : non-numeric time yields exception" )
    checkEquals( as.character(test9), "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\") : \n  Error in ClastTlast: Time is not a numeric vector\n",
                msg = " || TEST 9 : non-numeric time yields correct exception message "  )
    
    # TEST 10 : unsorted time generates exception
    test10 <- try( ClastTlast( Conc = 1:2, Time = 2:1 ), silent = TRUE )
    checkTrue(is(test10, "try-error"), msg = " || TEST 10 : exception generated due to unsorted time")
    checkEquals(as.character(test10), "Error in ClastTlast(Conc = 1:2, Time = 2:1) : Time vector is not sorted!\n", msg = " || TEST 10: correct exception message generated")
    
}  

