# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 24/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.AUCLin <- function()
{
    if (!exists("AUCLin", mode = "function")) { AUCLin <- MangoNca:::AUCLin }
    if (!exists("trapez", mode = "function")) { trapez <- MangoNca:::trapezium }
    
    # TEST 1 : only 1 element
    
    test1 <- AUCLin( 1, 1 )
    checkEquals(test1, 0, msg = " || TEST 1 : only one concentration means only 0 is returned\n")
    
    # TEST 2 : 0 elements
    
    test2 <- AUCLin( numeric(0), numeric(0) )
    checkTrue(is.na( test2 ), msg = " || TEST 1 : 0 elements means only NA is returned\n")
    
    # TEST 3 : 2 elements, one NA present.
    
    test3 <- AUCLin( c(0, NA), 0:1 )
    checkTrue(is.na ( test3 ), msg = " || TEST 3 : NA present - only NA should be returned")
    
    # TEST 4 : "standard" data set.  Use built-in theophiline data set
    # NOTE: result is validated against trapezium function
    
    Theoph12 <- subset(Theoph, Subject == 12 )
    test4 <- AUCLin(Conc = Theoph12$conc, Time = Theoph12$Time )
    checkEquals( sum(test4), trapez(Theoph12$Time, Theoph12$conc), msg = " || TEST 4 : Total AUC is equal to AUC determined by trapezium rule"  )
    
    # TEST 5 : contrived data set
    
    test5 <- AUCLin( Time = 1:100, Conc = rep(2, 100) )
    checkEquals( test5, rep(2, 99), msg = " || TEST 5 : for contrived data, individual AUCs should all be 2\n"  )
    
    # TEST 6 : another contrived data set
    
    test6 <- AUCLin( Time = c(0, 1, 2.5, 3) , c(0, 1, 0.5, 0.25))
    checkEquals(test6, c(0.5, 1.125, 0.1875), msg = " || TEST 6 : second contrived data set\n")
    
    # TEST 7 : Missing values in various locations 
    
    test7 <- AUCLin(Time = c(1, NA, 2, NA, 3), Conc = c(NA, 1, NA, 2, NA))
    checkEquals( test7, as.numeric(c(NA, NA, NA, NA)), msg = " || TEST 7 : all values should be missing "  )
    
    # TEST 8 : single missing time
    
    test8 <- AUCLin(Time = c(1, NA, 2, 4, 5), Conc = c(1, 2, 3, 4, 5))
    checkEquals( test8, c(NA, NA, 7, 4.5 ), msg = " || TEST 8 : single missing time" )
    
    # TEST 9 : single missing concentration
    
    test9 <- AUCLin(Time = c(1, 1.5, 2, 4, 5), Conc = c(1, 2, NA, 4, 5))
    checkEquals(test9, c(0.75, NA, NA, 4.5 ), msg = " || TEST 9 : single missing concentration" )
    
    # TEST 10 : 2 elements only
    
    test10 <- AUCLin(Time =  c(1, 1000), c(1, 1.5))
    checkEquals(test10, 1248.75, msg = " || TEST 10 : 2 elements" )
    
    # TEST 11 : vectors of different length
    
    test11 <- try( AUCLin( Time = 1:10, Conc = 1:9 ), silent = TRUE)
    checkTrue(is(test11, "try-error"), msg = " || TEST 11 : exception generated due differing vector lengths "  )
    
    # TEST 12 : unordered time vector should generate an exception 
    
    test12 <- try(AUCLin( Time = 3:1, Conc = 1:3), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test12, "Error in checkOrderedVector(Time, \"Time\", \"AUCLin\") : \n  Error in AUCLin: Time is not ordered.  Actual value is 3 2 1\n" ),  msg = " || TEST 12 : exception generated due to unsorted time")
    
    # TEST 13 : unordered time vector (with missing values) should generate an exception 
    
    test13 <- try(AUCLin( Time = c(1,2, NA, 1.5), Conc = 4:1), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test13, "Error in checkOrderedVector(Time, \"Time\", \"AUCLin\") : \n  Error in AUCLin: Time is not ordered.  Actual value is 1 2 NA 1.5\n"), msg = " || TEST 13 : exception generated due to unsorted time (with missing values)")   
    
    # TEST 14 : WNL example Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    
    c14 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.73, 0.42, 0.19)
    t14 <- c(0, 0.5, 1, 1.5, 2, 4, 6, 8)
    
    test14 <- AUCLin(Conc = c14, Time = t14)
    checkEquals(round(sum(test14), digits = 5), 7.2475, msg = " || TEST 14 : WNL example Log interpolation")
  
}
