# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 28/05/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.AUCLog <- function()
{
    if (!exists("AUCLog", mode = "function")) { AUCLog <- MangoNca:::AUCLog }
    
    rdiff <- function(x) { x[-length(x)] - x[-1] }
    
    # TEST 1 : only 1 element
    
    test1 <- AUCLog(1, 1)
    checkEquals(test1, 0, msg = " || TEST 1 : only one concentration means only 0 is returned\n")
    
    # TEST 2 : 0 elements
    
    test2 <- AUCLog(numeric(0), numeric(0))
    checkTrue(is.na(test2), msg = " || TEST 1 : 0 elements means only NA is returned\n")
    
    # TEST 3 : 2 elements, one NA present.
    
    test3 <- AUCLog(c(0, NA), 0:1)
    checkTrue(is.na (test3), msg = " || TEST 3 : NA present - only NA should be returned")
    
    # TEST 4 : "standard" data set,  using built-in theophiline data set
    
    Theoph12 <- subset(Theoph, Subject == 12 )
    test4 <- AUCLog(Conc = Theoph12$conc, Time = Theoph12$Time)
    checkEquals(sum(test4), 114.85723004832, tol = 1e-8, msg = " || TEST 4 : Total AUC is equal to number"  )
    
    # TEST 5 : contrived data set
    
    test5 <- AUCLog(Time = 1:100, Conc = rep(2, 100))
    checkEquals(test5, rep(2, 99), msg = " || TEST 5 : for contrived data, individual AUCs should all be 2\n"  )
    
    # TEST 6 : exp vals cf integration
    
    test6 <- AUCLog(Time = (0:5)^2, Conc = 25 * exp(-(1/pi) * (0:5)^2))
    checkEquals(test6, rdiff(25 / (1/pi) * exp(-(1/pi) * (0:5)^2)), msg = " || TEST 6 : exp vals cf integration\n")
    
    # TEST 7 : Missing values in various locations 
    
    test7 <- AUCLog( Time = c(1, NA, 2, NA, 3), Conc = c( NA, 1, NA, 2, NA  ) )
    checkEquals( test7, as.numeric(c(NA, NA, NA, NA)), msg = " || TEST 7 : all values should be missing "  )
    
    # TEST 8 : Single missing time 
    
    test8 <- AUCLog(Time = c(1, NA, 2, 4, 5), Conc = c( 1, 2, 3, 4, 5))
    checkEquals(round(test8, 2), c(NA, NA, 6.95, 4.48), msg = " || TEST 8 : Single missing time")
    
    # TEST 9 : similar to the above, but missing concentrations
    
    test9 <- AUCLog( Time = c(1, 1.5, 2, 4, 5), Conc = c( 1, 2, NA, 4, 5))
    checkEquals(round(test9, 2), c(0.72, NA, NA, 4.48 ), msg = " || TEST 9 : single missing concentration " )
    
    # TEST 10 : 2 elements only
    
    test10 <- AUCLog(Time =  c(1, 1000), c(1, 1.5))
    checkEquals(round(test10, 5), 1231.91857, msg = " || TEST 10 : 2 elements " )
    
    # TEST 11 : vectors of different length
    
    test11 <- try( AUCLog(Time = 1:10, Conc = 1:9), silent = TRUE)
    checkTrue(is(test11, "try-error"), msg = " || TEST 11 : exception generated due differing vector lengths "  )
    
    # TEST 12 : unordered time vector should generate an exception 
    
    test12 <- try(AUCLog(Time = 3:1, Conc = 1:3), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test12, "Error in checkOrderedVector(Time, \"Time\", functionName = \"AUCLog\") : \n  Error in AUCLog: Time is not ordered.  Actual value is 3 2 1\n" ),  
        msg = " || TEST 12 : exception generated due to unsorted time")
    
    # TEST 13 : unordered time vector (with missing values) should generate an exception 
    
    test13 <- try(AUCLog( Time = c(1, 2, NA, 1.5), Conc = 4:1), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test13, "Error in checkOrderedVector(Time, \"Time\", functionName = \"AUCLog\") : \n  Error in AUCLog: Time is not ordered.  Actual value is 1 2 NA 1.5\n"), 
        msg = " || TEST 13 : exception generated due to unsorted time (with missing values)")   
    
    # TEST 14 : WNL example Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    
    #c14 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.73, 0.42, 0.19)
    #t14 <- c(0, 0.5, 1, 1.5, 2, 4, 6, 8)
    
    #test14 <- AUCLog(Conc = c14, Time = t14)
    #checkEquals(round(sum(test14), digits = 5), 6.8010, msg = " || TEST 14 : WNL example Log interpolation")
    
}
