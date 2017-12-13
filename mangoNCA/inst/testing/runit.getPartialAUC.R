# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 22/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests getPartialAUC function from getPartialAUC.R

test.getPartialAUC <- function()
{
    # simple data
    
    t0 <- 0:9
    c0 <- 10:1
    
    # Test Data
    
    t1 <- c(0, 1, 1.5, 2, 3.08, 5, 8.92, 22.25, 48.67, 168.25, 263.45, 336.25)

    c1 <- c(37.155, 121.957, 136.175, 112.603, 127.944, 107.873, 153.631, 
        116.352, 123.991, 42.004, 28.211, 26.0)
    
    # TEST 1 - 3 : endTime is an element of time - normal and boundary cases
    
    test1 <- getPartialAUC(Conc = c0, Time = t0, startTime = 0, endTime = 9)
    checkEquals(test1[["AUCPartial"]], AUCLast(Time = t0, Conc = c0 ), msg = " || TEST 1 : endTime is the final element of Time" )
    
    test2 <- getPartialAUC(Conc = c1, Time =  t1, startTime = 1, endTime = 1.5)
    checkEquals(test2[["AUCPartial"]], 64.533, msg = " || TEST 2 : startTime and endTime from WinNonLin Test Data")
    
    test3 <- getPartialAUC(Conc = c0, Time = t0, startTime = 2, endTime = 4)
    checkEquals(test3[["AUCPartial"]], AUCLast(Time = 0:4, Conc = 10:6 ) - AUCLast(Time = 0:2, Conc = 10:8 ), msg = " || TEST 3 : startTime is 3rd element, endTime is the 5th")
    
    # TEST 4-5 : endTime is not an element of Time, but is inside the lower and upper limits
    
    test4 <- getPartialAUC(Conc = c0, Time = t0, startTime = 0, endTime = 1.75)
    checkEquals(test4[["AUCPartial"]], AUCLast(Conc = c(10, 9, 8.25), Time = c(0, 1, 1.75)), msg = " || TEST 4 : endTime lies between 2nd and 3rd elements of Time")
    
    test5 <- getPartialAUC(Conc = c0, Time = t0, startTime = 2.2, endTime = 8.3)
    checkEquals(test5[["AUCPartial"]], AUCLast(Conc = c(10:2, 1.7), Time = c(0:8, 8.3)) - AUCLast(Conc = c(10:8, 7.8), Time = c(0:2, 2.2)), msg = " || TEST 5 : endTime lies between last 2 elements of Time ")
    
    # TEST 6 : endTime is not an element of t1, but is inside the lower and upper limits
    
    test6 <- getPartialAUC(Conc = c1, Time = t1, startTime = 10, endTime = 50)
    checkEquals(test6[["AUCPartial"]], 4974.378865, tol = 1e-7, msg = " || TEST 6 : endTime lies between middle elements of Time" )
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- getPartialAUC(Conc = Theoph2$conc, Time = Theoph2$Time, startTime = 0, endTime = 18)
    checkEquals(test7[["AUCPartial"]], AUCLast(Time = Theoph2$Time[1:10], Conc = Theoph2$conc[1:10]) + 14.97220, tol = 1e-7, msg = " || TEST 7 Theoph data set, endTime between last 2 elements of Time")
    
    # TEST 8 : NA should be returned if endTime < earliest time
    
    test8 <- getPartialAUC(Conc =  c1, Time = t1, startTime = -1, endTime = 1)
    checkTrue(is.na(test8[["AUCPartial"]]), msg = " || TEST 8 : endTime < Time[1]")
    
    # TEST 9-13 : check for endTime > tlast, calculable lambdaz
    
    test9 <- getPartialAUC(Conc = Theoph2$conc, Time = Theoph2$Time, startTime = 0, endTime = 26)
    checkEquals(test9[["AUCPartial"]], 92.91137853, tol = 1e-7, msg = " || TEST 9 : endTime > last time, calculable lambdaz" )
    
    test10 <- getPartialAUC(Conc = c1, Time = t1, startTime = 500, endTime = 600)
    checkEquals(test10[["AUCPartial"]], 680.757494, tol = 1e-7, msg = " || TEST 10 : endTime > last time, checking extrapolation" )

    test11 <- getPartialAUC(Conc = c(0, 3.94, 1.41, 1.71, 0.98, 0.57), 
        Time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), startTime = 500, endTime = 600)
        checkEquals(test11[["AUCPartial"]], 3.404974791e-08, tol = 1e-7, msg = " || TEST 11 : endTime > last time, checking extrapolation" )
    
    test12 <- getPartialAUC(Conc = c(0, 3.94, 1.41, 1.71, 0.98, 0.57), 
        Time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), startTime = 100, endTime = 100)
        checkEquals(test12[["AUCPartial"]], 0, tol = 1e-7, msg = " || TEST 12 : endTime > last time, startTime same as endTime" )
    
    test13 <- getPartialAUC(Conc = c(0, 394000000, 141000000, 171000000, 98000000, 57000000), 
        Time = c(0, 20.76923, 33.53846, 45.30769, 54.07692, 68.84615), startTime = 70.00000000001, endTime = 70.00000000002)
        checkEquals(test13[["AUCPartial"]], 0.0005226135254, tol = 1e-7, msg = " || TEST 13 : endTime > last time, startTime close to endTime" )

    # TEST 14 : check for endTime < tlast, lambdaz NA
    
    c2 <- c(0, 3.94, 1.41, 1.71, 0.57)
    t2 <- c(0, 20.76923, 33.53846, 45.30769, 68.84615)
    
    test14 <- getPartialAUC(Conc = c2, Time = t2, startTime = 3, endTime = 26)
    checkEquals(test14[["AUCPartial"]], 57.96040244, tol = 1e-7, msg = " || TEST 14 : check for endTime < tlast, lambdaz NA" )
    
    # TEST 15 : check for endTime < tlast, lambdaz NA
    
    test15 <- getPartialAUC(Conc = c2, Time = t2, startTime = 50, endTime = 600)
    checkEquals(test15[["AUCPartial"]], as.numeric(NA), msg = " || TEST 15 : check for endTime < tlast, lambdaz NA" )
}

test.getPartialAUC_errorHandling <- function()
{
    
    # TEST 1 : missing values in Conc
    
    time <- 0:9
    conc <- c(10:4, NA, 2:1)
    
    test1 <- getPartialAUC(Conc = conc, Time = time, startTime = 2, endTime = 8)
    checkEquals(test1[["Error"]], "Error in try(if (any(is.na(Conc))) { : Missing values in Conc\n", 
        msg = " || TEST 1 : missing values in Conc" )
   
    # TEST 2 : endTime is not a length 1 
    
    test2 <- getPartialAUC(Conc = 2:1, Time = 1:2, startTime = 0, endTime = c(1, 2))
    checkEquals(test2[["Error"]], "Error in checkSingleNumeric(endTime, description = \"endTime\", \"getPartialAUC\") : \n  Error in  getPartialAUC :  endTime is not a numeric of length 1.  Value is: 1 2\n",
            msg = "  || TEST 2 : endTime is not of length 1")
    
    # TEST 3 : startTime is not numeric
    
    test3 <- getPartialAUC(Conc = 2:1, Time = 1:2, startTime = "1", endTime = 3)
    checkEquals(test3[["Error"]], "Error in checkSingleNumeric(startTime, description = \"startTime\", \"getPartialAUC\") : \n  Error in  getPartialAUC :  startTime is not a numeric of length 1.  Value is: 1\n",
                msg = " || TEST 3 : startTime is not numeric ")
    
}
