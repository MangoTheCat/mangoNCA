# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 18/03/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests AUCLast from AUCLast.R

test.AUCLast <- function()
{
    
    # TEST 1 : basic contrived data set, no missing data
  
    test1 <- AUCLast(Conc = rep(2, 100), Time = 0:99, addT0 = FALSE)
    checkEquals( test1, 2 * 99, msg = " || TEST 1 : Contrived data set AUC correct  " )
    
    # TEST 2 : contrived data set, last concentration element is 0
    
    test2 <- AUCLast(Conc = c(rep(2, 9), 0), Time = 0:9, addT0 = FALSE)
    checkEquals( test2, 8 * 2, msg = " || TEST 2 : contrived data set, last concentration element is 0" )
    
    # TEST 3 : contrived data set, multiple 0 elements at the end
    
    test3 <- AUCLast(Conc = c(0, 8:2, 0, 0), Time = 0:9, addT0 = FALSE)
    checkEquals( test3, 34, msg = " || TEST 3 : contrived data set, multiple 0 elements at the end" )
    
    # TEST 4 : NA present in time values
    
    test4 <- AUCLast(Conc = 3:1, Time =  c(NA, 1, 2), addT0 = TRUE)
    checkEquals(test4, 2.5) # trapezium area 1 (from 0:1) + 1.5 (from 1:2)
    
    # TEST 5 : NA present in concentration values
    
    test5 <- AUCLast(Conc = c(NA, 2:1), Time =  1:3, addT0 = TRUE)
    checkEquals(test5, 3.5) # trapezium area 2 (from 0:2) + 1.5 (from 2:3)
    
    # TEST 6 : actual data set
   
    Theoph1 <- subset(Theoph, Subject == 1)
    
    test6 <- AUCLast(Conc = Theoph1$conc, Time = Theoph1$Time)
    checkEquals(test6, 148.92305, tol = 1e-6)
    
    # TEST 7 : unsorted time values
    
    test7 <- try(AUCLast(Conc = 1:10, Time = 9:0), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage( test7, "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"AUCLast\") : \n  Error in AUCLast: Time is not ordered.  Actual value is 9 8 7 6 5 4 3 2 1 0\n"))

    # TEST 8 : 1 concentration has area 0
    
    test8 <- AUCLast(Conc = 1, Time = 0, addT0 = FALSE)
    checkEquals(test8, 0, msg = " || TEST 8 : 1 concentration has area 0")
    
    # TEST 9 : 2 concentrations including T = 0
    
    test9 <- AUCLast(Conc = 1:2, Time = 0:1, addT0 = TRUE)
    checkEquals(test9, 1.5, msg = " || TEST 9 : 2 concentrations correct")
    
    # TEST 10 : Time is not a vector
    
    test10 <- try(AUCLast(Conc = Theoph["conc"], Time = Theoph["Time"]), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage( test10, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\", functionName = \"AUCLast\") : \n  Error in AUCLast: Time is not a numeric vector\n"))
    
    # TEST 11 : Negative values of Conc
    
    test11a <- AUCLast(Conc = c(-1, 1), Time = 0:1, addT0 = TRUE)
    checkEquals( test11a, 0.5, msg = " || TEST 11a : Negative values of Conc with addT0 TRUE removed and T = 0 added" )
    
    test11b <- try(AUCLast(Conc = c(-1, 1), Time = 0:1, addT0 = FALSE), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage( test11b, "Error in AUCLast(Conc = c(-1, 1), Time = 0:1, addT0 = FALSE) : \n  Error in AUCLast: Error during data cleaning Error in cleanConcTime(Conc = Conc, Time = Time, addT0 = addT0) : \n  values of Conc < 0 in cleanData\n\n"))
    
    # TEST 12 : basic contrived data set, no missing data, add T = 0
  
    test12 <- AUCLast(Conc = rep(2, 100), Time = 1:100, addT0 = TRUE)
    checkEquals( test12, 2 * 100 - 2/2, msg = " || TEST 12 : Contrived data set AUC correct" )
    
    # TEST 13 : basic contrived data set repeated time values in Time
  
    test13 <- AUCLast(Conc = rep(2, 6), Time = c(0, 1, 2, 2, 2, 3), addT0 = TRUE)
    checkEquals( test13, 6, msg = " || TEST 13 : basic contrived data set repeated time values in Time" )
    
    # TEST 14 : basic contrived data set repeated time values in Time
  
    test14 <- AUCLast(Conc = 1:6, Time = c(0, 1, 2, 2, 2, 3), addT0 = TRUE, inter = "Linear")
    checkEquals( test14, 9.5, msg = " || TEST 14 : basic contrived data set repeated time values in Time" )
    
    # TEST 15 : WNL example Linear/Log interpolation
    
    #c15 <- c(18.01, 16.96, 14.16, 10.49, 5.803, 1.881, 0.715, 0.191, 0.096, 0.052, 0.028, 0.015, 0.008)
    #t15 <- c(0.0167, 0.033, 0.083, 0.167, 0.33, 0.67, 1, 2, 4, 6, 8, 10, 12)
    #testFile <- system.file("data", "wnl_analysis_linlog.csv", package = "MangoNca")
    #testData <- read.csv(testFile)
    #testData <- read.csv("C:\\Users\\ccampbell\\Documents\\IMC_RapidNCA\\R\\MangoNca\\trunk\\data\\wnl_analysis_linlog.csv")
    
    #test15 <- AUCLast(Conc = c15, Time = t15, addT0 = TRUE, inter = "Linear Log")
    #checkEquals(test15, testFile[1, "AUClast..hr.ug.mL."], msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    # TEST 16 : WNL example Linear/Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    # Clayton’s example data set subject 5
    
    c16 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.99, 0.73, 0.42, 0.19)
    t16 <- c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8)
    
    test16 <- AUCLast(Conc = c16, Time = t16, inter = "Linear Log")
    checkEquals(round(test16, digits = 5), 6.8176, msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    test16a <- AUCLast(Conc = c16[-6], Time = t16[-6], inter = "Linear Log")
    checkEquals(round(test16a, digits = 4), 6.9976, msg = " || TEST 15 : WNL example Linear/Log interpolation")
    
    # TEST 17 : invalid interpolation method
  
    test17 <- try(AUCLast(Conc = 1:6, Time = c(0, 1, 2, 2, 2, 3), addT0 = TRUE, inter = "abc"), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test17, "Error in AUCLast(Conc = 1:6, Time = c(0, 1, 2, 2, 2, 3), addT0 = TRUE,  : \n  argument inter should have value 'Linear', 'Lin up Log down' or 'Linear Log' in AUCLast\n"))
    
    # TEST 18: 0, 0 Linear
    
    test18 <- AUCLast(0, 0)
    checkEquals(test18, 0, msg = " || TEST 18 : 0, 0 Linear")

    # TEST 19: 0, 0 Linear Log
    
    test19 <- AUCLast(0, 0, inter = "Linear Log")
    checkEquals(test19, 0, msg = " || TEST 19 : 0, 0 Linear Log")

    # TEST 20: 0, 0 Lin up Log down
    
    test20 <- AUCLast(0, 0, inter = "Lin up Log down")
    checkEquals(test20, 0, msg = " || TEST 19 : 0, 0 Lin up Log down") 
}
