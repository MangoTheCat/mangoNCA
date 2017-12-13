# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests getConcentration function from getConcentration.R

test.getConcentration <- function()
{
    time <- 0:9
    conc <- 10:1
    
    # TEST 1 - 3 : secondTime is an element of time - normal and boundary cases
    
    test1 <- getConcentration(Conc = conc, Time = time, firstTime = 0, secondTime = 9)
    checkEquals(as.numeric(test1[1, c("firstConc", "secondConc")]), c(10, 1), msg = " || TEST 1 : firstTime 1st, secondTime final element of Time" )
    
    test2 <- getConcentration(Conc = conc, Time =  time, firstTime = 3, secondTime = 2)
    checkEquals(as.numeric(test2[1, c("firstConc", "secondConc")]), c(7, 8), msg = " || TEST 2 : firstTime 4th, secondTime 3rd element of Time")
    
    test3 <- getConcentration(Conc = conc, Time = time, firstTime = 2, secondTime = NA)
    checkEquals(test3[["firstConc"]], 8, msg = " || TEST 3 (a) : firstTime is 3rd element")
    checkEquals(test3[["secondConc"]], as.numeric(NA), msg = " || TEST 3 (b) : secondTime is NULL")
    
    ## TEST 4-6 : secondTime is not an element of Time, but is inside the lower and upper limits
    
    test4 <- getConcentration(Conc = conc, Time = time, firstTime = 0, secondTime = 1.75)
    checkEquals(test4[["secondConc"]], 8.25, msg = " || TEST 4 : secondTime lies between 2nd and 3rd elements of Time")
    
    test5 <- getConcentration(Conc = conc, Time = time, firstTime = NA, secondTime = 8.4)
    checkEquals(test5[["firstConc"]], as.numeric(NA), msg = " || TEST 5 (a) : firstTime is NA")
    checkEquals(test5[["secondConc"]], 1.6, msg = " || TEST 5 (b) : secondTime lies between last 2 elements of Time ")
    
    test6 <- getConcentration(Conc = conc, Time = time, firstTime = 2.1e-8, secondTime = as.numeric(NA))
    checkEquals(test6[["firstConc"]], 10, msg = " || TEST 6 : secondTime just after 1st element of Time" )
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- getConcentration(Conc = Theoph2$conc, Time = Theoph2$Time, firstTime = 1, secondTime = 18)
    checkEquals(as.numeric(test7[1, c("firstConc", "secondConc")]), c(8.31, 1.980731707), tol = 1e-7, msg = " || TEST 7 Theoph data set, secondTime between last 2 elements of Time")
    
    # TEST 8 - 9 : check for secondTime > tlast
    
    test8 <- getConcentration(Conc = c(1, 6, 5, 4, 2, 1), Time = 0:5, firstTime = as.numeric(NA), secondTime = 6)
    checkEquals(test8[["secondConc"]], 0.5, msg = " || TEST 8 : secondTime < Time[1]")
    
    test9 <- getConcentration(Conc = Theoph2$conc, Time = Theoph2$Time, firstTime = 0, secondTime = 26)
    checkEquals(test9[["secondConc"]], 0.7445239941, msg = " || TEST 9 : secondTime > last time, checking extrapolation" )
    
}

test.getConcentration_errorHandling <- function()
{
    
    # TEST 1 : missing values present
    
    test1 <- try( getConcentration(Conc = c(NA, 6, 5, 4, 2, 1), Time = 0:5, secondTime = 6), silent = TRUE )
    checkEquals( test1[["Error"]], "Error in try(if (any(is.na(Conc))) { : Missing values in Conc\n",
            msg = " || TEST 1 : missing numPoints")
   
    # TEST 2 : secondTime is not a length 1 
    
    test2 <- getConcentration(Conc = 2:1, Time = 1:2, firstTime = 0, secondTime = c(1, 2))
    checkEquals(test2[["Error"]], "Error in checkSingleNumeric(secondTime, description = \"secondTime\", \"getConcentration\") : \n  Error in  getConcentration :  secondTime is not a numeric of length 1.  Value is: 1 2\n",
            msg = "  || TEST 2 : secondTime is not of length 1")
    
    # TEST 3 : firstTime is not numeric
    
    test3 <- getConcentration(Conc = 2:1, Time = 1:2, firstTime = "1", secondTime = 3)
    checkEquals(test3[["Error"]], "Error in checkSingleNumeric(firstTime, description = \"firstTime\", \"getConcentration\") : \n  Error in  getConcentration :  firstTime is not a numeric of length 1.  Value is: 1\n",
                msg = " || TEST 3 : firstTime is not numeric ")
    
}
