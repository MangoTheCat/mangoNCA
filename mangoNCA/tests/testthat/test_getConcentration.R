
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test getConcentration")

# tests getConcentration function from getConcentration.R

test_that("getConcentration",  {
    
    time <- 0:9
    conc <- 10:1
    
    # TEST 1 - 3 : secondtime is an element of time - normal and boundary cases
    
    test1 <- getConcentration(conc = conc, time = time, firsttime = 0, secondtime = 9)
    expect_equal(as.numeric(test1[1, c("firstconc", "secondconc")]), c(10, 1))
    
    test2 <- getConcentration(conc = conc, time =  time, firsttime = 3, secondtime = 2)
    expect_equal(as.numeric(test2[1, c("firstconc", "secondconc")]), c(7, 8))
    
    test3 <- getConcentration(conc = conc, time = time, firsttime = 2, secondtime = NA)
    expect_equal(test3[["firstconc"]], 8)
    expect_equal(test3[["secondconc"]], as.numeric(NA))
    
    ## TEST 4-6 : secondtime is not an element of time, but is inside the lower and upper limits
    
    test4 <- getConcentration(conc = conc, time = time, firsttime = 0, secondtime = 1.75)
    expect_equal(test4[["secondconc"]], 8.25)
    
    test5 <- getConcentration(conc = conc, time = time, firsttime = NA, secondtime = 8.4)
    expect_equal(test5[["firstconc"]], as.numeric(NA))
    expect_equal(test5[["secondconc"]], 1.6)
    
    test6 <- getConcentration(conc = conc, time = time, firsttime = 2.1e-8, secondtime = as.numeric(NA))
    expect_equal(test6[["firstconc"]], 10)
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- getConcentration(conc = Theoph2$conc, time = Theoph2$Time, firsttime = 1, secondtime = 18)
    expect_equal(as.numeric(test7[1, c("firstconc", "secondconc")]), c(8.31, 1.980731707), tol = 1e-7)
    
    # TEST 8 - 9 : check for secondtime > tlast
    
    test8 <- suppressWarnings(getConcentration(conc = c(1, 6, 5, 4, 2, 1), 
        time = 0:5, firsttime = as.numeric(NA), secondtime = 6))
    expect_equal(test8[["secondconc"]], 0.5)
    
    test9 <- getConcentration(conc = Theoph2$conc, 
        time = Theoph2$Time, firsttime = 0, secondtime = 26)
    expect_equal(test9[["secondconc"]], 0.7445239941)
})

test_that("getConcentration error",  {
    
    # TEST 1 : missing values present
    
    test1 <- getConcentration(conc = c(NA, 6, 5, 4, 2, 1), time = 0:5, secondtime = 6)
    expect_equal( test1[["ERROR"]], "Error in try(if (any(is.na(conc))) { : Missing values in conc\n")
   
    # TEST 2 : secondtime is not a length 1 
    
    test2 <- getConcentration(conc = 2:1, time = 1:2, firsttime = 0, secondtime = c(1, 2))
    expect_equal(test2[["ERROR"]], "Error in checkSingleNumeric(secondtime, description = \"secondtime\", \"getConcentration\") : \n  Error in  getConcentration :  secondtime is not a numeric of length 1.  Value is: 1 2\n")
    
    # TEST 3 : firsttime is not numeric
    
    test3 <- getConcentration(conc = 2:1, time = 1:2, firsttime = "1", secondtime = 3)
    expect_equal(test3[["ERROR"]], "Error in checkSingleNumeric(firsttime, description = \"firsttime\", \"getConcentration\") : \n  Error in  getConcentration :  firsttime is not a numeric of length 1.  Value is: 1\n")
})
