
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUCPartial")

# tests AUCPartial function from AUCPartial.R

test_that("AUCPartial", {
    
    time <- 0:10
    conc <- c(0, 10:1)
    
    # TEST 1 - 3 : endtime is an element of time - normal and boundary cases
    
    test1 <- AUCPartial(conc = conc, time = time, endtime = 10, lamznpt = 3, addt0 = TRUE)
    expect_equal(test1, AUCLast(time = time, conc = conc ), msg = " || TEST 1 : endtime is the final element of time" )
    
    test2 <- AUCPartial(conc = conc, time =  time, endtime = 2, lamznpt = 3, addt0 = TRUE)
    expect_equal(test2, AUCLast(time = 0:2, conc = c(0, 10:9)), msg = " || TEST 2 : endtime is the second element of time")
    
    test3 <- AUCPartial(conc = conc, time = time, endtime = 3, lamznpt = 3)
    expect_equal(test3, AUCLast(time = 0:3, conc = c(0, 10:8)), msg = " || TEST 3 : endtime is the third element of time ")
    
    # TEST 4-6 : endtime is not an element of time, but is inside the lower and upper limits
    
    # TEST 4 : endtime lies between first two elements of time
    test4 <- AUCPartial(conc = conc, time = time, endtime = 1.75, lamznpt = 3)
    expect_equal(test4, AUCLast(conc = c(0, 10, 9.25), time = c(0, 1, 1.75)))
    
    # TEST 5 : endtime lies between last 2 elements of time
    test5 <- AUCPartial(conc = conc, time = time, endtime = 9.25, lamznpt = 3)
    expect_equal(test5, AUCLast(conc = c(0, 10:2, 1.75), time = c(0:9, 9.25)))
    
    # TEST 6 : endtime lies between middle elements of time
    test6 <- AUCPartial(conc = conc, time = time, endtime = 5.2, lamznpt = 3)
    expect_equal(test6, AUCLast(conc = c(0, 10:6, 5.8), time = c(0:5, 5.2)))
    
    # TEST 7 : alternative data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- AUCPartial(conc = Theoph2$conc, time = Theoph2$Time, endtime = 18, lamznpt = 3)
    expect_equal(test7, AUCLast(time = Theoph2$Time[1:10], 
        conc = Theoph2$conc[1:10]) + 14.97220, tol = 1e-7)
    
    # TEST 8 : NA should be returned if endtime < earliest time
    
    test8 <- AUCPartial(conc =  4:1, time = 0:3, endtime = -1, 
        lamznpt = 3, addt0 = FALSE)
    expect_true(is.na(test8))
    
    # TEST 9 : check for tau > tlast
    # endtime > last time, checking extrapolation
    
    test9 <- AUCPartial(conc = Theoph2$conc, time = Theoph2$Time, endtime = 26, lamznpt = 5)
    
    expect_equal(test9, 92.93229975)
    
    # TEST 10 : endtime is T = 0
    
    test10 <- AUCPartial(conc = conc, time = time, endtime = 0, lamznpt = 3, addt0 = TRUE)
    expect_equal(test10, 0)
    
})

test_that("AUCPartial_errorHandling", {
    
    # TEST 1 : missing lamznpt
    
    Theoph2 <- Theoph[Theoph$Subject == 2, ]
    
    expect_error(AUCPartial(conc = Theoph2$conc, time = Theoph2$Time, endtime = 26), 
        regex = "lamznpt is not a numeric of length 1.  Value is:")
   
    # TEST 2 : endtime is not a length 1 
    
    expect_error(AUCPartial(conc = 3:1, time = 0:2, endtime = c(1, 2), lamznpt = 3), 
        regex = "endtime is not a numeric of length 1.  Value is: 1 2")
    
    # TEST 3 : endtime is not numeric
    
    expect_error(AUCPartial(conc = 3:1, time = 0:2, endtime = "1", lamznpt = 3), 
        regex = "endtime is not a numeric of length 1.  Value is: 1")
})
