
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUMCPartial")

# tests AUMCPartial function from AUMCPartial.R

test_that("AUMCPartial", {
    
    time <- 0:10
    conc <- c(0, 10:1)
    
    # TEST 1 - 3 : endtime is an element of time
    # endtime is the final element of time
    test1 <- AUMCPartial(conc = conc, time = time, endtime = 10, lamznpt = 3, addt0 = FALSE)    
    expect_equal(test1, AUCLast(conc = conc * time, time = time, addt0 = FALSE))
    
    # endtime is the second element of time
    test2 <- AUMCPartial(conc = conc, time =  time, endtime = 2, lamznpt = 3, addt0 = FALSE)    
    expect_equal(test2, AUCLast(time = 0:2, conc = c(0, 10:9) * 0:2, addt0 = FALSE))
    
    # endtime is the third element of time
    test3 <- AUMCPartial(conc = conc, time = time, endtime = 3, lamznpt = 3)    
    expect_equal(test3, AUCLast(time = 0:3, conc = c(0, 10:8) * 0:3 ))
    
    # TEST 4-6 : endtime is not an element of time, but is inside the lower and upper limits
    # endtime lies between first two elements of time
    test4 <- AUMCPartial(conc = conc, time = time, endtime = 1.75, lamznpt = 3, addt0 = FALSE)
    test4comp <- mangoNCA:::trapezium(c(0, 1, 1.75), c(0, 10, 9.25 * 1.75))
    expect_equal(test4,  test4comp, tol = 1e-7)
    # endtime lies between last 2 elements of time
    test5 <- AUMCPartial(conc = conc, time = time, endtime = 9.25, lamznpt = 3)
    expect_equal(test5, AUCLast(conc = c(0, 10:2, 1.75) * c(0:9, 9.25), time = c(0:9, 9.25)))
    
    # endtime lies between middle elements of time
    test6 <- AUMCPartial(conc = conc, time = time, endtime = 5.2, lamznpt = 3)
    expect_equal(test6, AUCLast(conc = c(0, 10:6, 5.8) * c(0:5, 5.2), time = c(0:5, 5.2)))
    
    # TEST 7 : alternative data set
    # Theoph data set, endtime between last 2 elements of time
    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- AUMCPartial(conc = Theoph2$conc, 
        time = Theoph2$Time, endtime = 18, lamznpt = 3)
    expect_equal(test7, AUCLast(time = Theoph2$Time[1:10], conc =  Theoph2$Time[1:10] * Theoph2$conc[1:10]) + 215.3195, tol = 1e-7)
    
    # TEST 8 : NA should be returned if endtime < earliest time
    test8 <- AUMCPartial(conc =  4:1,
        time = 0:3, endtime = -1, lamznpt = 3, addt0 = FALSE)
    expect_true(is.na(test8))
    
    # TEST 9 : check for endtime > tlast
    test9 <- AUMCPartial(conc = Theoph2$conc, 
        time = Theoph2$Time, endtime = 26, lamznpt = 5)
    expect_equal(test9, 707.9921, tol = 1e-7)
    
})

test_that("AUMCPartial error",  {
    
    # TEST 1 : missing lamznpt
    Theoph2 <- Theoph[Theoph$Subject == 2, ]
    expect_error(AUMCPartial(conc = Theoph2$conc, 
        time = Theoph2$Time, endtime = 26), 
        regex = "lamznpt is not a numeric of length 1.  Value is:")
    
    # TEST 2 : endtime is not a numeric of length 1
    expect_error(AUMCPartial(conc = 3:1, 
        time = 0:2, endtime = c(1, 2), lamznpt = 3), 
        regex = "endtime is not a numeric of length 1.  Value is: 1 2")
    
    # TEST 3 : endtime is not numeric
    expect_error(AUMCPartial(conc = 3:1, 
        time = 0:2, endtime = "1", lamznpt = 3), 
        regex = "endtime is not a numeric of length 1.  Value is: 1")
})
