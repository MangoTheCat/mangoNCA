
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUCLog")

test_that("AUCLog", {
    
    rdiff <- function(x) { x[-length(x)] - x[-1] }
    
    # TEST 1 : only one concentration means only 0 is returned
    
    test1 <- AUCLog(1, 1)
    expect_equal(test1, 0)
    
    # TEST 2 : 0 elements means only NA is returned
    
    test2 <- AUCLog(numeric(0), numeric(0))
    expect_true(is.na(test2))
    
    # TEST 3 : 2 elements, one NA present.
    # only NA should be returned
    
    test3 <- AUCLog(c(0, NA), 0:1)
    expect_true(is.na (test3))
    
    # TEST 4 : "standard" data set,  using built-in theophiline data set
    # Total AUC is equal to number
    
    Theoph12 <- subset(Theoph, Subject == 12 )
    test4 <- AUCLog(conc = Theoph12$conc, time = Theoph12$Time)
    expect_equal(sum(test4), 114.85723004832, tol = 1e-8)
    
    # TEST 5 : contrived data set individual AUCs should all be 2
    
    test5 <- AUCLog(time = 1:100, conc = rep(2, 100))
    expect_equal(test5, rep(2, 99))
    
    # TEST 6 : exp vals cf integration
    
    test6 <- AUCLog(time = (0:5)^2, conc = 25 * exp(-(1/pi) * (0:5)^2))
    expect_equal(test6, rdiff(25 / (1/pi) * exp(-(1/pi) * (0:5)^2)))
    
    # TEST 7 : Missing values in various locations 
    
    test7 <- AUCLog( time = c(1, NA, 2, NA, 3), conc = c(NA, 1, NA, 2, NA))
    expect_equal( test7, as.numeric(c(NA, NA, NA, NA)))
    
    # TEST 8 : Single missing time 
    
    test8 <- AUCLog(time = c(1, NA, 2, 4, 5), conc = c( 1, 2, 3, 4, 5))
    expect_equal(round(test8, 2), c(NA, NA, 6.95, 4.48))
    
    # TEST 9 : similar to the above, but missing concentrations
    
    test9 <- AUCLog( time = c(1, 1.5, 2, 4, 5), conc = c( 1, 2, NA, 4, 5))
    expect_equal(round(test9, 2), c(0.72, NA, NA, 4.48 ))
    
    # TEST 10 : 2 elements only
    
    test10 <- AUCLog(time =  c(1, 1000), c(1, 1.5))
    expect_equal(round(test10, 5), 1231.91857)
    
    # TEST 11 : vectors of different length
    
    expect_error(AUCLog(time = 1:10, conc = 1:9), regex = "length")
    
    # TEST 12 : unordered time vector should generate an exception 
    
    expect_error(AUCLog(time = 3:1, conc = 1:3), 
        regex = "Error in AUCLog: time is not ordered.  Actual value is 3 2 1")
    
    # TEST 13 : unordered time vector (with missing values) should generate an exception 
    
    expect_error(AUCLog( time = c(1, 2, NA, 1.5), conc = 4:1), 
        regex = "Error in AUCLog: time is not ordered.  Actual value is 1 2 NA 1.5")
    
    # TEST 14 : WNL example Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    
    #c14 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.73, 0.42, 0.19)
    #t14 <- c(0, 0.5, 1, 1.5, 2, 4, 6, 8)
    
    #test14 <- AUCLog(conc = c14, time = t14)
    #expect_equal(round(sum(test14), digits = 5), 6.8010, msg = " || TEST 14 : WNL example Log interpolation")
    
})
