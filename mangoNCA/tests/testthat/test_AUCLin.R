
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test AUCLin")

test_that("AUCLin", {
    
    # TEST 1 : only one concentration means only 0 is returned
    
    test1 <- AUCLin( 1, 1 )
    expect_equal(test1, 0)
    
    # TEST 2 : 0 elements
    
    test2 <- AUCLin( numeric(0), numeric(0) )
    expect_true(is.na(test2))
    
    # TEST 3 : 2 elements, one NA present.
    
    test3 <- AUCLin( c(0, NA), 0:1 )
    expect_true(is.na(test3))
    
    # TEST 4 : "standard" data set.  Use built-in theophiline data set
    # Total AUC is equal to AUC determined by trapezium rule
    # NOTE: result is validated against trapezium function
    
    Theoph12 <- subset(Theoph, Subject == 12 )
    test4 <- AUCLin(conc = Theoph12$conc, time = Theoph12$Time )
    expect_equal(sum(test4), mangoNCA:::trapezium(Theoph12$Time, Theoph12$conc))
    
    # TEST 5 : contrived data set
    # for contrived data, individual AUCs should all be 2
    
    test5 <- AUCLin( time = 1:100, conc = rep(2, 100) )
    expect_equal( test5, rep(2, 99))
    
    # TEST 6 : another contrived data set
    
    test6 <- AUCLin( time = c(0, 1, 2.5, 3) , c(0, 1, 0.5, 0.25))
    expect_equal(test6, c(0.5, 1.125, 0.1875))
    
    # TEST 7 : Missing values in various locations 
    # all values should be missing
    
    test7 <- AUCLin(time = c(1, NA, 2, NA, 3), conc = c(NA, 1, NA, 2, NA))
    expect_equal( test7, as.numeric(c(NA, NA, NA, NA)))
    
    # TEST 8 : single missing time
    
    test8 <- AUCLin(time = c(1, NA, 2, 4, 5), conc = c(1, 2, 3, 4, 5))
    expect_equal( test8, c(NA, NA, 7, 4.5 ))
    
    # TEST 9 : single missing concentration
    
    test9 <- AUCLin(time = c(1, 1.5, 2, 4, 5), conc = c(1, 2, NA, 4, 5))
    expect_equal(test9, c(0.75, NA, NA, 4.5 ))
    
    # TEST 10 : 2 elements only
    
    test10 <- AUCLin(time =  c(1, 1000), c(1, 1.5))
    expect_equal(test10, 1248.75)
    
    # TEST 11 : vectors of different length
    
    expect_error(AUCLin(time = 1:10, conc = 1:9), 
        regex = "lengths of time and concentration do not match")
    
    # TEST 12 : unordered time vector should generate an exception 
    
    expect_error(AUCLin( time = 3:1, conc = 1:3), 
        regex = "time is not ordered.  Actual value is 3 2 1")
    
    # TEST 13 : unordered time vector (with missing values) should generate an exception 
    
    expect_error(AUCLin( time = c(1,2, NA, 1.5), conc = 4:1), 
        regex = "time is not ordered.  Actual value is") 
    
    # TEST 14 : WNL example Log interpolation
    # http://forum.bebac.at/forum_entry.php?id=3723
    
    c14 <- c(0, 0.12, 1.85, 2.92, 1.85, 0.73, 0.42, 0.19)
    t14 <- c(0, 0.5, 1, 1.5, 2, 4, 6, 8)
    
    test14 <- AUCLin(conc = c14, time = t14)
    expect_equal(round(sum(test14), digits = 5), 7.2475)
  
})
