
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("CminTmin")

# tests CminTmin function

test_that("CminTmin",  {
    
    # TEST 1 : standard case
    
    test1 <- CminTmin( conc = c(NA, 10, 9:2), time = c(1:10) )
    expect_equal( test1, list(cmin = 2, tmin = 10, index = 10))
    
    # TEST 2 : 3 minima, check that minimal time is retrieved
    
    test2 <- CminTmin( conc = c(2:4, rep(1, times = 7) ), time = c(10:1) )
    expect_equal( test2, list(cmin = 1, tmin = 7, index = 4))
    
    # TEST 3 : basic error handling - vectors should be of same length
    expect_error(CminTmin( 1:10, 1:9 ), regex = "length")
    
    # TEST 4 : one of the times corresponding to cmin is NA
    
    test4 <- CminTmin( c(2, 3, 1, 1), c(1, 2, 3, NA) )
    expect_equal( test4, list( cmin = 1, tmin = 3, index = 3) )
    
    # TEST 5 : only time corresponding to cmin is NA
    
    test5 <- CminTmin( 3:1, c(1, 2, NA) )
    expect_equal( test5, list( cmin = 1, tmin = as.numeric(NA), index = 3) )    
    
    # TEST 6 : concentrations add up to 0
    
    test6 <- CminTmin(time =  1:4, conc = rep(0, 4) )
    expect_equal( test6, list(cmin = NA, tmin = NA, index = NA) )
    
    # TEST 7 : min concentration is 0
    
    test7 <- CminTmin(time =  1:2, conc = c(1, 0) )
    expect_equal( test7, list(cmin = 0, tmin = 2, index = 2) )
    
    # TEST 8 : no non-missing concentrations.  Return should be NA
    
    test8 <- CminTmin( time = 1:3, conc = rep(NA, 3) )
    expect_equal( test8, list(cmin = NA, tmin = NA, index = NA))
})
