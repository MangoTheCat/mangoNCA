
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("CmaxTmax")

# test the CmaxTmax function from maxmin.R

test_that("CmaxTmax",  {
    
    # minimum number of cases = 4
    
    # TEST 1 : standard case
    
    test1 <- CmaxTmax( conc = c(NA, 10, 9:2), time = c(1:10) )
    expect_equal( test1, list(cmax = 10, tmax = 2, index = 2))
    
    # TEST 2 : 1 < maxima, check that minimal time is retrieved
    
    test2 <- CmaxTmax( conc = c(1:3, rep(10, times = 7) ), time = c(10:1) )
    expect_equal( test2, list(cmax = 10, tmax = 7, index = 4))
    
    # TEST 3 : basic error handling - vectors should be of same length
    expect_error(CmaxTmax( 1:10, 1:9), regex = "length")
    
    # TEST 4 : one of the times corresponding to cmax is NA
    
    test4 <- CmaxTmax( c(1, 2, 3, 3), c(1, 2, 3, NA) )
    expect_equal( test4, list( cmax = 3, tmax = 3, index = 3) )
    
    # TEST 5 : only time corresponding to cmax is NA
    
    test5 <- CmaxTmax( 1:3, c(1, 2, NA) )
    expect_equal( test5, list( cmax = 3, tmax = as.numeric(NA), index = 3) )    
    
    # TEST 6 : concentrations add up to 0

    test6 <- CmaxTmax(time =  1:4, conc = rep(0, 4) )
    expect_equal( test6, list(cmax = NA, tmax = NA, index = NA) )
    
    # TEST 7 : max concentration is 0
    
    test7 <- CmaxTmax(time =  1:2, conc = c(-1, 0) )
    expect_equal( test7, list(cmax = 0, tmax = NA, index = 2) )
    
    # TEST 8 : no non-missing concentrations.  Return should be NA (?)
    
    test8 <- CmaxTmax( time = 1:3, conc = rep(NA, 3) )
    expect_equal( test8, list(cmax = NA, tmax = NA, index = NA))

    # TEST 9 : 3 maxima, check that minimal time is retrieved
    
    test9 <- CmaxTmax(conc = c(1, 10, 8, 10, 7, 10, 6, 4, 3.5, 2.8), time = 0:9 )
    expect_equal( test9, list(cmax = 10, tmax = 1, index = 2))
})
