# SVN revision: 
# Date of last change: 18/06/2013
# Last changed by: ccampbell
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# test the CmaxTmax function from maxmin.R

test.CmaxTmax <- function()
{
    
    # minimum number of cases = 4
    
    # TEST 1 : standard case
    
    test1 <- CmaxTmax( Conc = c(NA, 10, 9:2), Time = c(1:10) )
    checkEquals( test1, list(cmax = 10, tmax = 2, index = 2), msg = " || simple case is correct")
    
    # TEST 2 : 1 < maxima, check that minimal time is retrieved
    
    test2 <- CmaxTmax( Conc = c(1:3, rep(10, times = 7) ), Time = c(10:1) )
    checkEquals( test2, list(cmax = 10, tmax = 7, index = 4), msg = " || simple case is correct")
    
    # TEST 3 : basic error handling - vectors should be of same length
    
    test3 <- try( CmaxTmax( 1:10, 1:9 ), silent = TRUE)
    checkTrue( is(test3, "try-error"), msg = " || TEST3 : exception generated ")
    # FIXME : need to check that the exception message is correct
    
    # TEST 4 : one of the times corresponding to cmax is NA
    
    test4 <- CmaxTmax( c(1, 2, 3, 3), c(1, 2, 3, NA) )
    checkEquals( test4, list( cmax = 3, tmax = 3, index = 3), msg = "|| TEST4: multiple values of concentration, one is NA" )
    
    # TEST 5 : only time corresponding to cmax is NA
    
    test5 <- CmaxTmax( 1:3, c(1, 2, NA) )
    checkEquals( test5, list( cmax = 3, tmax = as.numeric(NA), index = 3), msg = "|| TEST5: time corresponding to max concentration is NA" )    
    
    # TEST 6 : concentrations add up to 0

    test6 <- CmaxTmax(Time =  1:4, Conc = rep(0, 4) )
    checkEquals( test6, list(cmax = NA, tmax = NA, index = NA), msg = " || TEST6 : concentrations add up to 0 causes NA values to be returned" )
    
    # TEST 7 : max concentration is 0
    
    test7 <- CmaxTmax(Time =  1:2, Conc = c(-1, 0) )
    checkEquals( test7, list(cmax = 0, tmax = NA, index = 2), msg = " || TEST7 : max concentration is 0, so NA is returned for time" )
    
    # TEST 8 : no non-missing concentrations.  Return should be NA (?)
    
    test8 <- CmaxTmax( Time = 1:3, Conc = rep(NA, 3) )
    checkEquals( test8, list(cmax = NA, tmax = NA, index = NA), msg = " || No non-missing concentrations.  Return should be NA")

    # TEST 9 : 3 maxima, check that minimal time is retrieved
    
    test9 <- CmaxTmax(Conc = c(1, 10, 8, 10, 7, 10, 6, 4, 3.5, 2.8), Time = 0:9 )
    checkEquals( test9, list(cmax = 10, tmax = 1, index = 2), msg = " || simple case is correct")
}
