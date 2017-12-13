# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 20/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests CminTmin function

test.CminTmin <- function()
{
    
    if (!exists("CminTmin", mode = "function")) { CminTmin <- MangoNca:::CminTmin }
    
    # TEST 1 : standard case
    
    test1 <- CminTmin( Conc = c(NA, 10, 9:2), Time = c(1:10) )
    checkEquals( test1, list(cmin = 2, tmin = 10, index = 10), msg = " || simple case is correct")
    
    # TEST 2 : 3 minima, check that minimal time is retrieved
    
    test2 <- CminTmin( Conc = c(2:4, rep(1, times = 7) ), Time = c(10:1) )
    checkEquals( test2, list(cmin = 1, tmin = 7, index = 4), msg = " || simple case is correct")
    
    # TEST 3 : basic error handling - vectors should be of same length
    
    test3 <- try( CminTmin( 1:10, 1:9 ), silent = TRUE)
    checkTrue( is(test3, "try-error"), msg = " || TEST3 : exception generated ")
    # FIXME : need to check that the exception message is correct
    
    # TEST 4 : one of the times corresponding to cmin is NA
    
    test4 <- CminTmin( c(2, 3, 1, 1), c(1, 2, 3, NA) )
    checkEquals( test4, list( cmin = 1, tmin = 3, index = 3), msg = "|| TEST4: multiple values of concentration, one is NA" )
    
    # TEST 5 : only time corresponding to cmin is NA
    
    test5 <- CminTmin( 3:1, c(1, 2, NA) )
    checkEquals( test5, list( cmin = 1, tmin = as.numeric(NA), index = 3), msg = "|| TEST5: time corresponding to min concentration is NA" )    
    
    # TEST 6 : concentrations add up to 0
    
    test6 <- CminTmin(Time =  1:4, Conc = rep(0, 4) )
    checkEquals( test6, list(cmin = NA, tmin = NA, index = NA), msg = " || TEST6 : concentrations add up to 0 causes NA values to be returned" )
    
    # TEST 7 : min concentration is 0
    
    test7 <- CminTmin(Time =  1:2, Conc = c(1, 0) )
    checkEquals( test7, list(cmin = 0, tmin = 2, index = 2), msg = " || TEST7 : min concentration is 0" )
    
    # TEST 8 : no non-missing concentrations.  Return should be NA
    
    test8 <- CminTmin( Time = 1:3, Conc = rep(NA, 3) )
    checkEquals( test8, list(cmin = NA, tmin = NA, index = NA), msg = " || No non-missing concentrations.  Return should be NA")
}
