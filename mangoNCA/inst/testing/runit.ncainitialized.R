# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: ccampbell $
# Last changed by: $LastChangedBy: 19/01/2012 $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.ncaInitialized <- function()
{
    # TEST 1 : Check Initialize Output
    
    test1 <- ncaInitialized()

    expectedOut <- data.frame(rVersionOk = TRUE, rArchOk = TRUE, statsOk = TRUE)
    
    # TODO: reconcile build and application R versions
    checkEquals(test1[, 2:3], expectedOut[, 2:3], msg = " || TEST 1 : Check Initilize Output\n" )
    
}

test.getWhichVersion <- function()
{
    # TEST 1 : Contrived check no errors
    
    test1 <- as.character(getWhichVersion()[[1]])
    
    test1 <- substr(test1, 1, 13)
    
    checkEquals( test1, "R version 2.1", msg = " || TEST 1 : Check Version\n" )
    
}
