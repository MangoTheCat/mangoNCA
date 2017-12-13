
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test checkLambdaZStats")

test_that("checkLambdaZStats",  {

    # TEST 1: length 9 numeric
    
    lzVec <- c(
                LAMZ = 1,
                intercept = exp(2),
                R2 = 1,
                R2ADJ = 1,
                CORRXY = -1,
                LAMZHL = log(2),
                LAMZLL = 2,
                LAMZUL = 2.75,
                lamznpt = 4)
    
    test1 <- checkLambdaZStats(lzVec)
    expect_null(test1)
    
    # TEST 2: lambdaZStatistics output
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    lzOut <- suppressWarnings(lambdaZStatistics(conc = conc, 
        time = time, lamznpt = 4))
    
    test2 <- checkLambdaZStats(unlist(lzOut))
    expect_null(test2)
    
})
