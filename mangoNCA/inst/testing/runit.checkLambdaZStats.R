# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 21/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.checkLambdaZStats <- function() {
    
    if (!exists("checkLambdaZStats", mode = "function")) { checkLambdaZStats <- MangoNca:::checkLambdaZStats }
    
    # TEST 1: length 9 numeric
    
    lzVec <- c(
                Lambdaz = 1,
                intercept = exp(2),
                r2 = 1,
                adjr2 = 1,
                rhoXY = -1,
                tPhaseHalfLife = log(2),
                LambdazLower = 2,
                LambdazUpper = 2.75,
                numPoints = 4)
    
    test1 <- checkLambdaZStats(lzVec)
    checkTrue(is.null(test1), msg = " || TEST 1: length 9 numeric" )
    
    # TEST 2: lambdaZStatistics output
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    lzOut <- lambdaZStatistics(Conc = conc, Time = time, numPoints = 4)
    
    test2 <- checkLambdaZStats(unlist(lzOut))
    checkTrue(is.null(test2), msg = " || TEST 2: lambdaZStatistics output" )
    
}
