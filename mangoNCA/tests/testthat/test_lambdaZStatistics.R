
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test lambdaZStatistics")

# tests lambdaZStatistics in lambdaz.R

test_that("lambdaZStatistics",  {
    # TEST 1 : contrived data - concentration is exponential of linear data
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    # lambda z should be slope of the logged concentration - i.e. 
    test1 <- suppressWarnings(lambdaZStatistics(conc = conc, time = time, lamznpt = 4))
    expect_equal(test1, 
        list(
            LAMZ = 1,
            intercept = exp(2),
            R2 = 1,
            R2ADJ = 1,
            CORRXY = -1,
            LAMZHL = log(2),
            LAMZLL = 2,
            LAMZUL = 2.75,
            lamznpt = 4
        ))
    
    # TEST 2 : more sophisticated data set (subject 3 of Theoph)
   
    Theoph3 <- subset(Theoph, Subject == 3)
    test2 <- lambdaZStatistics(conc = Theoph3$conc, time = Theoph3$Time, lamznpt = 5)
    
    # NOTE: results calculated manually with the lm function
    
    expect_equal(test2, 
        list(
            LAMZ = 0.0945763, 
            intercept = exp(2.37478 ),
            R2 =  0.9907265,
            R2ADJ = 0.9876353,
            CORRXY = -0.9953525,
            LAMZHL = log(2) / 0.09458,
            LAMZLL = 5.08,
            LAMZUL = 24.17,
            lamznpt = 5
            ), tol = 1e-4) 
   
    # TEST 3 : data set with missing data
    
    Theoph3NA <- Theoph3
    Theoph3NA$Time[ c(1, 3, 5) ] <- NA
    Theoph3NA <- na.omit(Theoph3NA)
    
    # results should be the same as before, because the first 6 rows will be eliminated due to missing values
    test3 <- lambdaZStatistics(conc = Theoph3NA$conc, time = Theoph3NA$Time, lamznpt = 5)
    expect_equal(test2, test3)
    
    # emptyList : list with the components returned by lambdaZStatistics, with all values set to NA
    
    emptyList <- list( LAMZ = as.numeric(NA), intercept = as.numeric(NA),
            R2 =  as.numeric(NA), R2ADJ = as.numeric(NA), CORRXY = as.numeric(NA), 
            LAMZHL = as.numeric(NA), LAMZLL = as.numeric(NA),
            LAMZUL = as.numeric(NA), lamznpt = as.numeric(NA) )
        
    # TEST 4 : concentrations add up to 0
    test4 <- suppressWarnings(lambdaZStatistics(conc = c(1, 0, -1), time = 1:3, lamznpt = 3 ))
    expect_equal(test4, emptyList)
    
    # TEST 5 : 0 values at the end - these should be disregarded as lambda z should be calculated counting from
    # last measurable concentration
    
    # lambda z should be slope of the logged concentration - i.e. 
    
    test5 <- suppressWarnings(
        lambdaZStatistics(conc = c(conc, 0, 0), time = c(time, 3, 3.25), lamznpt = 4 ))
    expect_equal(test5, test1)
})

# tests lambdaZStatistics error handling in lambdaz.R

test_that("lambdaZStatistics error",  {
    
    # TEST 1 : standard check for numeric vectors and equality of length
    # Since it is obvious by inspection of the code that the check is performed, we only have one check
    
    expect_error(lambdaZStatistics(5:1, 1:4), 
        regex = "lengths of time and concentration do not match")
    
    # TEST 2 : check that lamznpt is numeric
    
    expect_error(lambdaZStatistics( 1:3, 1:3, lamznpt = "a"), 
        regex = "lamznpt is not a numeric of length 1.  Value is: a")

    # TEST 3 : check that lamznpt has only 1 value
    
    expect_error(lambdaZStatistics( 1:3, 1:3, lamznpt = 1:2 ), 
        regex = "lamznpt is not a numeric of length 1.  Value is: 1 2")
    
    # TEST 4 : check that lamznpt is greater than 0
    
    test4 <- lambdaZStatistics(conc = c(0, 10, 9, 7, 4, 3, 2), time = 0:6, lamznpt = 0)
    expect_true(is.na(test4$LAMZ))
    
    # TEST 5 : check that lamznpt is less than or equal to the number of time/concentration points
    
    test5 <- suppressWarnings(lambdaZStatistics(conc = c(0, 10, 9, 7, 4, 3, 2), time = 0:6, lamznpt = 8))
    expect_true(is.na(test5$R2ADJ))
    
    # TEST 6 : check that if data are not allowed, an empty list is returned
    # emptyList : list with the components returned by lambdaZStatistics, with all values set to NA
    
    emptyList <- list(LAMZ = as.numeric(NA), intercept = as.numeric(NA),
        R2 =  as.numeric(NA), R2ADJ = as.numeric(NA), CORRXY = as.numeric(NA),
        LAMZHL = as.numeric(NA), LAMZLL = as.numeric(NA),
        LAMZUL = as.numeric(NA), lamznpt = as.numeric(NA))
    
    test6 <- lambdaZStatistics(0:2, time =  2:4, lamznpt = 3)
    expect_equal(test6, emptyList)
    
    # TEST 7 : missing values in more sophisticated data set (subject 3 of Theoph)
   
    Theoph3NA <- subset(Theoph, Subject == 3)
    Theoph3NA[c(1, 3, 5), "Time"] <- NA
    expect_error(lambdaZStatistics(conc = Theoph3NA$conc, time = Theoph3NA$Time, lamznpt = 5), 
        regex = "missing values in object")
    
})
