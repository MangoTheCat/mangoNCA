
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("cleanconctime")

# tests the function cleanconctime from checkdata.R

test_that("cleanconctime",  {
    
    # TEST 1 : check clean values are converted to data frame
    
    test1 <- cleanconctime(conc = 5:1, time = 0:4, addt0 = TRUE)
    expect_equal( test1[, c("conc", "time")], data.frame(conc = 5:1, time = 0:4) )
    
    # TEST 2 : check NAs are removed

    test2 <- cleanconctime(conc = c(0, NA, 3:1), time = c(0, 1:3, NA), addt0 = TRUE)
    expect_equal( test2[["conc"]], c(0, 3, 2) ) 
    
    # TEST 3 : All NA vector used
    
    NAVector <- rep(as.numeric(NA), 5)
    zeroDf <- data.frame(conc = numeric(0), time = numeric(0), excpoints = logical(0))
    test3 <- suppressWarnings(try(cleanconctime(conc = NAVector, time = 0:4, addt0 = TRUE), silent = TRUE))
    check3 <- identical(storage.mode(test3), storage.mode(zeroDf)) && identical(names(test3), names(zeroDf)) && identical(nrow(test3), nrow(zeroDf))
    expect_true(check3)
    
    # TEST 4 : check clean values adding T = 0
    
    test4 <- suppressWarnings(cleanconctime(conc = 5:1, time = 1:5, addt0 = TRUE))
    expect_equal( test4[["time"]], 0:5 )
    expect_equal( test4[["conc"]], c(0, 5:1) )
    
    # TEST 5 : minpoints length > 1, exception generated
    
    expect_error(cleanconctime(conc = 5:1, time = 1:5, addt0 = c(FALSE, TRUE)), 
        regex = "is not a logical of length 1")
    
    # TEST 6 : Non-numeric vector used, exception generated
    
    expect_error(cleanconctime(conc = c("BLQ", 8, 5, 4, 4), time = 1:5, addt0 = TRUE), 
        regex = "conc is not a numeric vector")
    
    # TEST 7 : conc and time are not numeric vectors
    
    Theoph10 <- Theoph[Theoph$Subject == 10, ]
    expect_error(cleanconctime(conc = Theoph10["conc"], 
        time = Theoph10["Time"], addt0 = TRUE), 
        regex = "time is not a numeric vector")
    

    # TEST 8 : Differing length numeric vectors
    
    expect_error(cleanconctime(conc = 1:5, time = 0:3, addt0 = TRUE), 
        regex = "lengths of time and conc do not match")
    
    # TEST 9 : NULL should not work!
    
    expect_error(cleanconctime(conc = 1:5, time = NULL, addt0 = TRUE), 
        "Error in cleanconctime: time is not a numeric vector")
    
    # TEST 10 : check clean values are converted to data frame when addt0 FALSE
    
    test10 <- cleanconctime(conc = 5:1, time = 0:4, addt0 = FALSE)
    expect_equal( test10[, c("conc", "time")], data.frame(conc = 5:1, time = 0:4) )

    # TEST 11 : exception with NA when addt0 = FALSE
    
    expect_error(cleanconctime(conc = c(5:3, NA, 1), time = 1:5, addt0 = FALSE), 
        regex = "missing values in object")
    
    # TEST 12 : exception with missing T = 0 when addt0 = FALSE
    
    expect_error(cleanconctime(conc = c(5:3, 1.5, 1), time = 1:5, addt0 = FALSE), 
        regex = "Missing row where T = 0")
    
    # TEST 13 : exception with missing T = 0 when addt0 = FALSE
    
    expect_error(cleanconctime(conc = c(5:3, -1, 1), 
        time = 0:4, addt0 = FALSE), 
        regex = "values of conc < 0 in cleanData")
    
    # TEST 14: check clean values are converted to a data frame with usepoints
    
    test14 <- cleanconctime(conc = 10:5, time = 0:5, usepoints = rep(TRUE, 6), addt0 = TRUE)
    expect_equal( test14, data.frame(conc = 10:5, time = 0:5, excpoints = FALSE, usepoints = TRUE))
    
    # TEST 15: check clean values are converted to a data frame with excpoints
    
    test15 <- cleanconctime(conc = 5:1, time = 0:4, excpoints = rep(TRUE, 5), addt0 = TRUE)
    expect_equal( test15, data.frame(conc = 5:1, time = 0:4, excpoints = TRUE) )
    
    # TEST 16: check clean values are converted to a data frame with usepoints and excpoints
    
    test16 <- cleanconctime(conc = Theoph10$conc, time = Theoph10$Time, 
        usepoints = Theoph10$Time > 3 & Theoph10$Time < 20, 
        excpoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 4)), addt0 = TRUE)
    expect_equal( test16, data.frame(conc = Theoph10$conc, time = Theoph10$Time, 
        excpoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 4)), 
        usepoints = Theoph10$Time > 3 & Theoph10$Time < 20))
    
    # TEST 17: check missing T0 values are converted to a data frame with usepoints and excpoints
    
    Theoph9 <- Theoph[Theoph$Subject == 9 & Theoph$Time != 0, ]
    
    test17 <- suppressWarnings(cleanconctime(conc = Theoph9$conc, time = Theoph9$Time, 
        usepoints = Theoph9$Time > 3 & Theoph9$Time < 20, excpoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 3)), addt0 = TRUE))
    expect_equal( test17, data.frame(conc = c(0, Theoph9$conc), time = c(0, Theoph9$Time), 
        usepoints = c(FALSE, Theoph9$Time > 3 & Theoph9$Time < 20), 
        excpoints = c(TRUE, rep(FALSE, 6), TRUE, rep(FALSE, 3))))
    
    # TEST 18: check error when missing T0 values with usepoints and excpoints
    
    expect_error(cleanconctime(conc = Theoph9$conc, time = Theoph9$Time, 
        usepoints = Theoph9$Time > 3 & Theoph9$Time < 20, 
        excpoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 3)), addt0 = FALSE), 
            regex = "Missing row where T = 0")
    
})

