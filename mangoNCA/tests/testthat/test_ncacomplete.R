
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test ncaComplete")

test_that("ncaComplete working",  {

    # TEST 1 : simple data working case

    test1 <- ncaComplete(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8,
        dose = 100, duration = 2, ROutput = shapeROutput)
    expect_true( class(test1) == "data.frame" )
    expect_equal( test1[["ERROR"]], 0 )
    expect_equal( test1[["R2ADJ"]], 0.9714937901 )

    # TEST 2 : Theoph data set

    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- ncaComplete(conc = Theoph2$conc, time = Theoph2$Time,
        dose = Theoph2$Dose[1], duration = 2, ROutput = shapeROutput)
    expect_equal( test2[["AUCIFO"]], 100.1734591, tol = 1e-06 )
    expect_equal( test2[["AUCIFP"]], 100.0643176, tol = 1e-06 )

    # TEST 3 : Patient 1007

    testFile <- system.file("testdata", "wnl_analysis.csv", package = "mangoNCA")

    testOutput <- read.csv(testFile)

    dataFile <- system.file("testdata", "wnl_data.csv", package = "mangoNCA")

    dataInput <- read.csv(dataFile)

    # Test Output from Patient 1007

    testP1007 <- testOutput[1, ]

    testP1007 <- unlist(testP1007)
    testP1007 <- unname(testP1007)
    testP1007 <- round(testP1007, 4)

    # Provide raw data for Patient 1007

    dataP1007 <- dataInput[dataInput$SUBJECT == 1007, ]

    # Run Test (Call function)

    test3 <- ncaComplete(conc = dataP1007$CONC, time = dataP1007$TIME,
        dose = dataP1007$DOSE[1] * 1000, duration = 1, ROutput = shapeROutput)

    expect_true( test3[, "ERROR"] == 0 )

    test3 <- unlist(test3)
    test3 <- unname(test3)
    test3 <- round(test3, 4)

    comparisonMatrix <- rbind(testP1007, test3)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]

    expect_equal( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08 )

    # TEST 4 : Patient 1033

    # Test Output from Patient 1033

    testP1033 <- testOutput[2, ]

    testP1033 <- unlist(testP1033)
    testP1033 <- unname(testP1033)
    testP1033 <- round(testP1033, 4)

    # Provide raw data for Patient 1033

    dataP1033 <- dataInput[dataInput$SUBJECT == 1033, ]

    # Run Test (Call function)

    test4 <- ncaComplete(conc = dataP1033$CONC, time = dataP1033$TIME,
        dose = dataP1033$DOSE[1] * 1000, duration = 1, addt0 = TRUE, ROutput = shapeROutput)

    test4 <- unlist(test4)
    test4 <- unname(test4)
    test4 <- round(test4, 4)

    comparisonMatrix <- rbind(testP1033, test4)
    comparisonMatrix <- comparisonMatrix[,
        apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]

    expect_equal( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08 )

    # TEST 5 : Patient 1037

    # Test Output from Patient 1037

    testP1037 <- testOutput[3, ]

    testP1037 <- unlist(testP1037)
    testP1037 <- unname(testP1037)
    testP1037 <- round(testP1037, 4)

    # Provide raw data for Patient 1037

    dataP1037 <- dataInput[dataInput$SUBJECT == 1037, ]

    # Run Test (Call function)

    test5 <- suppressWarnings(ncaComplete(conc = dataP1037$CONC, time = dataP1037$TIME,
        dose = dataP1037$DOSE[1] * 1000, duration = 1, addt0 = TRUE, ROutput = shapeROutput))

    test5 <- unlist(test5)
    test5 <- unname(test5)
    test5 <- round(test5, 4)

    comparisonMatrix <- rbind(testP1037, test5)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix,
        MARGIN = 2, function(x) !any( is.na( x ) ))]

    expect_equal( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08 )

    # TEST 6 : Clean Data (Patient 1007)

    test6 <- ncaComplete(conc = dataP1007$CONC, time = dataP1007$TIME,
        dose = dataP1007$DOSE[1] * 1000, duration = 1,
        excpoints = rep(FALSE, times = nrow(dataP1007)), ROutput = shapeROutput)

    expect_true( test6[, "ERROR"] == 0 )

    test6 <- unlist(test6)
    test6 <- unname(test6)
    test6 <- round(test6, 4)

    comparisonMatrix <- rbind(testP1007, test6)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix,
        MARGIN = 2,
        FUN = function(x) !any( is.na( x ) ))]

    expect_equal(comparisonMatrix[1, ],
        comparisonMatrix[2, ], tol = 1e-08 )

    # TEST 7: Check excpoints

    compRows <- c("R2ADJ", "INTERCEPT", "LAMZNPT", "R2", "CORRXY", "ERROR")
    test7a <- suppressWarnings(ncaComplete(conc = dataP1037$CONC[-4], time = dataP1037$TIME[-4],
        dose = dataP1037$DOSE[1] * 1000, duration = 0, addt0 = TRUE, ROutput = shapeROutput))
    test7b <- suppressWarnings(ncaComplete(conc = dataP1037$CONC, time = dataP1037$TIME,
        dose = dataP1037$DOSE[1] * 1000, duration = 0, excpoints = c(FALSE, FALSE, FALSE, TRUE, FALSE),
        addt0 = TRUE, ROutput = shapeROutput))
    expect_equal(test7a[compRows], test7b[compRows])

    # TEST 8: all zeros

    emptyDf <- c(rep(as.numeric(NA), 5), as.numeric(0))
    names(emptyDf) <- compRows
    emptyDf <- as.data.frame(as.list(emptyDf))
    test8 <- suppressWarnings(ncaComplete(conc = rep(0, 6), time = 0:5,
            dose = 10, duration = 1, ROutput = shapeROutput))
    expect_equal(test8[compRows], emptyDf)

    # TEST 9: mostly zeros

    test9 <- suppressWarnings(ncaComplete(conc = c(1e-5, rep(0, 6), 1e-6), time = 0:7,
            dose = 10, duration = 1, ROutput = shapeROutput))
    expect_equal(test9[compRows], emptyDf)

    # TEST 10: reversed time

    test10 <- ncaComplete(conc = c(4, 9, 8, 6, 4:1, 1), time = 8:0, dose = 100, duration = 2, ROutput = shapeROutput)
    expect_equal(test10[, "ERROR"],
                "Error in checkOrderedVector(time, description = \"time\", functionName = \"ncaComplete\") : \n  Error in ncaComplete: time is not ordered.  Actual value is 8 7 6 5 4 3 2 1 0\n")

    # TEST 11: Boomer Example
    # http://www.boomer.org/c/p3/c02/c0210.html

    test11 <- ncaComplete(conc = c(100, 71, 50, 35, 25, 12, 6.2, 3.1),
        time = c(0:4, 6, 8, 10), dose = 1, duration = 0, ROutput = shapeROutput)
    expect_equal(round(test11[, c("AUCLST", "AUCIFO")], 2),
        data.frame("AUCLST" = 283.0, "AUCIFO" = 291.9))

})

context("test chooseNumPointsAction")

test_that("chooseNumPointsAction",  {

    # TEST 1: auto selection

    conc <- c(2, 10, 8, 5, 3, 2)

    test1 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = as.numeric(NA))
    expect_equal(test1, list(ACTION = "auto", MINROWSFORLAMBDAZ = 3))

    # TEST 2: number selection

    test2 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = 4)
    expect_equal(test2, list(ACTION = "fixed", MINROWSFORLAMBDAZ = 3))

    # TEST 3: invalid number selection

    test3 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = 2)
    expect_equal(test3, list(ACTION = "none", MINROWSFORLAMBDAZ = 3))

    # TEST 4: section selection

    test4 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = as.numeric(NA),
        usepoints = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(test4, list(ACTION = "used", MINROWSFORLAMBDAZ = 3))

    # TEST 5: section selection

    expect_error(chooseNumPointsAction(conc = conc, time = 0:5,
        lamznpt = as.numeric(NA), usepoints = NA),
        regex = "lengths of Used Points and concentration do not match")

    # TEST 6: invalid auto selection

    conc <- c(2, 10, 8, 5, 11, 2)

    test6 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = as.numeric(NA))
    expect_equal(test6, list(ACTION = "none", MINROWSFORLAMBDAZ = 3))

    # TEST 7: used selection

    test7 <- chooseNumPointsAction(conc = conc, time = 0:5, lamznpt = as.numeric(NA),
        usepoints = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(test7, list(ACTION = "used", MINROWSFORLAMBDAZ = 3))

})
