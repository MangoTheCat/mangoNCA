
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test getNCAnalysis")

test_that("getNCAnalysis",  {

    # TEST 1 : simple data working case

    test1 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, dose = 100, duration = 2)
    expect_true( class(test1) == "data.frame" )
    expect_equal( unname(unlist(test1[, c("R2ADJ", "LAMZNPT", "AUCLST", "ERROR")])),
        c(0.9714937901, 7, 35.5, 0), tol = 1e-8 )

    # TEST 4 : Contrived data errors

    test4 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1), time = c(0:4, 3, 6:8), dose = 100, duration = 2)
    expect_true( test4[["ERROR"]] != 0 )
    expect_true( is.na(test4[["R2ADJ"]]) )
    expect_equal( test4[["ERROR"]], "Error in checkOrderedVector(time, description = \"time\", functionName = \"getNCAnalysis\") : \n  Error in getNCAnalysis: time is not ordered.  Actual value is 0 1 2 3 4 3 6 7 8\n")

    # TEST 5 : Contrived invalid Duration of Infusion

    test5 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, dose = 100, duration = rep(2, 9))
    expect_true( test5[["ERROR"]] != 0 )
    expect_true( is.na(test5[["R2ADJ"]]) )
    expect_equal( test5[["ERROR"]], "Error in checkSingleNumeric(duration, description = \"Duration of Infusion\",  : \n  Error in  getNCAnalysis :  Duration of Infusion is not a numeric of length 1.  Value is: 2 2 2 2 2 2 2 2 2\n")

    # TEST 6 : Contrived invalid Duration of Infusion and dose

    test6 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, dose = rep(100, 9), duration = rep(2, 9))
    expect_true( test6[["ERROR"]] != 0 )
    expect_true( is.na(test6[["R2ADJ"]]) )
    expect_equal( test6[["ERROR"]], "Error in checkSingleNumeric(dose, description = \"dose\", functionName = \"getNCAnalysis\") : \n  Error in  getNCAnalysis :  dose is not a numeric of length 1.  Value is: 100 100 100 100 100 100 100 100 100\nError in checkSingleNumeric(duration, description = \"Duration of Infusion\",  : \n  Error in  getNCAnalysis :  Duration of Infusion is not a numeric of length 1.  Value is: 2 2 2 2 2 2 2 2 2\n")

    # TEST 7 : Theoph data set

    Theoph2 <- subset(Theoph, Subject == 2)
    test7 <- getNCAnalysis(conc = Theoph2$conc, time = Theoph2$Time, dose = Theoph2$Dose[1], duration = 2)
    expect_equal( test7[["LAMZ"]], 0.1040864437, tol = 1e-06 )
    expect_equal( test7[["LAMZHL"]], 6.659341563, tol = 1e-06 )

    # TEST 8 : Contrived check errors

    test8 <- getNCAnalysis(conc = c(4, 9, 8), time = 0:2, dose = 100, duration = 2)
    expect_true(is(test8, "data.frame"))
    expect_equal( test8[["ERROR"]], 0 )
    expect_equal( test8[["CPEAK"]], NA_real_ )

    # TEST 9 : Contrived check no PeakTrough

    test9 <- getNCAnalysis(conc = c(4, 9, 8), time = 0:2, dose = 100, duration = 2)
    expect_equal( test9[["ERROR"]], 0 )

    # TEST 10 : Contrived check errors
    # TODO check behaviour re CMAX, CPEAK
    #test10 <- getNCAnalysis(conc = c(4, 9, 8), time = 0:2, dose = 100, duration = 2)
    #expect_true( class(test10) == "data.frame" )

    # TEST 11 : Contrived check errors

    test11 <- getNCAnalysis(conc = c(4, 9, 8), dose = 100, duration = 2)
    expect_true( class(test11) == "data.frame" )
    expect_equal( test11[, "ERROR", drop = TRUE],
        "Error in checkOrderedVector(time, description = \"time\", functionName = \"getNCAnalysis\") : \n  Error in getNCAnalysis: time is not a vector\nError in checkNumericSameLength(time, conc, \"time\", \"concentration\", functionName = \"getNCAnalysis\") : \n  Error in getNCAnalysis: time is not a numeric vector\n")

    # TEST 12 : Simulated data lamznpt excluded by checking rules

    time12 <- c(0, 1, 9, 16, 25, 64, 121, 144)
    conc12 <- c(0, 4247.07899, 588109.04837, 941402.82277, 1919159.79148, 1156619.4309, 24307.63924, 185612.15267)
    test12 <- getNCAnalysis(conc = conc12, time = time12, dose = 100, duration = 1)
    expect_equal(test12[, "ERROR", drop = TRUE], as.numeric(0))
    expect_true( !is.na(test12[["CMAX"]]) )
    # select some columns

    checkCols <- c("R2ADJ", "LAMZNPT", "LAMZ", "AUCLST", "ERROR")

    # TEST 13 : using lamznpt = 0 i.e. suppress terminal phase calculation

    test13 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 0)
    expect_equal(unname(unlist(test13[, checkCols])),
        c(NA, NA, NA, 35.5, 0) )

    # TEST 14 : using lamznpt = 1 i.e. not-allowed value for lamznpt
    # suppress terminal phase calculation

    test14 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 1)
    expect_equal(unname(unlist(test14[, checkCols])),
        c(NA, NA, NA, 35.5, 0) )

    # TEST 15 : using lamznpt = 2 i.e. not-allowed value for lamznpt
    # suppress terminal phase calculation

    test15 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 2)
    expect_equal(unname(unlist(test15[, checkCols])),
        c(NA, NA, NA, 35.5, 0) )

    # TEST 16 : using lamznpt = 3 i.e. allowed value for lamznpt
    # perform terminal phase calculation

    test16 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 3)
    expect_equal(unname(unlist(test16[, checkCols])),
        c(0.5, 3, 0.34657359028, 35.5, 0), tol = 1e-8 )

    # TEST 17 : using lamznpt = 7 i.e. allowed value for lamznpt
    # perform terminal phase calculation

    test17 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 7)
    expect_equal(test17, test1 )

    # TEST 18 : using lamznpt = 8 i.e. lamznpt == Cmax,
    # perform terminal phase calculation

    test18 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
        time = 0:8, dose = 100, duration = 2, lamznpt = 8)
    expect_equal(unname(unlist(test18[, checkCols])),
        c(0.966253286928, 8, 0.349539270098, 35.5, 0), to = 1e-8 )

    # TEST 19 : using lamznpt = 10 i.e. lamznpt > length(time)
    # suppress terminal phase calculation

    test19 <- suppressWarnings(
        getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1),
            time = 0:8, dose = 100, duration = 2, lamznpt = 10))
    expect_equal(unname(unlist(test19[, checkCols])),
        c(NA, NA, NA, 35.5, 0) )

    # TEST 20: checking excpoints auto

    conc <- c(1, 9, 8, 6, 4, 3)
    compar <- getNCAnalysis(conc = conc, time = 0:5, dose = 1, duration = 1)

    test20 <- getNCAnalysis(conc = conc, time = 0:5, dose = 1, duration = 1,
        excpoints = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
    expect_true(!identical(compar, test20))
    expect_equal(unname(unlist(test20[, checkCols])),
        c(0.9961019, 3.0000000, 0.3297474, 29.0000000, 0.0000000),
        tol = 1e-6)

    # TEST 21: checking excpoints fixed

    t21 <- getNCAnalysis(conc = conc[-5],
        time = (0:5)[-5], dose = 1, duration = 1, lamznpt = 4)

    test21 <- getNCAnalysis(conc = conc, time = 0:5, dose = 1, duration = 1,
        excpoints = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), lamznpt = 4)
    expect_true(!identical(compar, test21))
    expect_true(identical(t21[, checkCols[-4]], test21[, checkCols[-4]]))

    # TEST 22: checking excpoints none

    test22 <- getNCAnalysis(conc = conc, time = 0:5, dose = 1, duration = 1,
        excpoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
    expect_equal(unname(unlist(test22[, checkCols])), c(NA, NA, NA, 29, 0))

    # TEST 23: checking usepoints

    conc <- c(1, 9, 8, 6, 4, 3, 3, 2)

    test23 <- getNCAnalysis(conc = conc, time = 0:7, dose = 1, duration = 1,
        usepoints = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
    expect_equal(unname(unlist(test20[, checkCols])),
        c(0.9961019, 3.0000000, 0.3297474, 29.0000000, 0.0000000),
        tol = 1e-6)

    # TEST 24: checking usepoints excpoints

    compar <- suppressWarnings(getNCAnalysis(conc = c(10, 8, 6, 4), time = 1:4,
        dose = 1, duration = 1, addt0 = TRUE))

    # force cmax to be included
    test24 <- getNCAnalysis(conc = conc, time = 0:7, dose = 1, duration = 1,
        excpoints = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
        usepoints = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
    expect_true(!identical(test24[, checkCols[-4]], compar[, checkCols[-4]]))

    # exclude CMAX
    test24 <- getNCAnalysis(conc = conc, time = 0:7, dose = 1, duration = 1,
        excpoints = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
        usepoints = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))

    expect_equal(test24[, checkCols[-4]], compar[, checkCols[-4]])

    # TEST 25: checking usepoints none

    test25 <- getNCAnalysis(conc = conc, time = 0:7, dose = 1, duration = 1,
        usepoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
    expect_equal(unname(unlist(test25[, checkCols])), c(NA, NA, NA, 34.5, 0.0))

    # TEST 26: missing dose

    test26 <- getNCAnalysis(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, duration = 1)
    expect_equal(test26[, "ERROR"],
        "Error in checkSingleNumeric(dose, description = \"dose\", functionName = \"getNCAnalysis\") : \n  Error in  getNCAnalysis :  dose is not a numeric of length 1.  Value is: \n NULL\n")

    # TEST 27: usepoints and lamznpt

    test27 <- getNCAnalysis(conc = conc, time = 0:7, dose = 1, duration = 1,
        usepoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
        lamznpt = 4)
    expect_equal(test27[, length(test27)],
        "usepoints was provided to getNCAnalysis in addition to lamznpt")

    # TEST 28: inter "Lin Up Log Down", lamznpt 3

    #c28 <- c(0, 18.01, 16.96, 14.16, 10.49, 5.803, 1.881, 0.715, 0.191, 0.096, 0.052, 0.028, 0.015, 0.008)
    #t28 <- c(0, 0.0167, 0.033, 0.083, 0.167, 0.33, 0.67, 1, 2, 4, 6, 8, 10, 12)
    #testFile <- system.file("data", "wnl_analysis_linlog.csv", package = "mangoNCA")
    #testData <- read.csv(testFile)
    #test28 <- getNCAnalysis(conc = c28, time = t28, dose = 10 * 1000, duration = 0, lamznpt = 3, inter = "Linear Log")
    #expect_equal(test28[1, ], testData[1, ], tol = 1e-08 )

    # TEST 29: exp vals cf integration

    test29 <- suppressWarnings(getNCAnalysis(conc = 25 * exp(-(1/pi) * (0:5)^2),
        time = (0:5)^2, dose = 1, duration = 1, inter = "Lin up Log down"))
    auc0Inf <- 25 / (1/pi)
    auc0Last <- auc0Inf - 25 / (1/pi) * exp(-(1/pi) * 5^2)
    expect_equal(unlist(test29[, c("AUCIFP", "AUCLST")]),
        c(AUCIFP = auc0Inf, AUCLST = auc0Last))

    # TEST 30: terminal phase gradient too long

    test30 <- getNCAnalysis(conc = c(50L, 60L, 75L, 62L, 66L, 71L, 60L, 50L, 41L, 38L),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1)
    expect_equal(unlist(test30[checkCols]),
        c("R2ADJ" = NA, "LAMZNPT" = NA, "LAMZ" = NA,
            "AUCLST" = 3267.5, "ERROR" = 0))

    # TEST 31: terminal phase gradient too long numhalflife = 0

    test31 <- getNCAnalysis(conc = c(50L, 60L, 75L, 62L, 66L, 71L, 60L, 50L, 41L, 38L),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1,
        numhalflife = 0)
    expect_equal(unlist(test31[checkCols]),
        c("R2ADJ" = 0.965625092336522, "LAMZNPT" = 5, "LAMZ" = 0.0144487342881538,
            "AUCLST" = 3267.5, "ERROR" = 0))

    # TEST 32: terminal phase too noisy

    test32 <- getNCAnalysis(conc = c(50L, 60L, 75L, 58L, 68L, 49L, 36L, 42L, 1L, 13L),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1)
    expect_equal(unlist(test32[checkCols]),
        c("R2ADJ" = NA, "LAMZNPT" = NA, "LAMZ" = NA,
            "AUCLST" = 2125.5, "ERROR" = 0))

    # TEST 33: terminal phase too noisy minr2adj = 0

    test33 <- getNCAnalysis(conc = c(50L, 60L, 75L, 58L, 68L, 49L, 36L, 42L, 1L, 13L),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1,
        minr2adj = 0)
    expect_equal(unlist(test33[checkCols]),
        c("R2ADJ" = 0.388214672191714, "LAMZNPT" = 7, "LAMZ" = 0.0498845157403781,
            "AUCLST" = 2125.5, "ERROR" = 0))

    # TEST 34: terminal phase too noisy

    test34 <- getNCAnalysis(conc = c(111.152, 98.806, 94.036, 20.567, 28.346, 30.302, 2.447, 14.967, 6.176, 0.819),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1, minr2adj = 0.6)
    expect_equal(unlist(test34[checkCols]),
        c("R2ADJ" = 0.9031663967954, "LAMZNPT" = 3, "LAMZ" = 0.12106329052817,
            "AUCLST" = 1052.729, "ERROR" = 0))

    # TEST 35: terminal phase too noisy minr2adj = 0

    test35 <- getNCAnalysis(conc = c(111.152, 98.806, 94.036, 20.567, 28.346, 30.302, 2.447, 14.967, 6.176, 0.819),
        time = c(0L, 1L, 2L, 4L, 8L, 16L, 24L, 36L, 48L, 60L), dose = 1, duration = 1,
        minr2adj = 0.6, maxdiffrsq = 0.25)
    expect_equal(unlist(test35[checkCols]),
        c("R2ADJ" = 0.668712209847011, "LAMZNPT" = 9, "LAMZ" = 0.0625003368927666,
            "AUCLST" = 1052.729, "ERROR" = 0))

})
