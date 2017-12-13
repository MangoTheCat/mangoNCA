
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test testTrailPoints")

# tests the function testTrailPoints from checkdata.R

test_that("testTrailPoints", {

    # TEST 1 : standard check minpoints after Cmax

    test1 <- testTrailPoints(conc = 4:1, time = 1:4, minpoints = 3)
    expect_true( test1 )

    # TEST 2 : minpoints > number of points after Cmax

    test2 <- testTrailPoints(conc = 4:1, time = 1:4, minpoints = 4)
    expect_true( !test2 )

    # TEST 3 : All NA vector used

    NAVector <- rep(as.numeric(NA), 4)
    expect_error(testTrailPoints(conc = NAVector, time = 1:4, minpoints = 1),
            regex = "missing values in object")

    # TEST 4 : actual data set Patient 1007, validated against WinNonlin

    dataFile <- system.file("testdata", "wnl_data.csv", package = "mangoNCA")
    dataInput <- read.csv(dataFile)

    # Provide raw data for Patient 1007

    dataP1033 <- dataInput[dataInput$SUBJECT == 1033, ]

    test4 <- testTrailPoints(conc = dataP1033$CONC, dataP1033$TIME)
    expect_true( test4 )

    # TEST 5 : actual data set, validated against mangoNCA:::trapezium function

    Theoph1 <- subset(Theoph, Subject == 1)
    test5 <- testTrailPoints(conc = Theoph1$conc, time = Theoph1$Time)
    expect_true(test5 )

    # TEST 6 : zero length data frame

    zeroDf <- data.frame(conc = numeric(0), time = numeric(0),
        excpoints = logical(0))
    test6 <- suppressWarnings(testTrailPoints(conc = zeroDf$conc,
            time = zeroDf$time, excpoints = zeroDf$excpoints))
    expect_true(!test6 )

})


test_that("testTrailPoints error",  {

    # TEST E1 : Vector is non-numeric

    Theoph6 <- subset(Theoph, Subject == 6)
    expect_error(testTrailPoints(conc = Theoph6["conc"], time = Theoph6["Time"]),
        regex = "time is not a numeric vector")

    # TEST E2 : Differing length numeric vectors

    expect_error(testTrailPoints(conc = Theoph6[1:10, "conc"], time = Theoph6[1:9, "Time"]),
        regex = "lengths of time and conc do not match")

    # TEST E3 : NULL values

    expect_error(testTrailPoints(conc = NULL, time = NULL),
        regex = "time is not a numeric vector")

    # TEST E4 : Numeric matrix used, this should fail as inputs must be vectors

    expect_error(testTrailPoints(conc = matrix(4:1), time = 1:4, minpoints = 2),
        regex = "conc is not a numeric vector")

    # TEST E5 : minpoints length > 1, exception generated

    expect_error(testTrailPoints(conc = 5:1, time = 1:5, minpoints = 1:5),
        regex = "minpoints is not a numeric of length 1.  Value is: 1 2 3 4 5")

    # TEST E6 : Non-numeric vector used, exception generated

    expect_error(testTrailPoints(conc = c("BLQ", 8, 5, 4, 4), time = 1:5, minpoints = 3),
        regex = "conc is not a numeric vector")

})


test_that("stripTrailingZeros", {

    # TEST 1: simple example

    conc <- c(43, 23, 0, 14, 9, 0, 0)

    test1 <- stripTrailingZeros(conc = conc, time = (0:6) * 10, excpoints = FALSE)
    expect_equal(test1,
        data.frame(conc = conc[1:5], time = (0:4) * 10, excpoints = FALSE))

    # TEST 2: simple error example

    expect_error(stripTrailingZeros(conc = conc, time = (0:6) * 10,
            excpoints = c(FALSE, TRUE)),
        regex = "lengths of excpoints and conc do not match")

    # TEST 3: NAs are not allowed

    expect_error(stripTrailingZeros(conc = rep(NA, 4), time = 0:3,
            excpoints = rep(FALSE, 4)),
        regex = "missing values in object")
})
