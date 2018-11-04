
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test selectPoints")

# tests selectPoints from selectPoints.R

test_that("selectPoints working",  {

    # TEST 1 : contrived data set, does not pass regression comparison decision rules

    test1 <- suppressWarnings(selectPoints(conc = c(2, rep(1, 9)), time = 0:9))
    expect_equal( test1$lamznpt, NA_real_)

    # TEST 2 : contrived data set, does not pass regression comparison decision rules

    test2 <- suppressWarnings(selectPoints(conc = c(rep(2, 9), 0), time = 1:10))
    expect_equal( test2$lamznpt, NA_real_)

    # TEST 3 : contrived data set, multiple 0 elements at the end

    test3 <- suppressWarnings(selectPoints(conc = c(0, 8:2, 0, 0), time = 1:10))
    expect_equal(test3$lamznpt, 4)

    # TEST 4 : contrived data set, handle huge numbers

    test4 <- selectPoints(
        conc = c(0, 1.4e+12, 1.3e+08, 4.2e+04, 1.8e+03, 7.7e+02),
        time = c(0, 10, 100, 200, 400, 600))
    expect_equal( test4$lamznpt, 3 )

    # TEST 5 : contrived data set, handle tiny numbers

    test5 <- selectPoints(
        conc = c(0, 3.4e+03, 2.5e+01, 5.9e-01, 1.7e-03, 7.4e-10, 1.3e-11),
        time = c(0, 10, 100, 200, 400, 600, 1200))
    expect_equal( test5$lamznpt, 5 )

    # TEST 6 : actual data set

    Theoph1 <- subset(Theoph, Subject == 1)
    test6 <- selectPoints(conc = Theoph1$conc, time = Theoph1$Time, minpoints = 2)
    expect_equal(test6$lamznpt, 3 )

    # TEST 7 : actual data set Patient 1007, validated against WinNonlin

    dataFile <- system.file("testdata", "wnl_data.csv", package = "mangoNCA")
    dataInput <- read.csv(dataFile)

    # Provide raw data for Patient 1007

    dataP1007 <- dataInput[dataInput$SUBJECT == 1007, ]

    test7 <- selectPoints(conc = dataP1007$CONC, dataP1007$TIME)
    expect_equal(test7$lamznpt, 3)

    # TEST 8 : actual data set Patient 1037, validated against WinNonlin

    dataP1037 <- dataInput[dataInput$SUBJECT == 1037, ]

    test8 <- selectPoints(conc = dataP1037$CONC, dataP1037$TIME)
    expect_equal(test8$lamznpt, 4)

    # TEST 9 : fewer than minpoints rows after Cmax generates NA

    test9 <- suppressWarnings(selectPoints(conc = c(4, 5, 4:2), time = 0:4, minpoints = 60))
    expect_true( is.na( test9$lamznpt ))

    # TEST 10 : length 0 vectors

    test10 <- suppressWarnings(selectPoints(numeric(0), numeric(0)))
    expect_true(is.na( test10$lamznpt ))

    # TEST 11 : no calculable lambdaz due to adjR2 < 0.8

    c11 <- c(0, 3.94, 1.41, 1.71, 0.57)
    t11 <- c(0, 20.76923, 33.53846, 45.30769, 68.84615)
    test11 <- selectPoints(conc = c11, time = t11)
    expect_equal(test11$lamznpt, NA_real_)

    # calculable if minr2adj = 0.2 (i.e. 0.77 or less)
    test11 <- selectPoints(conc = c11, time = t11, minr2adj = 0.2)
    out11 <- structure(list(
            lamznpt = 3,
            result = structure(c(0.0286549076268164, 4.55712126884878,
                    0.771283104352291, 0.542566208704583, -0.878227250973398,
                    24.1894753103748, 33.53846, 68.84615, 3),
                .Names = c("Lambdaz", "intercept", "R2", "R2ADJ", "CORRXY",
                    "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt"))),
        .Names = c("lamznpt", "result"))
    expect_equal(test11, out11 )

    # TEST 12 : no calculable lambdaz due to LAMZHL > LAMZUL - LAMZLL

    t12 <- c(0, 0.25, 0.5, 1, 2, 3.52, 5.07, 7.07, 9.03)
    c12 <- c(0, 1.25, 3.96, 7.82, 9.72, 9.75, 8.57, 6.59, 6.11)
    test12 <- selectPoints(conc = c12, time = t12)
    expect_equal(test12$lamznpt, NA_real_)

    # calculable by shrinking LAMZHL
    test12 <- selectPoints(conc = c12, time = t12, numhalflife = 0.3)
    out12 <- structure(list(
            lamznpt = 3,
            result = structure(c(0.0855957971350336, 12.8319346693909,
                    0.91085804432271, 0.82171608864542, -0.954388832878251,
                    8.09791139004705, 5.07, 9.03, 3),
                .Names = c("Lambdaz", "intercept", "R2", "R2ADJ", "CORRXY",
                    "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt"))),
        .Names = c("lamznpt", "result"))
    expect_equal(test12, out12 )

    test12 <- selectPoints(conc = c12, time = t12, numhalflife = 0.3)
    expect_equal(test12, out12)

})

test_that("selectPoints error",  {

    # TEST E1 : non-numeric vector due to miscoded value in conc

    expect_error(selectPoints(conc = c(4, 65, 45, 17, 2, "BLA"), time = 0:5),
        regex = "conc is not a numeric vector")

    # TEST E2 : Check unsorted time values

    expect_error(selectPoints(time = c(1:4, 3:6), conc = c(2, 8, 9, 7, 5, 5, 4, 4)),
        regex = "Error in selectPoints: time is not ordered.  Actual value is 1 2 3 4 3 4 5 6")

    # TEST E3 : time is not a vector

    Theoph7 <- subset(Theoph, Subject == 7)

    expect_error(selectPoints(conc = Theoph7["conc"], time = Theoph7["time"]),
        regex = "Error in selectPoints: time is not a numeric vector")

    # TEST E4 : actual data set, invalid minpoints

    expect_error(selectPoints(conc = Theoph7$conc, time = Theoph7$Time, minpoints = 1),
        regex = "minpoints must be at least 2")

    # TEST E5 : NA present in concentration values

    expect_error(selectPoints(conc = c(3, NA, 3, 2, 1), time =  c(0, 1, 2, 3, 5)),
        regex = "missing values in object")

    # TEST E6 : NA present in time values

    expect_error(selectPoints(conc = c(4, 3, 2, 1.5, 1), time =  c(0, NA, 1, 2, 3.1)),
        regex = "missing values in object")
})

