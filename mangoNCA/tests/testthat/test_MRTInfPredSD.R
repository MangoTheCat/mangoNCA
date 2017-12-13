
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test MRTInfPredSD")

test_that("MRTInfPredSD",  {

    # TEST 1 : contrived data, standard example

    # AUMCInfPred = 67.93626429, AUCInfPred = 25.90989137
    test1 <- MRTInfPredSD(time = 0:6, conc = 7:1, lamznpt = 3, duration = 5, addt0 = FALSE)
    expect_equal( test1, 67.93626429 / 25.90989137 - 5/2, tol = 1e-8  )

    # TEST 2 : stock data set

    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfPredSD(time = Theoph2$Time, conc = Theoph2$conc, lamznpt = 3, duration = 2 )

    expect_equal(test2, 998.052942912161 / 100.1128269954017 - 1 , tol = 1e-5 )

    # TEST 3 : duration is less than 0 : NA should be returned.

    test3 <- MRTInfPredSD(time = Theoph2$Time, conc = Theoph2$conc, lamznpt = 3, duration = -1 )
    expect_true(is.na(test3))

})


test_that("MRTInfPredSD error",  {

    # TEST 1 : duration of infusion is of length > 1

    expect_error(MRTInfPredSD( time = 0:6, conc = 7:1, lamznpt = 3, duration = 1:2),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1 2" )

    # TEST 2 : duration of infusion is not numeric

    expect_error(MRTInfPredSD( time = 0:6, conc = 7:1, lamznpt = 3, duration = "1"),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1")

})
