
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test MRTInfPredSS")

test_that("MRTInfPredSS", {

    # TODO check calcs
    # needs verification if required by tool

    # TEST 1 : contrived data, standard example

    # AUMCtau = 8; AUCtau = 5.5; tau = 2; duration = 5; AUCInfPred = 19.40989
    # (AUMCtau + tau * (AUCInfPred - AUCtau)) / AUCtau - duration/2
    # [1] 4.012687273

    test1 <- MRTInfPredSS( time = 0:6, conc = 7:1, lamznpt = 3, tau = 2, duration = 5, addt0 = FALSE)
    expect_equal( test1, 0.7349818947, tol = 1e-7  )

    # TEST 2 : stock data set

    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfPredSS(time = Theoph2$Time, conc = Theoph2$conc, tau = 18, lamznpt = 3, duration = 2 )

    # AUMCtau = 565; AUCtau = 82; tau = 18; duration = 3; AUCInfPred = 100
    # (AUMCtau + tau * (AUCInfPred - AUCtau)) / AUCtau - duration/2
    # [1] 9.341463415

    expect_equal(test2,  9.711059145, tol = 1e-7 )

    # TEST 3 : tau is greater than the largest element of time, so extrapolation will be tested

    test3 <- MRTInfPredSS(time = Theoph2$Time, conc = Theoph2$conc, tau = 26, lamznpt = 3, duration = 2 )

    expect_equal(test3, 8.634078618, tol = 1e-7)
})


test_that("MRTInfPredSS error", {

    # TEST 1 : duration of infusion is of length > 1

    expect_error(MRTInfPredSS( time = 0:6, conc = 7:1, lamznpt = 3, tau = 2, duration = 1:2),
        regex = "duration of infusion is not a numeric of length 1.  Value is: 1 2")

    # TEST 2 : duration of infusion is not numeric

    expect_error(MRTInfPredSS( time = 0:6, conc = 7:1, lamznpt = 3, tau = 2, duration = "1"),
        regex = "duration of infusion is not a numeric of length 1.  Value is: 1")
})
