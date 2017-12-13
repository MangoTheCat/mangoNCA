
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test MRTInfObsSD")

test_that("MRTInfObsSD", {

    # TEST 1 : contrived data, standard example

    AUMCio <- AUCInfObs( time = 0:6, conc = 7:1, lamznpt = 3, calculation = "moment", addt0 = FALSE) # 67.23701252
    AUCio <- AUCInfObs( time = 0:6, conc = 7:1, lamznpt = 3, calculation = "standard", addt0 = FALSE) # 25.82047845
    mrto <- AUMCio / AUCio - 5/2
    expect_equal( 0.1040188465, mrto, tol = 1e-9 )
    mrt <- MRTSD(AUC = 25.82047845, AUMC = 67.23701252, duration = 5)
    expect_equal( 0.1040188465, mrto, tol = 1e-9 )

    test1 <- MRTInfObsSD( time = 0:6, conc = 7:1, lamznpt = 3, duration = 5, addt0 = FALSE)
    expect_equal( test1, mrto, tol = 1e-6 )


    # TEST 2 : stock data set

    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfObsSD(time = Theoph2$Time, conc = Theoph2$conc, lamznpt = 3, duration = 2 )

    expect_equal( test2, 8.992229665, tol = 1e-6 )

    # TEST 3 : duration is less than 0 : NA should be returned.

    test3 <- MRTInfObsSD(time = Theoph2$Time, conc = Theoph2$conc, lamznpt = 3, duration = -1 )
    expect_true(is.na( test3 ))
})


test_that("MRTInfObsSD error",  {

    # TEST 1 : duration of infusion is of length > 1

    expect_error(MRTInfObsSD( time = 0:6, conc = 7:1,
        lamznpt = 3, duration = 1:2),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1 2" )

    # TEST 2 : duration of infusion is not numeric

    expect_error(MRTInfObsSD( time = 0:6, conc = 7:1, lamznpt = 3, duration = "1"),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1")
})
