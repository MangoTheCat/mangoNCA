#
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test VZ")

test_that("VZ",  {

    # TEST 1 : Trivial Example

    test1 <- VZ(lambdaz = 0.001, AUCInf = 1000, dose = 6)
    expect_equal( test1, 6 )

    # TEST 2 : Trivial Example unsafe

    test2 <- VZ(lambdaz = 0.002, AUCInf = 2000, dose = 40)
    expect_equal( test2, 10 )

    # TEST 3 : lambdaz is NA

    expect_error(VZ(lambdaz = NA, AUCInf = 1000, dose = 10),
            regex = "lambdaz is not a numeric of length 1.  Value is: NA")

})
