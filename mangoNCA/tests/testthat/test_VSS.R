
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test VSS")

test_that("VSS", {

    # TEST 1 : Trivial Example

    test1 <- VSS(MRT = 100, CL = 0.5)
    expect_equal( test1, 50 )

    # TEST 3 : MRT is NA

    expect_error(VSS(MRT = NA, CL = 0.5),
        regex = "MRT is not a numeric of length 1")

})

