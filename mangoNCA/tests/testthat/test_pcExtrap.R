#
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test pcExtrap")

# Tests the pcExtrap function

test_that("pcExrap working", {

    # TEST 1 : standard data set, no NA or 0 values

    test1 <-  pcExtrap(Whole = 100, Measured = 60)
    expect_equal(test1, 40)

    # TEST 2 : Whole is 0

    test2 <- pcExtrap(Whole = 0, Measured = 60)
    expect_equal(test2, as.numeric(NA))

    # TEST 3 : Measured is 0

    test3 <- pcExtrap(Whole = 1000, Measured = 0)
    expect_equal(test3, 100)

    # TEST 4 : Infinite Whole

    test4 <- pcExtrap(Whole = Inf, Measured = 10000)
    expect_equal(test4, as.numeric(NA))

    # TEST 5 : 0 length vector

    expect_error(pcExtrap(Whole = numeric(0), Measured = pi),
        regex = "Whole is not a numeric of length 1.  Value is: ")

    # TEST 6 : Complex Whole

    expect_error(pcExtrap(Whole = 1+0i, Measured = pi),
        regex = "Whole is not a numeric of length 1")

    # TEST 7 : basic data check

    expect_error(pcExtrap(Whole = "pi", Measured = pi),
        regex = "Whole is not a numeric of length 1.  Value is: pi")
})
