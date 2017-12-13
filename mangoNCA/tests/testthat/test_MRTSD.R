
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test MRTSD")

test_that("MRTSD working",  {

    # TEST 1 : trivial example

    test1 <- MRTSD(AUC = 2, AUMC = 4, duration = 2)
    expect_equal( test1, 1 )

    # TEST 2 : AUC > AUMC

    test2 <- MRTSD(AUC = 4, AUMC = 2, duration = 3)
    # is this desired behaviour or NA?
    expect_equal( test2, -1 )

    # TEST 3 : simple contrived data

    AUMCio <- AUCInfObs( time = 0:6, conc = 7:1, lamznpt = 3, calculation = "moment",
        addt0 = FALSE) # 67.23701252
    AUCio <- AUCInfObs( time = 0:6, conc = 7:1, lamznpt = 3, calculation = "standard",
        addt0 = FALSE) # 25.82047845
    mrtio <- AUMCio / AUCio - 5/2
    test3 <- MRTSD(AUC = AUCio, AUMC = AUMCio, duration = 5)
    expect_equal(test3, mrtio )

    # TEST 4 : WinNonlin Verification data 1108

    C1108 <- c(0L, 4L, 11L, 17L, 17L, 13L, 8L, 4L, 2L, 1L)

    T1108 <- c(0L, 1L, 4L, 9L, 16L, 25L, 36L, 49L, 64L, 81L)

    AUCL <- AUCLast(conc = C1108, time = T1108, addt0 = FALSE,
        inter = "Linear")

    AUMCL <- AUCLast(conc = C1108 * T1108, time = T1108, addt0 = FALSE,
        inter = "Linear")

    test4 <- MRTSD(AUC = AUCL, AUMC = AUMCL, duration = 1L)

    expect_equal(object = test4, expected = 24.74816327)
})


test_that("MRTSD error",  {

    # TEST 1 : duration of infusion is of length > 1

    expect_error(MRTSD(AUC = 5387, AUMC = 3456549, duration = 1:2),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1 2")

    # TEST 2 : duration of infusion is not numeric

    expect_error(MRTSD(AUC = 1387, AUMC = 1256549, duration = "1"),
        regex = "Duration of infusion is not a numeric of length 1.  Value is: 1")

    # TEST 3 : AUC and AUMC are of length > 1

    expect_error(MRTSD(AUC = c(7825, 3847), AUMC = c(315674, 959267), duration = 1),
        regex = "AUC is not a numeric of length 1.  Value is: 7825 3847")

})
