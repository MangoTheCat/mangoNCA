
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("CLObs")

test_that("CLObs", {
    # TEST 1 : Contrived
    
    test1 <- CLObs(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, lamznpt = 3, dose = 38.38539008)
    expect_equal( test1, 1, tol = 1e-06 )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- CLObs(conc = Theoph2$conc, time = Theoph2$Time, lamznpt = 3, dose = Theoph2$Dose[1])
    expect_equal( test2, 0.04390834788, tol = 1e-06 )
})

