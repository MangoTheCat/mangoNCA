
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test VSSPred")

test_that("VSSPred", 
{
    # TEST 1 : Contrived
    
    test1 <- VSSPred(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, lamznpt = 3, dose = 16.67643, duration = 2)
    expect_equal( test1, 1, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VSSPred(conc = Theoph2$conc, time = Theoph2$Time, lamznpt = 3, dose = Theoph2$Dose[1], duration = 2)
    expect_equal( test2, 0.3942036, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
})

