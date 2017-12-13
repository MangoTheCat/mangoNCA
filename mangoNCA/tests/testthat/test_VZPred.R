 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test VZPred")

test_that("VZPred", 
{
    # TEST 1 : Contrived
    
    test1 <- VZPred(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, lamznpt = 3, dose = 13.19426)
    expect_equal( test1,
            1, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VZPred(conc = Theoph2$conc, time = Theoph2$Time, lamznpt = 3, dose = Theoph2$Dose[1])
    expect_equal( test2, 0.4239718, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
})

