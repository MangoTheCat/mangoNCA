 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

context("test VZObs")

test_that("VZObs", 
{
    # TEST 1 : Contrived
    
    test1 <- VZObs(conc = c(4, 9, 8, 6, 4:1, 1), time = 0:8, lamznpt = 3, dose = 133.0336246)
    expect_equal( test1,
            10, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VZObs(conc = Theoph2$conc, time = Theoph2$Time, lamznpt = 3, dose = Theoph2$Dose[1])
    expect_equal( test2, 0.4235660279, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
})

