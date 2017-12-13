# SVN revision: $Rev: $
# Date of last change: $LastChangedDate:  $
# Last changed by: $LastChangedBy: $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.VZObs <- function()
{
    # TEST 1 : Contrived
    
    test1 <- VZObs(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, numPoints = 3, Dose = 133.0336246)
    checkEquals( test1,
            10, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VZObs(Conc = Theoph2$conc, Time = Theoph2$Time, numPoints = 3, Dose = Theoph2$Dose[1])
    checkEquals( test2, 0.4235660279, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
}

