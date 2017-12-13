# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 04/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.CLPred <- function()
{
    # TEST 1 : Contrived
    
    test1 <- CLPred(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, numPoints = 3, Dose = 38.07059031)
    checkEquals( test1, 1, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- CLPred(Conc = Theoph2$conc, Time = Theoph2$Time, numPoints = 3, Dose = Theoph2$Dose[1])
    checkEquals( test2, 0.04395041207, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
}

