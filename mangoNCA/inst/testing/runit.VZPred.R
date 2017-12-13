# SVN revision: $Rev: $
# Date of last change: $LastChangedDate:  $
# Last changed by: $LastChangedBy: $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.VZPred <- function()
{
    # TEST 1 : Contrived
    
    test1 <- VZPred(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, numPoints = 3, Dose = 13.19426)
    checkEquals( test1,
            1, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VZPred(Conc = Theoph2$conc, Time = Theoph2$Time, numPoints = 3, Dose = Theoph2$Dose[1])
    checkEquals( test2, 0.4239718, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
}

