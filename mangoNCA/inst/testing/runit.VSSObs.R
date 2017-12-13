# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 19/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.VSSObs <- function()
{
    # TODO All testing
    # TEST 1 : Contrived
    
    test1 <- VSSObs(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, numPoints = 3, Dose = 16.36731493, Dof = 2)
    checkEquals( test1, 1, tol = 1e-06, msg = " || TEST 1 : Contrived data set\n" )

    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- VSSObs(Conc = Theoph2$conc, Time = Theoph2$Time, numPoints = 3, Dose = Theoph2$Dose[1], Dof = 2)
    checkEquals( test2, 0.3948339484, tol = 1e-06, msg = " || TEST 2 : Theoph data set\n" )
}

