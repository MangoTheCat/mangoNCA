# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.MRTInfPredSD <- function()
{
    # TEST 1 : contrived data, standard example
    
    # AUMCInfPred = 67.93626429, AUCInfPred = 25.90989137
    test1 <- MRTInfPredSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = 5, addT0 = FALSE)
    checkEquals( test1, 67.93626429 / 25.90989137 - 5/2, tol = 1e-8, msg = " || TEST 1 : simple contrived data"  )
    
    
    # TEST 2 : stock data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfPredSD(Time = Theoph2$Time, Conc = Theoph2$conc, numPoints = 3, Dof = 2 )
    
    checkEquals(test2, 998.052942912161 / 100.1128269954017 - 1 , tol = 1e-5, msg = " || TEST 2 : stock data set" )
    
    # TEST 3 : Dof is less than 0 : NA should be returned.
    
    test3 <- MRTInfPredSD(Time = Theoph2$Time, Conc = Theoph2$conc, numPoints = 3, Dof = -1 )
    checkTrue(is.na(test3), msg = " || TEST 3 : negative Dof causes NA to be returned")
                
}


test.MRTInfPredSD_errorHandling <- function()
{
    
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
        
    # TEST 1 : duration of infusion is of length > 1  
    
    test1 <- try( MRTInfPredSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = 1:2), silent = TRUE)
    
    checkTrue( isErrorWithMessage(test1, "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTInfPredSD :  Duration of infusion is not a numeric of length 1.  Value is: 1 2\n" ), 
            msg = " || TEST 1 : duration of infusion is of length > 1" )
    
    # TEST 2 : duration of infusion is not numeric
    
    test2 <- try( MRTInfPredSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = "1"), silent = TRUE)
    checkTrue( isErrorWithMessage(test2,  "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTInfPredSD :  Duration of infusion is not a numeric of length 1.  Value is: 1\n" ),
            msg = " || TEST 2 : duration of infusion is not numeric" )
    
}
