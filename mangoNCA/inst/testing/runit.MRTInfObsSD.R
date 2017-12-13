# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.MRTInfObsSD <- function()
{

    # TEST 1 : contrived data, standard example
    
    AUMCio <- AUCInfObs( Time = 0:6, Conc = 7:1, numPoints = 3, calculation = "moment", addT0 = FALSE) # 67.23701252
    AUCio <- AUCInfObs( Time = 0:6, Conc = 7:1, numPoints = 3, calculation = "standard", addT0 = FALSE) # 25.82047845
    mrto <- AUMCio / AUCio - 5/2
    checkEquals( 0.1040188465, mrto, tol = 1e-9, msg = " || TEST 1(a) : simple contrived data" )
    mrt <- MRTSD(AUC = 25.82047845, AUMC = 67.23701252, Dof = 5)
    checkEquals( 0.1040188465, mrto, tol = 1e-9, msg = " || TEST 1(b) : simple contrived data" )
    
    test1 <- MRTInfObsSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = 5, addT0 = FALSE)
    checkEquals( test1, mrto, tol = 1e-6, msg = " || TEST 1(c) : simple contrived data" )
    
    
    # TEST 2 : stock data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfObsSD(Time = Theoph2$Time, Conc = Theoph2$conc, numPoints = 3, Dof = 2 )
    
    checkEquals( test2, 8.992229665, tol = 1e-6, msg = " || TEST 2 : stock data set" )
    
    # TEST 3 : Dof is less than 0 : NA should be returned.
    
    test3 <- MRTInfObsSD(Time = Theoph2$Time, Conc = Theoph2$conc, numPoints = 3, Dof = -1 )
    checkTrue( is.na( test3 ), msg = " || TEST 3 : negative Dof causes NA to be returned" )
                
}


test.MRTInfObsSD_errorHandling <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : duration of infusion is of length > 1  
    
    test1 <- try( MRTInfObsSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = 1:2), silent = TRUE)
    
    checkTrue( isErrorWithMessage(test1, "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTInfObsSD :  Duration of infusion is not a numeric of length 1.  Value is: 1 2\n" ), 
            msg = " || TEST 1 : duration of infusion is of length > 1" )
    
    # TEST 2 : duration of infusion is not numeric
    
    test2 <- try( MRTInfObsSD( Time = 0:6, Conc = 7:1, numPoints = 3, Dof = "1"), silent = TRUE)
    checkTrue( isErrorWithMessage(test2,  "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTInfObsSD :  Duration of infusion is not a numeric of length 1.  Value is: 1\n" ),
            msg = " || TEST 2 : duration of infusion is not numeric" )
    
}
