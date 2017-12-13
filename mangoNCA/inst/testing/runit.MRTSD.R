# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 27/01/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.MRTSD <- function()
{

    # TEST 1 : trivial example
    
    test1 <- MRTSD(AUC = 2, AUMC = 4, Dof = 2)
    checkEquals( test1, 1, msg = " || TEST 1 : trivial example" )
    
    # TEST 2 : AUC > AUMC 
    
    test2 <- MRTSD(AUC = 4, AUMC = 2, Dof = 3)
    checkEquals( test2, -1, msg = " || TEST 2 : AUC > AUMC" ) # is this desired behaviour or NA?
    
    # TEST 3 : simple contrived data
    
    AUMCio <- AUCInfObs( Time = 0:6, Conc = 7:1, numPoints = 3, calculation = "moment", addT0 = FALSE) # 67.23701252
    AUCio <- AUCInfObs( Time = 0:6, Conc = 7:1, numPoints = 3, calculation = "standard", addT0 = FALSE) # 25.82047845
    mrtio <- AUMCio / AUCio - 5/2
    test3 <- MRTSD(AUC = AUCio, AUMC = AUMCio, Dof = 5)
    checkEquals( test3, mrtio, msg = " || TEST 3 : simple contrived data" )
    
}


test.MRTSD_errorHandling <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : duration of infusion is of length > 1  
    
    test1 <- try( MRTSD(AUC = 5387, AUMC = 3456549, Dof = 1:2), silent = TRUE)
    
    checkTrue( isErrorWithMessage(test1, "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTSD :  Duration of infusion is not a numeric of length 1.  Value is: 1 2\n" ), 
            msg = " || TEST 1 : duration of infusion is of length > 1" )
    
    # TEST 2 : duration of infusion is not numeric
    
    test2 <- try( MRTSD(AUC = 1387, AUMC = 1256549, Dof = "1"), silent = TRUE)
    checkTrue( isErrorWithMessage(test2,  "Error in checkSingleNumeric(Dof, description = \"Duration of infusion\") : \n  Error in  MRTSD :  Duration of infusion is not a numeric of length 1.  Value is: 1\n" ),
            msg = " || TEST 2 : duration of infusion is not numeric" )

    # TEST 3 : AUC and AUMC are of length > 1  
 
    test3 <- try( MRTSD(AUC = c(7825, 3847), AUMC = c(315674, 959267), Dof = 1), silent = TRUE)
    
    checkTrue( isErrorWithMessage(test3, "Error in checkSingleNumeric(AUC, description = \"AUC\") : \n  Error in  MRTSD :  AUC is not a numeric of length 1.  Value is: 7825 3847\n"), 
            msg = " || TEST 3 : duration of infusion is of length > 1" )
    
}
