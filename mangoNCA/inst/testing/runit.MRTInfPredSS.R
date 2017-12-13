# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 03/04/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


test.MRTPredInfSS <- function()
{
    if (!exists("MRTInfPredSS", mode = "function")) { MRTInfPredSS <- MangoNca:::MRTInfPredSS }
    
    # TODO check calcs
    # needs verification if required by tool
    
    # TEST 1 : contrived data, standard example
    
    # AUMCtau = 8; AUCtau = 5.5; tau = 2; dof = 5; AUCInfPred = 19.40989
    # (AUMCtau + tau * (AUCInfPred - AUCtau)) / AUCtau - dof/2 
    # [1] 4.012687273
     
    test1 <- MRTInfPredSS( Time = 0:6, Conc = 7:1, numPoints = 3, tau = 2, dof = 5, addT0 = FALSE)
    checkEquals( test1, 0.7349818947, tol = 1e-7, msg = " || TEST 1 : simple contrived data"  )
        
    # TEST 2 : stock data set
    
    Theoph2 <- subset(Theoph, Subject == 2)
    test2 <- MRTInfPredSS(Time = Theoph2$Time, Conc = Theoph2$conc, tau = 18, numPoints = 3, dof = 2 )

    # AUMCtau = 565; AUCtau = 82; tau = 18; dof = 3; AUCInfPred = 100
    # (AUMCtau + tau * (AUCInfPred - AUCtau)) / AUCtau - dof/2 
    # [1] 9.341463415
    
    checkEquals(test2,  9.711059145, tol = 1e-7, msg = " || TEST 2 : stock data set" )
    
    # TEST 3 : tau is greater than the largest element of Time, so extrapolation will be tested
    
    test3 <- MRTInfPredSS(Time = Theoph2$Time, Conc = Theoph2$conc, tau = 26, numPoints = 3, dof = 2 )

    checkEquals(test3, 8.634078618, tol = 1e-7, msg = " || TEST 3 : tau is greater than the largest element of Time ")
}


test.MRTPredInfSS_errorHandling <- function()
{
    if (!exists("MRTInfPredSS", mode = "function")) { MRTInfPredSS <- MangoNca:::MRTInfPredSS }
    
    # TEST 1 : duration of infusion is of length > 1  
    
    test1 <- try( MRTInfPredSS( Time = 0:6, Conc = 7:1, numPoints = 3, tau = 2, dof = 1:2), silent = TRUE)
    
    checkTrue(MangoNca:::isErrorWithMessage(test1,  "Error in checkSingleNumeric(dof, description = \"duration of infusion\") : \n  Error in  MRTInfPredSS :  duration of infusion is not a numeric of length 1.  Value is: 1 2\n" ), 
            msg = " || TEST 1 : duration of infusion is of length > 1"  )
    
    # TEST 2 : duration of infusion is not numeric
    
    test2 <- try( MRTInfPredSS( Time = 0:6, Conc = 7:1, numPoints = 3, tau = 2, dof = "1"), silent = TRUE)
    checkTrue(MangoNca:::isErrorWithMessage(test2, "Error in checkSingleNumeric(dof, description = \"duration of infusion\") : \n  Error in  MRTInfPredSS :  duration of infusion is not a numeric of length 1.  Value is: 1\n" ),
             msg = " || TEST 2 : duration of infusion is not numeric")
    
}
