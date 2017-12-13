# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.ncaPeakTrough <- function()
{
    load(system.file(package = "MangoNca", "data", "shapeROutput.RData"))
    
    # TEST 1 : Contrived check no errors
    
    test1 <- ncaPeakTrough(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 10, Dof = 2, PeakTrough = c(1, 0, 2, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test1[["ROutput_Error"]], 0, msg = " || TEST 1(a) : No Errors, contrived data set\n" )
    checkEquals( test1[["ROutput_Peak"]], 8, msg = " || TEST 1(b) : Contrived data set\n" )

    # TEST 2 : Contrived check NAs in PeakTrough
    
    test2 <- ncaPeakTrough(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 10, Dof = 2, PeakTrough = c(1, NA, NA, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test2[["ROutput_Error"]], 0, msg = " || TEST 2(a) : No Errors, contrived data set with NAs\n" )
    checkEquals( test2[["ROutput_Trough"]], 4, msg = " || TEST 2(b) : Contrived data set\n" )
    checkTrue( is.na(test2[["ROutput_Peak"]]), msg = " || TEST 2(c) : Contrived data set, missing code returns NA\n" )

    # TEST 3 : Contrived check errors in PeakTrough
    
    test3 <- ncaPeakTrough(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 10, Dof = 2, PeakTrough = c(1, 2, 2, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test3[["ROutput_Error"]], "  Error in checkPeakTrough(PeakTrough, functionName = \"ncaPeakTrough\") : \n  Error in ncaPeakTrough: PeakTrough is miscoded.  Actual value is 1 2 2 0 0 0 0 0 0\n   ", 
        msg = " || TEST 3(a) : Errors in PeakTrough with contrived data set\n" )
    checkTrue( is.na(test3[["ROutput_Peak"]]), msg = " || TEST 3(c) : Contrived data set, Error returns NA for Peak\n" )
    
    ## TEST 4 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2) 
 
    test4 <- ncaPeakTrough(Conc = Theoph2$conc, Time = Theoph2$Time, 
        Dose = Theoph2$Dose[1], Dof = 1, PeakTrough = c(1, rep(0, 3), 2, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test4[["ROutput_Trough"]], 0, msg = " || TEST 4 : Theoph data set\n" )
    
    ## TEST 5 : Theoph data, miscoded Trough
    
    test5 <- ncaPeakTrough(Conc = Theoph2$conc, Time = Theoph2$Time, 
        Dose = Theoph2$Dose[1], Dof = 1, PeakTrough = c(-1, rep(0, 3), 2, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test5[["ROutput_Error"]], 0, msg = " || TEST 5(a) : Theoph data set, miscoded Trough no error\n" )    
    checkTrue( is.na(test5[["ROutput_Trough"]]), msg = " || TEST 5(b) : Theoph data set, miscoded Trough missing\n" )   
    
    ## TEST 6 : Theoph data set, time out of sequence
    
    test6 <- ncaPeakTrough(Conc = Theoph2$conc, Time = rev(Theoph2$Time), 
        Dose = Theoph2$Dose[1], Dof = 1, PeakTrough = c(1, rep(0, 3), 2, rep(0, 6)), ROutput = shapeROutput)
    checkEquals( test6[["ROutput_Error"]], 
        "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"ncaPeakTrough\") : \n  Error in ncaPeakTrough: Time is not ordered.  Actual value is 24.3 12 9 7.03 5.02 3.5 1.92 1 0.52 0.27 0\n     ", 
        msg = " || TEST 6 : Theoph data set, time out of sequence \n" )
    
}

