# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 25/10/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.ncaAnalysis <- function()
{
    # TEST 1 : simple data working case
    
    test1 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2)
    checkTrue( class(test1) == "data.frame" , msg = " || TEST 1(a) simple data working case data frame\n")
    checkEquals( unname(unlist(test1[, c("ROutput_adjr2", "ROutput_numPoints", "ROutput_AUCLast", "ROutput_Error")])), 
        c(0.9714937901, 7, 35.5, 0), tol = 1e-8, msg = " || TEST 1(b) : simple data working case\n" )

    # TEST 2 : Contrived check PeakTrough
    
    test2 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, PeakTrough = c(1, 0, 2, rep(0, 6)))
    checkEquals( test2[1, "ROutput_Peak"], 8, msg = " || TEST 2(a) : PeakTrough returned\n" )
    checkEquals( test2[["ROutput_AUMCLast"]], 97, tol = 1e-6, msg = " || TEST 2(b) : Test AUMCLast from contrived data\n" )

    # TEST 3 : Contrived invalid PeakTrough
    
    test3 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, PeakTrough = 0)
    checkTrue( test3[["ROutput_Error"]] != 0, msg = " || TEST 3(a) : Errors due to Invalid PeakTrough\n" )
    checkTrue( is.na(test3[["ROutput_adjr2"]]), msg = " || TEST 3(b) : Error causes values to be returned NA\n" )
    checkEquals( test3[["ROutput_Error"]], "Error in checkNumericSameLength(Time, PeakTrough, \"Time\", \"Peak/Trough\",  : \n  Error in ncaAnalysis:  lengths of Time and Peak/Trough do not match\n", 
        msg = " || TEST 3(c) : Error String Returned in ROutput_Error column of test3\n" )

    # TEST 4 : Contrived data errors
    
    test4 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = c(0:4, 3, 6:8), Dose = 100, Dof = 2)
    checkTrue( test4[["ROutput_Error"]] != 0, msg = " || TEST 4(a) : Errors due to data errors\n" )
    checkTrue( is.na(test4[["ROutput_adjr2"]]), msg = " || TEST 4(b) : Error causes values to be returned NA\n" )
    checkEquals( test4[["ROutput_Error"]], "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"ncaAnalysis\") : \n  Error in ncaAnalysis: Time is not ordered.  Actual value is 0 1 2 3 4 3 6 7 8\n", 
        msg = " || TEST 4(c) : Error String Returned in ROutput_Error column of test4\n" )
    
    # TEST 5 : Contrived invalid Duration of Infusion
    
    test5 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = rep(2, 9))
    checkTrue( test5[["ROutput_Error"]] != 0, msg = " || TEST 5(a) : Errors due to Invalid Duration of Infusion\n" )
    checkTrue( is.na(test5[["ROutput_adjr2"]]), msg = " || TEST 5(b) : Error causes values to be returned NA\n" )
    checkEquals( test5[["ROutput_Error"]], "Error in checkSingleNumeric(Dof, description = \"Duration of Infusion\",  : \n  Error in  ncaAnalysis :  Duration of Infusion is not a numeric of length 1.  Value is: 2 2 2 2 2 2 2 2 2\n", 
        msg = " || TEST 5(c) : Error String Returned in ROutput_Error column of test5\n" )
        
    # TEST 6 : Contrived invalid Duration of Infusion and Dose
    
    test6 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = rep(100, 9), Dof = rep(2, 9))
    checkTrue( test6[["ROutput_Error"]] != 0, msg = " || TEST 6(a) : Errors due to Invalid Duration of Infusion and Dose\n" )
    checkTrue( is.na(test6[["ROutput_adjr2"]]), msg = " || TEST 6(b) : Error causes values to be returned NA\n" )
    checkEquals( test6[["ROutput_Error"]], "Error in checkSingleNumeric(Dose, description = \"Dose\", \"ncaAnalysis\") : \n  Error in  ncaAnalysis :  Dose is not a numeric of length 1.  Value is: 100 100 100 100 100 100 100 100 100\nError in checkSingleNumeric(Dof, description = \"Duration of Infusion\",  : \n  Error in  ncaAnalysis :  Duration of Infusion is not a numeric of length 1.  Value is: 2 2 2 2 2 2 2 2 2\n", 
        msg = " || TEST 6(c) : Error String Returned in ROutput_Error column of test6\n" )
    
    # TEST 7 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test7 <- ncaAnalysis(Conc = Theoph2$conc, Time = Theoph2$Time, Dose = Theoph2$Dose[1], Dof = 2)
    checkEquals( test7[["ROutput_LambdaZ"]], 0.1040864437, tol = 1e-06, msg = " || TEST 7(a) : Test LambdaZ from Theoph data set\n" )
    checkEquals( test7[["ROutput_HalfLife"]], 6.659341563, tol = 1e-06, msg = " || TEST 7(b) : Test HalfLife from Theoph data set\n" )
    
    # TEST 8 : Contrived check errors
    
    test8 <- ncaAnalysis(Conc = c(4, 9, 8), Time = 0:2, Dose = 100, Dof = 2, PeakTrough = c(1, 0, 2))
    checkTrue(is(test8, "data.frame"), msg = " || TEST 8(a) Return object is data frame\n")
    checkEquals( test8[["ROutput_Error"]], 0, msg = " || TEST 8(b) : No Errors with contrived data\n" )
    checkEquals( test8[["ROutput_Peak"]], 8, msg = " || TEST 8(c) : Test Peak from contrived data\n" )
    
    # TEST 9 : Contrived check no PeakTrough
    
    test9 <- ncaAnalysis(Conc = c(4, 9, 8), Time = 0:2, Dose = 100, Dof = 2)
    checkEquals( test9[["ROutput_Error"]], 0, msg = " || TEST 9(a) : No Errors with contrived data\n" )
    checkTrue( is.na(test9[["ROutput_Peak"]]), msg = " || TEST 9(b) : No Peak or Trough values assed to function\n" )
    
    # TEST 10 : Contrived check errors
    
    test10 <- ncaAnalysis(Conc = c(4, 9, 8), Time = 0:2, Dose = 100, Dof = 2, PeakTrough = 1:2)
    checkTrue( class(test10) == "data.frame" , msg = " || TEST 10(a) Return object is data frame\n")
    checkEquals( test10[1, "ROutput_Error"], "Error in checkNumericSameLength(Time, PeakTrough, \"Time\", \"Peak/Trough\",  : \n  Error in ncaAnalysis:  lengths of Time and Peak/Trough do not match\n", 
        msg = " || TEST 10(b) : No Errors with contrived data\n" )
    
    # TEST 11 : Contrived check errors
    
    test11 <- ncaAnalysis(Conc = c(4, 9, 8), Dose = 100, Dof = 2)
    checkTrue( class(test11) == "data.frame" , msg = " || TEST 10(a) Return object is data frame\n")
    checkEquals( test11[, "ROutput_Error", drop = TRUE], 
        "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"ncaAnalysis\") : \n  Error in ncaAnalysis: Time is not a vector\nError in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\", \"ncaAnalysis\") : \n  Error in ncaAnalysis: Time is not a numeric vector\n", 
        msg = " || TEST 11(b) : Missing Time error trapped by ncaAnalysis\n" )
    
    # TEST 12 : Simulated data numPoints excluded by checking rules
    
    Time12 <- c(0, 1, 9, 16, 25, 64, 121, 144)
    Conc12 <- c(0, 4247.07899, 588109.04837, 941402.82277, 1919159.79148, 1156619.4309, 24307.63924, 185612.15267)
    test12 <- ncaAnalysis(Conc = Conc12, Time = Time12, Dose = 100, Dof = 1)
    checkEquals(test12[, "ROutput_Error", drop = TRUE], as.numeric(0), msg = " || TEST 12a : Simulated data no error when numPoints excluded by checking rules")
    checkTrue( !is.na(test12[["ROutput_Cmax"]]), msg = " || TEST 12b : Simulated data numPoints excluded by checking rules" )

    # select some columns
    
    checkCols <- c("ROutput_adjr2", "ROutput_numPoints", "ROutput_LambdaZ", "ROutput_AUCLast", "ROutput_Error")
    
    # TEST 13 : using numPoints = 0 i.e. suppress terminal phase calculation
    
    test13 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 0)
    checkEquals(unname(unlist(test13[, checkCols])), 
        c(NA, NA, NA, 35.5, 0), msg = " || TEST 13 : suppress terminal phase calculation\n" )
    
    # TEST 14 : using numPoints = 1 i.e. not-allowed value for numPoints suppress terminal phase calculation
    
    test14 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 1)
    checkEquals(unname(unlist(test14[, checkCols])), 
        c(NA, NA, NA, 35.5, 0), msg = " || TEST 14 : not-allowed value for numPoints suppress terminal phase calculation\n" )
    
    # TEST 15 : using numPoints = 2 i.e. not-allowed value for numPoints suppress terminal phase calculation
    
    test15 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 2)
    checkEquals(unname(unlist(test15[, checkCols])), 
        c(NA, NA, NA, 35.5, 0), msg = " || TEST 15 : not-allowed value for numPoints suppress terminal phase calculation\n" )
    
    # TEST 16 : using numPoints = 3 i.e. allowed value for numPoints perform terminal phase calculation
    
    test16 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 3)
    checkEquals(unname(unlist(test16[, checkCols])), 
        c(0.5, 3, 0.34657359028, 35.5, 0), tol = 1e-8, msg = " || TEST 16 : allowed value for numPoints perform terminal phase calculation\n" )
    
    # TEST 17 : using numPoints = 7 i.e. allowed value for numPoints perform terminal phase calculation
    
    test17 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 7)
    checkEquals(test17, test1, msg = " || TEST 17 : allowed value for numPoints perform terminal phase calculation\n" )
    
    # TEST 18 : using numPoints = 8 i.e. numPoints == Cmax, perform terminal phase calculation
    
    test18 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 8)
    checkEquals(unname(unlist(test18[, checkCols])), 
        c(0.966253286928, 8, 0.349539270098, 35.5, 0), to = 1e-8, msg = " || TEST 18 : numPoints == Cmax, perform terminal phase calculation\n" )
    
    # TEST 19 : using numPoints = 10 i.e. numPoints > length(Time) suppress terminal phase calculation
    
    test19 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, numPoints = 10)
    checkEquals(unname(unlist(test19[, checkCols])), 
        c(NA, NA, NA, 35.5, 0), msg = " || TEST 19 : numPoints > length(Time) suppress terminal phase calculation\n" )
    
    # TEST 20: checking excPoints auto
    
    Conc <- c(1, 9, 8, 6, 4, 3)
    compar <- ncaAnalysis(Conc = Conc, Time = 0:5, Dose = 1, Dof = 1)
    
    test20 <- ncaAnalysis(Conc = Conc, Time = 0:5, Dose = 1, Dof = 1, 
        excPoints = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
    checkTrue(!identical(compar, test20), msg = " || TEST 20a: checking excPoints auto")
    checkEquals(unname(unlist(test20[, checkCols])), c(0.9961019, 3.0000000, 0.3297474, 29.0000000, 0.0000000), 
        tol = 1e-6, msg = " || TEST 20b: checking excPoints auto")
    
    # TEST 21: checking excPoints fixed
    
    t21 <- ncaAnalysis(Conc = Conc[-5], Time = (0:5)[-5], Dose = 1, Dof = 1, numPoints = 4)
    
    test21 <- ncaAnalysis(Conc = Conc, Time = 0:5, Dose = 1, Dof = 1, 
        excPoints = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), numPoints = 4)
    checkTrue(!identical(compar, test21), msg = " || TEST 21a: checking excPoints fixed")
    checkTrue(identical(t21[, checkCols[-4]], test21[, checkCols[-4]]), msg = " || TEST 21b: checking excPoints fixed")
    
    # TEST 22: checking exPoints none
    
    test22 <- ncaAnalysis(Conc = Conc, Time = 0:5, Dose = 1, Dof = 1, 
        excPoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
    checkEquals(unname(unlist(test22[, checkCols])), c(NA, NA, NA, 29, 0), msg = " || TEST 22: checking excPoints none")
    
    # TEST 23: checking usePoints
    
    Conc <- c(1, 9, 8, 6, 4, 3, 3, 2)
    
    test23 <- ncaAnalysis(Conc = Conc, Time = 0:7, Dose = 1, Dof = 1, 
        usePoints = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
    checkEquals(unname(unlist(test20[, checkCols])), c(0.9961019, 3.0000000, 0.3297474, 29.0000000, 0.0000000), 
        tol = 1e-6, msg = " || TEST 23: checking usePoints")
    
    # TEST 24: checking usePoints excPoints
    
    compar <- ncaAnalysis(Conc = c(10, 9, 8, 6, 4), Time = 0:4, Dose = 1, Dof = 1)
    
    test24 <- ncaAnalysis(Conc = Conc, Time = 0:7, Dose = 1, Dof = 1, 
        excPoints = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
        usePoints = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
    checkEquals(test24[, checkCols[-4]], compar[, checkCols[-4]], 
        msg = " || TEST 24: checking usePoints excPoints")
    
    # TEST 25: checking usePoints none
    
    test25 <- ncaAnalysis(Conc = Conc, Time = 0:7, Dose = 1, Dof = 1, 
        usePoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
    checkEquals(unname(unlist(test25[, checkCols])), c(NA, NA, NA, 34.5, 0.0), 
        msg = " || TEST 25: checking excPoints none")
    
    # TEST 26: missing Dose
    
    test26 <- ncaAnalysis(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dof = 1)
    checkEquals(test26[, "ROutput_Error"], 
        "Error in checkSingleNumeric(Dose, description = \"Dose\", \"ncaAnalysis\") : \n  Error in  ncaAnalysis :  Dose is not a numeric of length 1.  Value is: \n NULL\n", 
        msg = " || TEST 26: missing Dose")
    
    # TEST 27: usePoints and numPoints
    
    test27 <- ncaAnalysis(Conc = Conc, Time = 0:7, Dose = 1, Dof = 1, 
        usePoints = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE), 
        numPoints = 4)
    checkEquals(test27[, length(test27)], "usePoints was provided to ncaAnalysis in addition to numPoints", 
        msg = " || TEST 27: checking excPoints none")
    
    # TEST 28: inter "Lin Up Log Down", numPoints 3
    
    #c28 <- c(0, 18.01, 16.96, 14.16, 10.49, 5.803, 1.881, 0.715, 0.191, 0.096, 0.052, 0.028, 0.015, 0.008)
    #t28 <- c(0, 0.0167, 0.033, 0.083, 0.167, 0.33, 0.67, 1, 2, 4, 6, 8, 10, 12)
    #testFile <- system.file("data", "wnl_analysis_linlog.csv", package = "MangoNca")
    #testData <- read.csv(testFile)
    #test28 <- ncaAnalysis(Conc = c28, Time = t28, Dose = 10 * 1000, Dof = 0, numPoints = 3, inter = "Linear Log")
    #checkEquals(test28[1, ], testData[1, ], tol = 1e-08, msg = " || TEST 28 : Compare MangoNca and WNL output\n" )
    
    # TEST 29: exp vals cf integration
    
    test29 <- ncaAnalysis(Conc = 25 * exp(-(1/pi) * (0:5)^2), Time = (0:5)^2, Dose = 1, Dof = 1, inter = "Lin up Log down")
    auc0Inf <- 25 / (1/pi)
    auc0Last <- auc0Inf - 25 / (1/pi) * exp(-(1/pi) * 5^2)
    checkEquals(unlist(test29[, c("ROutput_AUCInfPred", "ROutput_AUCLast")]), 
        c(ROutput_AUCInfPred = auc0Inf, ROutput_AUCLast = auc0Last), 
        msg = " || TEST 29 : exp vals cf integration\n")
    
}

