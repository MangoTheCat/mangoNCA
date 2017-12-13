# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 18/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

test.ncaComplete <- function()
{
    load(system.file(package = "MangoNca", "data", "shapeROutput.RData"))
    
    # TEST 1 : simple data working case
    
    test1 <- ncaComplete(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 0:8, Dose = 100, Dof = 2, ROutput = shapeROutput)
    checkTrue( class(test1) == "data.frame" , msg = " || TEST 1(a) simple data working case data frame\n")
    checkEquals( test1[["ROutput_Error"]], 0, msg = " || TEST 1(b) : simple data working case\n" )
    checkEquals( test1[["ROutput_adjr2"]], 0.9714937901, msg = " || TEST 1(c) : Test Adjusted R squared from contrived data\n" )
    
    # TEST 2 : Theoph data set
    
    Theoph2 <- subset(Theoph, Subject == 2)    
    test2 <- ncaComplete(Conc = Theoph2$conc, Time = Theoph2$Time, Dose = Theoph2$Dose[1], Dof = 2, ROutput = shapeROutput)
    checkEquals( test2[["ROutput_AUCInfObs"]], 100.1734591, tol = 1e-06, msg = " || TEST 2(a) : Test AUCInfObs from Theoph data set\n" )
    checkEquals( test2[["ROutput_AUCInfPred"]], 100.0643176, tol = 1e-06, msg = " || TEST 2(b) : Test AUCInfPred from Theoph data set\n" )
    
    # TEST 3 : Patient 1007
    
    testFile <- system.file("data", "wnl_analysis.csv", package = "MangoNca")
    
    testOutput <- read.csv(testFile)
    
    dataFile <- system.file("data", "wnl_data.csv", package = "MangoNca")
    
    dataInput <- read.csv(dataFile)
    
    # Test Output from Patient 1007
    
    testP1007 <- testOutput[1, ]
    
    testP1007 <- unlist(testP1007)
    testP1007 <- unname(testP1007)
    testP1007 <- round(testP1007, 4)
    
    # Provide raw data for Patient 1007
    
    dataP1007 <- dataInput[dataInput$SUBJECT == 1007, ]
    
    # Run Test (Call function)
    
    test3 <- ncaComplete(Conc = dataP1007$CONC, Time = dataP1007$TIME, 
        Dose = dataP1007$DOSE[1] * 1000, Dof = 1, ROutput = shapeROutput)
        
    checkTrue( test3[, "ROutput_Error"] == 0 , msg = " || TEST 3(a) No Error when calling dataset\n") 
        
    test3 <- unlist(test3)
    test3 <- unname(test3)
    test3 <- round(test3, 4)
    
    comparisonMatrix <- rbind(testP1007, test3)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]
    
    checkEquals( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08, msg = " || TEST 3(b) : Compare MangoNca and WNL output\n" )
    
    # TEST 4 : Patient 1033
    
    # Test Output from Patient 1033
    
    testP1033 <- testOutput[2, ]
    
    testP1033 <- unlist(testP1033)
    testP1033 <- unname(testP1033)
    testP1033 <- round(testP1033, 4)
    
    # Provide raw data for Patient 1033
    
    dataP1033 <- dataInput[dataInput$SUBJECT == 1033, ]
    
    # Run Test (Call function)
    
    test4 <- ncaComplete(Conc = dataP1033$CONC, Time = dataP1033$TIME, 
        Dose = dataP1033$DOSE[1] * 1000, Dof = 1, addT0 = TRUE, ROutput = shapeROutput)
        
    test4 <- unlist(test4)
    test4 <- unname(test4)
    test4 <- round(test4, 4)
    
    comparisonMatrix <- rbind(testP1033, test4)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]
    
    checkEquals( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08, msg = " || TEST 4 : Compare MangoNca and WNL output\n" )
    
    # TEST 5 : Patient 1037
    
    # Test Output from Patient 1037
    
    testP1037 <- testOutput[3, ]
    
    testP1037 <- unlist(testP1037)
    testP1037 <- unname(testP1037)
    testP1037 <- round(testP1037, 4)
    
    # Provide raw data for Patient 1037
    
    dataP1037 <- dataInput[dataInput$SUBJECT == 1037, ]
    
    # Run Test (Call function)
    
    test5 <- ncaComplete(Conc = dataP1037$CONC, Time = dataP1037$TIME, 
        Dose = dataP1037$DOSE[1] * 1000, Dof = 1, addT0 = TRUE, ROutput = shapeROutput)
        
    test5 <- unlist(test5)
    test5 <- unname(test5)
    test5 <- round(test5, 4)
    
    comparisonMatrix <- rbind(testP1037, test5) 
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]
    
    checkEquals( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08, msg = " || TEST 5 : Compare MangoNca and WNL output\n" )
    
    # TEST 6 : Clean Data (Patient 1007) in unsafe mode
    
    test6 <- ncaComplete(Conc = dataP1007$CONC, Time = dataP1007$TIME, 
        Dose = dataP1007$DOSE[1] * 1000, Dof = 1, excPoints = rep(FALSE, times = nrow(dataP1007)), Safe = FALSE, ROutput = shapeROutput)
        
    checkTrue( test6[, "ROutput_Error"] == 0 , msg = " || TEST 6(a) No Error when calling dataset\n") 
        
    test6 <- unlist(test6)
    test6 <- unname(test6)
    test6 <- round(test6, 4)
    
    comparisonMatrix <- rbind(testP1007, test6)
    comparisonMatrix <- comparisonMatrix[, apply(comparisonMatrix, 2, function(x) !any( is.na( x ) ))]
    
    checkEquals( comparisonMatrix[1, ], comparisonMatrix[2, ], tol = 1e-08, msg = " || TEST 6(b) : Compare MangoNca and WNL output\n" )
    
    # TEST 7: Check excPoints
    
    compRows <- c("ROutput_adjr2", "ROutput_intercept", "ROutput_numPoints", "ROutput_r2", "ROutput_rhoXY", "ROutput_Error")
    test7a <- ncaComplete(Conc = dataP1037$CONC[-4], Time = dataP1037$TIME[-4], 
        Dose = dataP1037$DOSE[1] * 1000, Dof = 0, addT0 = TRUE, ROutput = shapeROutput)
    test7b <- ncaComplete(Conc = dataP1037$CONC, Time = dataP1037$TIME, 
        Dose = dataP1037$DOSE[1] * 1000, Dof = 0, excPoints = c(FALSE, FALSE, FALSE, TRUE, FALSE), 
        addT0 = TRUE, ROutput = shapeROutput)
    checkEquals(test7a[compRows], test7b[compRows], msg = " || TEST 7: Check excPoints")
    
    # TEST 8: all zeros
    
    emptyDf <- c(rep(as.numeric(NA), 5), as.numeric(0))
    names(emptyDf) <- compRows
    emptyDf <- as.data.frame(as.list(emptyDf))
    test8 <- ncaComplete(Conc = rep(0, 6), Time = 0:5, Dose = 10, Dof = 1, ROutput = shapeROutput)
    checkEquals(test8[compRows], emptyDf, msg = " || TEST 8: all zeros")
    
    # TEST 9: mostly zeros
    
    test9 <- ncaComplete(Conc = c(1e-5, rep(0, 6), 1e-6), Time = 0:7, Dose = 10, Dof = 1, ROutput = shapeROutput)
    checkEquals(test9[compRows], emptyDf, msg = " || TEST 9: mostly zeros")
    
    # TEST 10: reversed time
    
    test10 <- ncaComplete(Conc = c(4, 9, 8, 6, 4:1, 1), Time = 8:0, Dose = 100, Dof = 2, ROutput = shapeROutput)
    checkEquals(test10[, "ROutput_Error"], 
                "Error in checkOrderedVector(Time, description = \"Time\", functionName = \"ncaComplete\") : \n  Error in ncaComplete: Time is not ordered.  Actual value is 8 7 6 5 4 3 2 1 0\n", 
                msg = " || TEST 10: reversed time")
    
    # TEST 11: Boomer Example
    # http://www.boomer.org/c/p3/c02/c0210.html
    
    test11 <- ncaComplete(Conc = c(100, 71, 50, 35, 25, 12, 6.2, 3.1), 
        Time = c(0:4, 6, 8, 10), Dose = 1, Dof = 0, ROutput = shapeROutput)
    checkEquals(round(test11[, c("ROutput_AUCLast", "ROutput_AUCInfObs")], 2), data.frame("ROutput_AUCLast" = 283.0, "ROutput_AUCInfObs" = 291.9), msg = " || TEST 11: Boomer Example")
    
}


test.chooseNumPointsAction <- function()
{
    if (!exists("chooseNumPointsAction", mode = "function")) { chooseNumPointsAction <- MangoNca:::chooseNumPointsAction }
    
    # TEST 1: auto selection
    
    conc <- c(2, 10, 8, 5, 3, 2)
    
    test1 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = as.numeric(NA))
    checkEquals(test1, list(ACTION = "auto", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 1: auto selection")
    
    # TEST 2: number selection
    
    test2 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = 4)
    checkEquals(test2, list(ACTION = "fixed", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 2: number selection")
    
    # TEST 3: invalid number selection
    
    test3 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = 2)
    checkEquals(test3, list(ACTION = "none", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 3: invalid number selection")
    
    # TEST 4: section selection
    
    test4 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = as.numeric(NA), 
        usePoints = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    checkEquals(test4, list(ACTION = "used", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 4: section selection")
    
    # TEST 5: section selection
    
    test5 <- try(chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = as.numeric(NA), 
        usePoints = NA), silent = TRUE)
    checkEquals(paste(test5), "Error in checkLogicalSameLength(usePoints, Conc, \"Used Points\", \"Concentration\",  : \n  Error in chooseNumPointsAction:  lengths of Used Points and Concentration do not match\n", 
        msg = "TEST 5: section selection")
    
    # TEST 6: invalid auto selection
    
    conc <- c(2, 10, 8, 5, 11, 2)
    
    test6 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = as.numeric(NA))
    checkEquals(test6, list(ACTION = "none", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 6: auto selection")
    
    # TEST 7: used selection
    
    test7 <- chooseNumPointsAction(Conc = conc, Time = 0:5, numPoints = as.numeric(NA), 
        usePoints = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    checkEquals(test7, list(ACTION = "used", MINROWSFORLAMBDAZ = 3), 
        msg = "TEST 7: used selection")
    
}
