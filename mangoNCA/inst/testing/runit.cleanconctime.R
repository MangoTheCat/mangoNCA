# SVN revision: $Rev: $
# Date of last change: $LastChangedDate: 02/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################



# tests the function cleanConcTime from checkdata.R

test.cleanConcTime <- function()
{
    if (!exists("cleanConcTime", mode = "function")) { cleanConcTime <- MangoNca:::cleanConcTime }
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : check clean values are converted to data frame
    
    test1 <- cleanConcTime(Conc = 5:1, Time = 0:4, addT0 = TRUE)
    checkEquals( test1[, c("Conc", "Time")], data.frame(Conc = 5:1, Time = 0:4), msg = " || TEST1: check clean values are converted to data frame" )
    
    # TEST 2 : check NAs are removed

    test2 <- cleanConcTime(Conc = c(0, NA, 3:1), Time = c(0, 1:3, NA), addT0 = TRUE)
    checkEquals( test2[["Conc"]], c(0, 3, 2), msg = " || TEST2: check NAs are removed" ) 
    
    # TEST 3 : All NA vector used
    
    NAVector <- rep(as.numeric(NA), 5)
    zeroDf <- data.frame(Conc = numeric(0), Time = numeric(0), excPoints = logical(0))
    test3 <- try(cleanConcTime(Conc = NAVector, Time = 0:4, addT0 = TRUE), silent = TRUE)
    check3 <- identical(storage.mode(test3), storage.mode(zeroDf)) && identical(names(test3), names(zeroDf)) && identical(nrow(test3), nrow(zeroDf))
    checkTrue(check3, msg = " || TEST 3 : All NA vector used")
    
    # TEST 4 : check clean values adding T = 0
    
    test4 <- cleanConcTime(Conc = 5:1, Time = 1:5, addT0 = TRUE)
    checkEquals( test4[["Time"]], 0:5, msg = " || TEST4(a): check clean values in Time adding T = 0" )
    checkEquals( test4[["Conc"]], c(0, 5:1), msg = " || TEST4(b): check clean values in Conc adding T = 0" )
    
    # TEST 5 : minPoints length > 1, exception generated
    
    test5 <- try( cleanConcTime(Conc = 5:1, Time = 1:5, addT0 = c(FALSE, TRUE)), silent = TRUE )
    checkTrue(isErrorWithMessage(test5, "Error in checkSingleLogical(addT0, description = \"Add Row at T = 0?\",  : \n  Error in  cleanConcTime :  Add Row at T = 0? is not a logical of length 1.  Value is: FALSE TRUE\n"))
    
    # TEST 6 : Non-numeric vector used, exception generated
    
    test6 <- try( cleanConcTime(Conc = c("BLQ", 8, 5, 4, 4), Time = 1:5, addT0 = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage(test6, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"cleanConcTime\") : \n  Error in cleanConcTime: Conc is not a numeric vector\n"))
    
    # TEST 7 : Conc and Time are not numeric vectors
    
    Theoph10 <- Theoph[Theoph$Subject == 10, ]
    test7 <- try( cleanConcTime(Conc = Theoph10["conc"], Time = Theoph10["Time"], addT0 = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage(test7, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"cleanConcTime\") : \n  Error in cleanConcTime: Time is not a numeric vector\n"))
    

    # TEST 8 : Differing length numeric vectors
    
    test8 <- try( cleanConcTime(Conc = 1:5, Time = 0:3, addT0 = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage(test8, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"cleanConcTime\") : \n  Error in cleanConcTime:  lengths of Time and Conc do not match\n"))
    
    # TEST 9 : NULL should not work!
    
    test9 <- try( cleanConcTime(Conc = 1:5, Time = NULL, addT0 = TRUE), silent = TRUE)
    checkTrue(isErrorWithMessage(test9, "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Conc\", functionName = \"cleanConcTime\") : \n  Error in cleanConcTime: Time is not a numeric vector\n"))
    
    # TEST 10 : check clean values are converted to data frame when addT0 FALSE
    
    test10 <- cleanConcTime(Conc = 5:1, Time = 0:4, addT0 = FALSE)
    checkEquals( test10[, c("Conc", "Time")], data.frame(Conc = 5:1, Time = 0:4), msg = " || TEST10 : check clean values are converted to data frame" )

    # TEST 11 : exception with NA when addT0 = FALSE
    
    test11 <- try(cleanConcTime(Conc = c(5:3, NA, 1), Time = 1:5, addT0 = FALSE), silent = TRUE)
    checkTrue(isErrorWithMessage(test11, "Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n"))
    
    # TEST 12 : exception with missing T = 0 when addT0 = FALSE
    
    test12 <- try(cleanConcTime(Conc = c(5:3, 1.5, 1), Time = 1:5, addT0 = FALSE), silent = TRUE)
    checkTrue(isErrorWithMessage(test12, "Error in cleanConcTime(Conc = c(5:3, 1.5, 1), Time = 1:5, addT0 = FALSE) : \n  Missing row where T = 0\n"))
    
    # TEST 13 : exception with missing T = 0 when addT0 = FALSE
    
    test13 <- try(cleanConcTime(Conc = c(5:3, -1, 1), Time = 0:4, addT0 = FALSE), silent = TRUE)
    checkTrue(isErrorWithMessage(test13, "Error in cleanConcTime(Conc = c(5:3, -1, 1), Time = 0:4, addT0 = FALSE) : \n  values of Conc < 0 in cleanData\n"))
    
    # TEST 14: check clean values are converted to a data frame with usePoints
    
    test14 <- cleanConcTime(Conc = 10:5, Time = 0:5, usePoints = rep(TRUE, 6), addT0 = TRUE)
    checkEquals( test14, data.frame(Conc = 10:5, Time = 0:5, excPoints = FALSE, usePoints = TRUE), 
        msg = " || TEST 14: check clean values are converted to data frame with usePoints" )
    
    # TEST 15: check clean values are converted to a data frame with excPoints
    
    test15 <- cleanConcTime(Conc = 5:1, Time = 0:4, excPoints = rep(TRUE, 5), addT0 = TRUE)
    checkEquals( test15, data.frame(Conc = 5:1, Time = 0:4, excPoints = TRUE), msg = " || TEST 15: check clean values are converted to data frame with excPoints" )
    
    # TEST 16: check clean values are converted to a data frame with usePoints and excPoints
    
    test16 <- cleanConcTime(Conc = Theoph10$conc, Time = Theoph10$Time, 
        usePoints = Theoph10$Time > 3 & Theoph10$Time < 20, excPoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 4)), addT0 = TRUE)
    checkEquals( test16, data.frame(Conc = Theoph10$conc, Time = Theoph10$Time, 
        excPoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 4)), usePoints = Theoph10$Time > 3 & Theoph10$Time < 20), 
        msg = " || TEST 16: check clean values are converted to data frame with usePoints and excPoints" )
    
    # TEST 17: check missing T0 values are converted to a data frame with usePoints and excPoints
    
    Theoph9 <- Theoph[Theoph$Subject == 9 & Theoph$Time != 0, ]
    
    test17 <- cleanConcTime(Conc = Theoph9$conc, Time = Theoph9$Time, 
        usePoints = Theoph9$Time > 3 & Theoph9$Time < 20, excPoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 3)), addT0 = TRUE)
    checkEquals( test17, data.frame(Conc = c(0, Theoph9$conc), Time = c(0, Theoph9$Time), 
        usePoints = c(FALSE, Theoph9$Time > 3 & Theoph9$Time < 20), excPoints = c(TRUE, rep(FALSE, 6), TRUE, rep(FALSE, 3))), 
        msg = " || TEST 17: check missing T0 values are converted to a data frame with usePoints and excPoints" )
    
    # TEST 18: check error when missing T0 values with usePoints and excPoints
    
    test18 <- try(cleanConcTime(Conc = Theoph9$conc, Time = Theoph9$Time, 
        usePoints = Theoph9$Time > 3 & Theoph9$Time < 20, excPoints = c(rep(FALSE, 6), TRUE, rep(FALSE, 3)), addT0 = FALSE), silent = TRUE)
    checkTrue(isErrorWithMessage( test18, "Error in cleanConcTime(Conc = Theoph9$conc, Time = Theoph9$Time, usePoints = Theoph9$Time >  : \n  Missing row where T = 0\n"))
    
}

