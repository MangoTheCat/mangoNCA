# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 02/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


# tests lambdaZStatistics in lambdaz.R

test.lambdaZStatistics <- function()
{
    # TEST 1 : contrived data - concentration is exponential of linear data
    
    conc <- exp(seq(from = 0, by = -0.25, length.out = 4))
    time <- seq(from = 2, by = 0.25, length.out = 4)
    
    # lambda z should be slope of the logged concentration - i.e. 
    test1 <- lambdaZStatistics(Conc = conc, Time = time, numPoints = 4)
    checkEquals(test1, 
        list(
                Lambdaz = 1,
                intercept = exp(2),
                r2 = 1,
                adjr2 = 1,
                rhoXY = -1,
                tPhaseHalfLife = log(2),
                LambdazLower = 2,
                LambdazUpper = 2.75,
                numPoints = 4
            ), 
             msg = " || TEST 1 : all results as expected"
                    
                    
             )    
    
    # TEST 2 : more sophisticated data set (subject 3 of Theoph)
   
    Theoph3 <- subset(Theoph, Subject == 3)
    test2 <- lambdaZStatistics(Conc = Theoph3$conc, Time = Theoph3$Time, numPoints = 5)
    
    # NOTE: results calculated manually with the lm function
    
    checkEquals(test2, 
            list(
                    Lambdaz = 0.0945763, 
                    intercept = exp(2.37478 ),
                    r2 =  0.9907265,
                    adjr2 = 0.9876353,
                    rhoXY = -0.9953525,
                    tPhaseHalfLife = log(2) / 0.09458,
                    LambdazLower = 5.08,
                    LambdazUpper = 24.17,
                    numPoints = 5
                    ), tol = 1e-4,
                  msg = " || TEST 2 : theoph subject 3 as expected ") 
   
    # TEST 3 : data set with missing data
    
    Theoph3NA <- Theoph3
    Theoph3NA$Time[ c(1, 3, 5) ] <- NA
    Theoph3NA <- na.omit(Theoph3NA)
    
    # results should be the same as before, because the first 6 rows will be eliminated due to missing values
    test3 <- lambdaZStatistics(Conc = Theoph3NA$conc, Time = Theoph3NA$Time, numPoints = 5)
    checkEquals(test2, test3)
    
    # emptyList : list with the components returned by lambdaZStatistics, with all values set to NA
    
    emptyList <- list( Lambdaz = as.numeric(NA), intercept = as.numeric(NA),
            r2 =  as.numeric(NA), adjr2 = as.numeric(NA), rhoXY = as.numeric(NA), 
            tPhaseHalfLife = as.numeric(NA), LambdazLower = as.numeric(NA),
            LambdazUpper = as.numeric(NA), numPoints = as.numeric(NA) )
        
    # TEST 4 : concentrations add up to 0
    test4 <- lambdaZStatistics(Conc = c(1, 0, -1), Time = 1:3, numPoints = 3 )
    checkEquals(test4, emptyList)
    
    # TEST 5 : 0 values at the end - these should be disregarded as lambda z should be calculated counting from
    # last measurable concentration
    
    # lambda z should be slope of the logged concentration - i.e. 
    
    test5 <- lambdaZStatistics(Conc = c(conc, 0, 0), Time = c(time, 3, 3.25), numPoints = 4 )
    checkEquals(test5, test1, msg = " || TEST 5 : lambda z ignores anything beyond clast, tlast")
}

# tests lambdaZStatistics error handling in lambdaz.R

test.lambdaZStatistics.ErrorHandling <- function()
{
    isErrorWithMessage <- MangoNca:::isErrorWithMessage
    
    # TEST 1 : standard check for numeric vectors and equality of length
    # Since it is obvious by inspection of the code that the check is performed, we only have one check
    
    test1 <- try( lambdaZStatistics(5:1, 1:4), silent = TRUE )
   
    checkTrue( isErrorWithMessage( test1 , "Error in checkNumericSameLength(Time, Conc, \"Time\", \"Concentration\", \"lambdaZStatistics\") : \n  Error in lambdaZStatistics:  lengths of Time and Concentration do not match\n"))
    
    # TEST 2 : check that numPoints is numeric
    
    test2 <- try( lambdaZStatistics( 1:3, 1:3, numPoints = "a"), silent = TRUE  )
    checkTrue(isErrorWithMessage( test2 , "Error in checkSingleNumeric(numPoints, description = \"numPoints\", \"lambdaZStatistics\") : \n  Error in  lambdaZStatistics :  numPoints is not a numeric of length 1.  Value is: a\n"))

    # TEST 3 : check that numPoints has only 1 value
    
    test3 <- try( lambdaZStatistics( 1:3, 1:3, numPoints = 1:2 ), silent = TRUE )
    checkTrue(isErrorWithMessage( test3,  "Error in checkSingleNumeric(numPoints, description = \"numPoints\", \"lambdaZStatistics\") : \n  Error in  lambdaZStatistics :  numPoints is not a numeric of length 1.  Value is: 1 2\n"))
    
    # TEST 4 : check that numPoints is greater than 0
    
    test4 <- lambdaZStatistics(Conc = c(0, 10, 9, 7, 4, 3, 2), Time = 0:6, numPoints = 0)
    checkTrue( is.na(test4$Lambdaz), msg = "check that numPoints is greater than 0")
    
    # TEST 5 : check that numPoints is less than or equal to the number of time/concentration points
    
    test5 <- lambdaZStatistics(Conc = c(0, 10, 9, 7, 4, 3, 2), Time = 0:6, numPoints = 8)
    checkTrue( is.na(test5$adjr2), msg = "check that numPoints is less than or equal to the number of time/concentration points")
    
    # TEST 6 : check that if data are not allowed, an empty list is returned
    # emptyList : list with the components returned by lambdaZStatistics, with all values set to NA
    
    emptyList <- list( Lambdaz = as.numeric(NA), intercept = as.numeric(NA),
            r2 =  as.numeric(NA), adjr2 = as.numeric(NA), rhoXY = as.numeric(NA),
            tPhaseHalfLife = as.numeric(NA), LambdazLower = as.numeric(NA),
            LambdazUpper = as.numeric(NA), numPoints = as.numeric(NA) )
    
    test6 <- lambdaZStatistics(0:2, Time =  2:4, numPoints = 3)
    checkEquals(test6, emptyList, msg = " || TEST 6 : NA values returned because data are not allowed")
    
    # TEST 7 : missing values in more sophisticated data set (subject 3 of Theoph)
   
    Theoph3NA <- subset(Theoph, Subject == 3)
    Theoph3NA[c(1, 3, 5), "Time"] <- NA
    test7 <- try(lambdaZStatistics(Conc = Theoph3NA$conc, Time = Theoph3NA$Time, numPoints = 5), silent = TRUE)
    checkTrue(isErrorWithMessage( test7,  "Error in stripTrailingZeros(Conc = Conc, Time = Time, checkT0 = FALSE,  : \n  call to cleanConcTime failed, message is:  Error in naFail(cleanData) : \n  Error in cleanConcTime: missing values in object\n\n"))
    
}
