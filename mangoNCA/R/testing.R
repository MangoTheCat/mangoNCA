# SVN revision: $Rev:  $
# Date of last change: 20/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Execute the RUnit unit tests suites for the MangoNca package.  
#' Prints an html report with name "Unit_Test_Report_MangoNca_<maj>_<min>_<svn>.html"
#' The package version is extracted from the DESCRIPTION file
#'
#' @param testPath A single string holding the path to the internal test suite scripts
#' @title Run MangoNca Test Suite
#' @return suiteResults 
#' @author Mango Solutions
#' @keywords debugging programming 
#' @examples 
#' \dontrun{ 
#'      runMangoNcaTests()
#' }
#' @export

runMangoNcaTests <- function(testPath = system.file(package = "MangoNca", "testing"))
{
    require(RUnit)
    
    # testSuite : object of class RUnitTestSTuite.  This will describe the internal unit test suite to be executed
    testSuite <- defineTestSuite(name = "MangoNca Unit Test Suite", dirs = testPath, 
            testFileRegexp = "^runit.+\\.[rR]$",  testFuncRegexp = "^test.+" )
    
    if (length(grep("^runit.+\\.[rR]$", testPath) == 0)) { stop("no files found called runit.name.R in directory", testPath) }
    
    # suiteResults : object of class RUnitTestData
    suiteResults <-  runTestSuite(testSuite)
    
    pack <- installed.packages()
    
    vers <- pack["MangoNca", "Version"]
    
    vers <- gsub("\\.", "_", vers)
    
    printHTMLProtocol(suiteResults, fileName = paste("Unit_Test_Report_MangoNca_", vers, ".html", sep = ""))
    
    return(suiteResults)
}

# miscellaneous testing routines

#' Check if an object is of class "try-error", and that it contains a specific error message.
#' @param x Object to check
#' @param expectedMessage String with expected message
#' @title Check if object is error with given error message
#' @return TRUE or FALSE depending on whether or not x is as expected
#' @author Mango Solutions


isErrorWithMessage <- function(x, expectedMessage)
{
    return(is(x, "try-error") & as.character(x) == expectedMessage)
}

