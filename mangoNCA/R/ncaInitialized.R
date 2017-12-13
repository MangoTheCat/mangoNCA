# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Get R Version
#' 
#' This is a trivial test used to confirm that R is running correctly.
#' @title Function to return R version to R.NET
#' @return data frame with 1 column, Version and 1 character row
#' @export
#' @author Mango Solutions
#' @keywords math
#' @keywords nca
#' @examples
#' getWhichVersion()


getWhichVersion <-  function()
{
  
   thisVersion <- R.Version()$version.string
 
   return(data.frame(Version = thisVersion))
}


#' Check MangoNca is Initialized
#'
#' This is a second test used to confirm that R is running correctly.
#' Returns a data frame with 3 columns and 1 row:
#'  \enumerate{
#'      \item \code{rVersionOk} logical
#'      \item \code{rArchOk} logical
#'      \item \code{statsOk} logical
#'  }
#'
#' @title Function to return R version to R.NET
#' @param rVersionMinor single numeric or character string describing R version to test against
#' @return data frame 
#' @export
#' @author Mango Solutions
#' @keywords math
#' @keywords nca
#' @examples
#' ncaInitialized()

ncaInitialized <- function(rVersionMinor = 14.1)
{
    # versions to validate against
    
    rVersionMajor <- "2"
    rArch <- "i386"
    
    rVersionOk <- FALSE
    
    # Check version
    
    RVersion <- R.Version()
    
    rVersionMajor <- c(rVersionMajor) %in%  RVersion$major
    rVersionMinor <- c(rVersionMinor) %in%  RVersion$minor
    
    if (isTRUE(rVersionMajor) && isTRUE(rVersionMinor))  {
        rVersionOk <- TRUE
    } 

    # Check Architecture
    
    rArchOk <- c(rArch) %in% RVersion$arch
    
    
    # Check stats loaded
    
    packagesInSession <- search()
    
    statsInSession <- "package:stats" %in% packagesInSession
    
    TestResultsSummary <- data.frame(rVersionOk = rVersionOk, 
        rArchOk = rArchOk, 
        statsOk = statsInSession)

    return(TestResultsSummary)

}
