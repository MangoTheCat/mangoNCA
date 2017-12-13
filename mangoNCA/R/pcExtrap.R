# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate:  $
# Last changed by: $LastChangedBy:  $
# 
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################


#' Percentage of Whole Extrapolated.
#' @param Whole Value of whole
#' @param Measured Value of Whole which was measured
#' @title Percentage Extrapolated
#' @return Single numeric value (percentage) of Whole which was extrapolated
#' @note The formula used to calculate this 
#' quantity is \deqn{ 100 * (Whole - Measured) / Whole }  
#' @author Mango Solutions
#' @keywords math
#' @examples
#' pcExtrap(Whole = 100, Measured = 33)

pcExtrap <- function(Whole, Measured)
{
    checkSingleNumeric(Whole, description = "Whole")
    checkSingleNumeric(Measured, description = "Measured")
    
    if(is.na(Whole[1]) | Whole[1] <= 0 | Whole[1] == Inf | is.na(Measured[1]) | Measured[1] < 0 ) {
        return(as.numeric(NA))
    }
    
    extrap <- 100 * (Whole - Measured) / Whole

    return(extrap)
}
