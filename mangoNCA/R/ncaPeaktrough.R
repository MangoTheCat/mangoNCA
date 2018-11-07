# Date of last change: 13/06/2016
# Last changed by: ccampbell
# 
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Return concentration values coded as PEAKCODE or TROUGHCODE for sparse data
#'
#' Calculates Peak through (pre / post dose concentrations) for individuals on time concentration curves.
#' PEAKCODE is 2, TROUGHCODE is 1, if there is more than one of each of these, this is a data error.
#'
#' @title PeakTrough Vector of pre- and post-dose identifiers (TROUGHCODE and PEAKCODE respectively). There must not be more than one of each. NULL is replaced with 0.
#' @inheritParams getNCAnalysis
#' @param output Named vector consisting of 38 NA and 1 zero with names which should match expected output names.
#' @return Data frame 
#' @author Mango Solutions
#' @export
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' ncaPeakTrough(conc = Theoph1$conc, time = Theoph1$time, 
#'     dose = Theoph1$Dose[1], duration = 1)

ncaPeakTrough <- function(conc, time, dose, duration, output) {
    
    error <- ""
    
    # Check data for gross errors
    check01 <- try(checkOrderedVector(time, description = "time", functionName = "ncaPeakTrough"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(time, conc, "time", "concentration", "ncaPeakTrough"), silent = TRUE)
    # removed PeakTrough
    check05 <- try(checkSingleNumeric(dose, description = "dose", "ncaPeakTrough"), silent = TRUE)
    
    check06 <- try(checkSingleNumeric(duration, description = "Duration of Infusion", "ncaPeakTrough"), silent = TRUE)
    
    if(missing(ROutput) || 
        !identical(as.integer(39), length(ROutput)) || 
        is.null(names(ROutput))) { check07 <- "error in ncaPeakTrough: ROutput should be a named vector of length 39" } else { check07 <- NULL }
    # return if gross data errors present coerce output to data frame add errors and return
    
    error <- paste(check01, check02, check05, check06, check07, collapse = "\n")
    
    if (!identical(x = error, y = "")) {
        
        ROutput["ERROR"] <- error
        
        return(ROutput) 
    }
    
    ROutput["DOSE"] <- dose
    
    ROutput["INTDOSE"] <- duration
    
    ##################################################################################################
    
    ##  Locate Peak and Trough if present
    
    #PEAKCODE <- 2
    #TROUGHCODE <- 1
    #PeakIndex <- which( PeakTrough == PEAKCODE )[1]
    #    
    #ROutput["CPEAK"] <- conc[PeakIndex]
    #TroughIndex <- which( PeakTrough == TROUGHCODE )[1]
    #    
    #ROutput["CTROUGH"] <- conc[TroughIndex]
    ##################################################################################################
    
    ##  Coerce to dataframe and return
    ROutput <- as.data.frame(as.list(ROutput))
        
    return(ROutput)
}
