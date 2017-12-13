# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 16/03/2012 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Return Concentration values coded as PEAKCODE or TROUGHCODE for sparse data
#'
#' Calculates Peak through (pre / post dose concentrations) for individuals on Time concentration curves.
#' PEAKCODE is 2, TROUGHCODE is 1, if there is more than one of each of these, this is a data error.
#'
#' @title PeakTrough Vector of pre- and post-dose identifiers (TROUGHCODE and PEAKCODE respectively). There must not be more than one of each. NULL is replaced with 0.
#' @param Conc Vector of concentrations.
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates.
#' @param Dose Single numeric value of dose.
#' @param Dof Single numeric value of duration of infusion.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @param ROutput Named vector consisting of 38 NA and 1 zero with names which should match expected output names.
#' @return Data frame 
#' @author Mango Solutions
#' @export
#' @examples
#' load(system.file(package = "MangoNca", "data", "shapeROutput.RData"))
#' Theoph1 <- subset(Theoph, Subject == 1)
#' ncaPeakTrough(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1], Dof = 1, PeakTrough = c(1, 0, 0, 0, 2, rep(0, 6)), ROutput = shapeROutput)

ncaPeakTrough <- function(Conc, Time, Dose, Dof, PeakTrough = NULL, Safe = TRUE, ROutput) {

    checkSafe <- try(checkSingleLogical(Safe, description = "Safe", functionName = "ncaPeakTrough"), silent = TRUE)

    if( class(checkSafe) == "try-error" ) {
    
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- paste(checkSafe, collapse = "\n")
        
        return(ROutput) 
    }

    if( Safe ) {

        # Check data for gross errors

        check01 <- try(checkOrderedVector(Time, description = "Time", functionName = "ncaPeakTrough"), silent = TRUE)
        
        check02 <- try(checkNumericSameLength(Time, Conc, "Time", "Concentration", "ncaPeakTrough"), silent = TRUE)
            
        if( is.null( PeakTrough ) ) { 
            PeakTrough <- rep(0, times = length(Time))
        }        
            
        check03 <- try(checkNumericSameLength(Time, PeakTrough, "Time", "Peak/Trough", "ncaPeakTrough"), silent = TRUE)
        
        check04 <- try(checkPeakTrough(PeakTrough, functionName = "ncaPeakTrough"), silent = TRUE)
        
        check05 <- try(checkSingleNumeric(Dose, description = "Dose", "ncaPeakTrough"), silent = TRUE)
        
        check06 <- try(checkSingleNumeric(Dof, description = "Duration of Infusion", "ncaPeakTrough"), silent = TRUE)
        
        if(missing(ROutput) || 
            !identical(as.integer(39), length(ROutput)) || 
            is.null(names(ROutput))) { check07 <- "error in ncaPeakTrough: ROutput should be a named vector of length 39" } else { check07 <- NULL }

        # return if gross data errors present coerce output to data frame add errors and return
        
        ROutput_Error <- paste(check01, check02, check04, check05, check06, check07, collapse = "\n")
        
        if( ROutput_Error != "" ) {
        
            ROutput <- as.data.frame(as.list(ROutput))
            
            ROutput["ROutput_Error"] <- ROutput_Error
            
            return(ROutput) 
        }

    }

    ROutput["ROutput_Dose"] <- Dose
    
    ROutput["ROutput_Dof"] <- Dof
    
    ##################################################################################################
    
    ##  Locate Peak and Trough if present
    
    PEAKCODE <- 2
    TROUGHCODE <- 1

    PeakIndex <- which( PeakTrough == PEAKCODE )[1]
        
    ROutput["ROutput_Peak"] <- Conc[PeakIndex]

    TroughIndex <- which( PeakTrough == TROUGHCODE )[1]
        
    ROutput["ROutput_Trough"] <- Conc[TroughIndex]

    ##################################################################################################
    
    ##  Coerce to dataframe and return

    ROutput <- as.data.frame(as.list(ROutput))
        
    return(ROutput)

}
