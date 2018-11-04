# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 24/05/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Perform Non-Compartmental Analysis for a Concentration-Time repeated measures dataset.
#' 
#' This is a wrapper function designed to be the interface for performing a complete Non-Compartmental Analysis. 
#' The function has required arguments: \enumerate{
#'      \item Conc, a vector of concentration values of length equal to \code{length(Time)}
#'      \item Time, a corresponding vector of measurement time values
#'      \item Dose, a single numeric value declaring dose
#'      \item Dof (duration of infusion), a single numeric value declaring duration of infusion.
#' }
#' Optional arguments are: \enumerate{
#'      \item PeakTrough, numeric vector of length equal to \code{length(Time)} coding time values 
#'      for no more than one trough (1) and peak (2) concentration records, and 0 elsewhere.
#'      \item numPoints, a single integer value declaring number of points to use in \code{lambdazStatistics}
#'      when calculating terminal phase elimination. 
#'      If numPoints is 0 (zero), terminal phase calculations are supressed.
#'      By default, numPoints is NULL, declaring that numPoints should automatically be calculated by \code{selectPoints} using the default method.
#'      \item usePoints suppresses automatic selection of terminal phase; 
#'      all rows which are \code{TRUE} will be used to calculate lambdaz. This argument must not be used with numPoints.
#'      \item excPoints uses automatic selection, but all rows which are \code{TRUE} are excluded 
#'      from the terminal phase calculation. If usePoints and excPoints are both supplied, then automatic selection will be supressed, 
#'      but excPoints will be omitted in addition to rows excluded by usePoints. 
#'      If supplied with numPoints, excluded values will not be counted in points used for lambdaz.
#'      \item Safe, a single logical stating that redundant checking should be performed in each function.
#'      \item inter, a single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' }
#' No unit conversion is performed by these functions.
#' \code{ncaAnalysis} returns a data frame with 39 columns containing 1 row of numeric results.
#' The final column of the return is \code{ROutput_Error}. This should be zero. If it is not, an error  
#' has occured. The text of each error will appear in this column as a single character string with  
#' individual messages separated by a newline.  
#' If there are three or fewer rows, data is passed to \code{\link{ncaPeakTrough}} for Peak/Trough identification only.
#' If there are four or more rows, data is passed to \code{\link{ncaComplete}} for full non-compartmental analysis.
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{R2ADJ}
#'      \item \code{INTERCEPT}
#'      \item \code{LAMZNPT}
#'      \item \code{R2}
#'      \item \code{CORRXY}
#'      \item \code{AUCPEO}
#'      \item \code{AUCPEP}
#'      \item \code{AUCIFO}
#'      \item \code{AUCIFP}
#'      \item \code{AUCLST}
#'      \item \code{AUMCPEO}
#'      \item \code{AUMCPEP}
#'      \item \code{AUMCIFO}
#'      \item \code{AUMCIFP}
#'      \item \code{AUMCLST}
#'      \item \code{CLST}
#'      \item \code{CLO}
#'      \item \code{CLP}
#'      \item \code{CMAX}
#'      \item \code{CMIN}
#'      \item \code{INTDOSE}
#'      \item \code{DOSE}
#'      \item \code{LAMZHL}
#'      \item \code{LAMZ}
#'      \item \code{LAMZLL}
#'      \item \code{LAMZUL}
#'      \item \code{MRTIFO}
#'      \item \code{MRTIFP}
#'      \item \code{MRTLST}
#'      \item \code{CPEAK}
#'      \item \code{TLST}
#'      \item \code{TMAX}
#'      \item \code{TMIN}
#'      \item \code{CTROUGH}
#'      \item \code{VSSO}
#'      \item \code{VSSP}
#'      \item \code{VZO}
#'      \item \code{VZP}
#'      \item \code{ERROR}
#'  }
#'
#' @param Conc Vector of Conc
#' @param Time Vector of Time, must be ordered in ascending order and should not have duplicates
#' @param Dose Single numeric value of dose
#' @param Dof Single numeric value of duration of infusion
#' @param PeakTrough Optional vector of numeric coding of Peak and Trough values, length equal to length of Time, otherwise \code{NULL}.
#' @param numPoints If \code{NULL} (default) automatically select, else, single numeric number of points for calculation of terminal phase.
#' @param usePoints If \code{NULL} (default) automatically select, else, logical vector of points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param excPoints If \code{NULL} (default) automatically select, else, logical vector of points to exclude from automatic calculation of terminal phase. Excluded rows are flagged by excPoints as \code{TRUE}.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is \code{TRUE}).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @title API wrapper for NCA analysis pharmacodynamic data
#' @return Data frame 
#' @export
#' @author Mango Solutions
#' @keywords math
#' @keywords nca
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' ncaAnalysis(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = 10, Dof = 1)

ncaAnalysis <- function(Conc = NULL, Time = NULL, Dose = NULL, Dof = NULL, PeakTrough = NULL, 
    numPoints = NULL, usePoints = NULL, excPoints = NULL, Safe = TRUE, inter = "Linear")  {

    # Initialise data check return object
    
    ROutput <- c(rep(as.numeric(NA), times = 38), 0)
    
    names(ROutput) <- c(
        "R2ADJ",
        "INTERCEPT",
        "LAMZNPT",
        "R2",
        "CORRXY",
        "AUCPEO",
        "AUCPEP",
        "AUCIFO",
        "AUCIFP",
        "AUCLST",
        "AUMCPEO",
        "AUMCPEP",
        "AUMCIFO",
        "AUMCIFP",
        "AUMCLST",
        "CLST",
        "CLO",
        "CLP",
        "CMAX",
        "CMIN",
        "INTDOSE",
        "DOSE",
        "LAMZHL",
        "LAMZ",
        "LAMZLL",
        "LAMZUL",
        "MRTIFO",
        "MRTIFP",
        "MRTLST",
        "CPEAK",
        "TLST",
        "TMAX",
        "TMIN",
        "CTROUGH",
        "VSSO",
        "VSSP",
        "VZO",
        "VZP",
        "ERROR")

    # Check data for gross errors

    check01 <- try(checkOrderedVector(Time, description = "Time", functionName = "ncaAnalysis"), silent = TRUE)
    
    check02 <- try(checkNumericSameLength(Time, Conc, "Time", "Concentration", "ncaAnalysis"), silent = TRUE)
        
    if(is.null(PeakTrough)) { 
        
        PeakTrough <- rep(0, times = length(Time))
    }
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check01, check02, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to data frame and return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- ROutput_Error
     
        return(ROutput)
        
    }
    
    check03 <- try(checkNumericSameLength(Time, PeakTrough, "Time", "Peak/Trough", "ncaAnalysis"), silent = TRUE)
    
    check04 <- try(checkPeakTrough(PeakTrough, functionName = "ncaAnalysis"), silent = TRUE)
    
    check05 <- try(checkSingleNumeric(Dose, description = "Dose", "ncaAnalysis"), silent = TRUE)
    
    check06 <- try(checkSingleNumeric(Dof, description = "Duration of Infusion", "ncaAnalysis"), silent = TRUE)
    
    if (!is.null(numPoints) & !is.null(usePoints)) {
        
        check07 <- "usePoints was provided to ncaAnalysis in addition to numPoints"
        
    } else {
        
        if (is.null(numPoints)) {
            
            numPoints <- as.numeric(NA)
        }
        
        check07 <- try(checkSingleNumeric(numPoints, description = "Number of Points", "ncaAnalysis"), silent = TRUE)
    }
    
    if (!is.null(usePoints)) { 
        
        check08 <- try(checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "ncaAnalysis"), silent = TRUE)
        
    } else {
        
        check08 <- NULL
    }
    
    if (is.null(excPoints)) { 
        
        excPoints <- rep(FALSE, times = length(Time))
    }
 
    check09 <- try(checkLogicalSameLength(excPoints, Conc, "excPoints", "Concentration", "ncaAnalysis"), silent = TRUE)

    check10 <- try(checkSingleCharacter(inter, "inter", "ncaAnalysis"), silent = TRUE)
    
    # return if gross data errors present
    
    ROutput_Error <- paste(check03, check04, check05, check06, check07, check08, check09, check10, sep = "", collapse = "")
    
    if( ROutput_Error != "" ) {
        
        # coerce to data frame and return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- ROutput_Error
     
        return(ROutput)
        
    }
    
    # perform complete NCA provided there are more rows than THRESHOLDCOMPLETE, otherwise only check for Peak and Trough
    # return results from ncaComplete or ncaPeakTrough provided they match expected structure
    
    THRESHOLDCOMPLETE <- 3
    
    if ( length(Time) > THRESHOLDCOMPLETE )  
    {
        
        ROutput_ncaComplete <- ncaComplete(Conc = Conc, Time = Time, Dose = Dose, Dof = Dof, PeakTrough = PeakTrough, 
            numPoints = numPoints, usePoints = usePoints, excPoints = excPoints, Safe = Safe, ROutput = ROutput, inter = inter)

        if (identical(names(ROutput_ncaComplete), names(ROutput)) && identical(as.integer(1), nrow(ROutput_ncaComplete))) {
        
            ROutput <- ROutput_ncaComplete
            
        } else 
        {
        
            ROutput["ROutput_Error"] <- "error in ncaAnalysis: return object from ncaComplete did not have expected structure\n"
        
        }
        
        return(ROutput)
        
    }  else  {
    
        ROutput_ncaPeakTrough <- ncaPeakTrough(Conc = Conc, Time = Time, Dose = Dose, Dof = Dof, PeakTrough = PeakTrough, ROutput = ROutput)
        
        if(identical(names(ROutput_ncaPeakTrough), names(ROutput)) && identical(as.integer(1), nrow(ROutput_ncaPeakTrough))) {
        
            ROutput <- ROutput_ncaPeakTrough
            
        } else 
        {
        
            ROutput["ROutput_Error"] <- "error in ncaAnalysis: return object from ncaPeakTrough did not have expected structure\n"
        
        }
        
        return(ROutput)
        
    }
}
