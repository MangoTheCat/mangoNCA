# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 19/08/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Perform Non-Compartmental Analysis for Complete Concentration-Time Datasets
#'
#' This function is not expected to be used directly. The output structure is passed from \code{ncaAnalysis}.
#' \code{ncaComplete} calculates NCA parameters for individuals returning as many parameters as are calculable.
#' \code{ROutput_Peak} and \code{ROutput_Trough} will always be returned where possible.
#' The final column of the return is \code{ROutput_Error}. This should be zero. 
#' \code{ncaComplete} will \code{try} to call the functions to calculate the NCA parameters.
#' If it is not, an error has occured. 
#' The text of each error will appear in this column as a single character string with  
#' individual messages separated by a newline. 
#' Returns a data frame with 1 row containing columns:
#'  \enumerate{
#'      \item \code{ROutput_adjr2}  
#'      \item \code{ROutput_intercept} 
#'      \item \code{ROutput_numPoints}  
#'      \item \code{ROutput_r2}  
#'      \item \code{ROutput_rhoXY}  
#'      \item \code{ROutput_AUC_Percent_Extrapolated_obs}  
#'      \item \code{ROutput_AUC_Percent_Extrapolated_pred}  
#'      \item \code{ROutput_AUCInfObs}  
#'      \item \code{ROutput_AUCInfPred}  
#'      \item \code{ROutput_AUCLast}  
#'      \item \code{ROutput_AUMC_Percent_Extrapolated_obs}  
#'      \item \code{ROutput_AUMC_Percent_Extrapolated_pred} 
#'      \item \code{ROutput_AUMCInfObs}  
#'      \item \code{ROutput_AUMCInfPred}  
#'      \item \code{ROutput_AUMCLast}  
#'      \item \code{ROutput_CLast}  
#'      \item \code{ROutput_ClObs}  
#'      \item \code{ROutput_ClPred}  
#'      \item \code{ROutput_Cmax}  
#'      \item \code{ROutput_Cmin}  
#'      \item \code{ROutput_Dof}  
#'      \item \code{ROutput_Dose}  
#'      \item \code{ROutput_HalfLife}  
#'      \item \code{ROutput_LambdaZ}  
#'      \item \code{ROutput_LambdazLower}  
#'      \item \code{ROutput_LambdazUpper}  
#'      \item \code{ROutput_MRTInfObs}  
#'      \item \code{ROutput_MRTInfPred}  
#'      \item \code{ROutput_MRTLast}  
#'      \item \code{ROutput_Peak}  
#'      \item \code{ROutput_TLast}  
#'      \item \code{ROutput_Tmax}  
#'      \item \code{ROutput_Tmin}  
#'      \item \code{ROutput_Trough}  
#'      \item \code{ROutput_VssObs}  
#'      \item \code{ROutput_VssPred}  
#'      \item \code{ROutput_VzObs} 
#'      \item \code{ROutput_VzPred} 
#'      \item \code{ROutput_Error} 
#'  } 
#'
#' @title Non compartment analysis on Time Concentration data
#' @param Conc Vector of Concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param Dose Single numeric of amount of compound delivered to system
#' @param Dof Single numeric of duration of infusion.
#' @param PeakTrough Optional vector of numeric coding of length Time (default is NULL).
#' @param numPoints If NA (default) number of points for calculation of terminal phase is calculated by \code{selectPoints}, otherwise a single integer declaring number of points to use. If numPoints is zero or less, supress terminal phase calculations
#' @param usePoints If NULL (default) automatically select, else, logical vector of points to use (\code{TRUE}) for calculation of terminal phase.
#' @param excPoints Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @param Safe Single logical value declaring whether to perform redundant data checks (default is TRUE).
#' @param addT0 Single logical value declaring whether to add row at T = 0 if missing (default is FALSE).
#' @param inter Single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' @param ROutput Named vector consisting of 38 NA and 1 zero with names which should match expected output names.
#' @return Data frame 
#' @author Mango Solutions
#' @export
#' @examples
#' load(system.file(package = "MangoNca", "data", "shapeROutput.RData"))
#' Theoph1 <- subset(Theoph, Subject == 1)
#' ncaComplete(Conc = Theoph1$conc, Time = Theoph1$Time, Dose = Theoph1$Dose[1], Dof = 1, ROutput = shapeROutput)

ncaComplete <- function(Conc, Time, Dose, Dof, PeakTrough = NULL, numPoints = as.numeric(NA), 
    usePoints = NULL, excPoints = FALSE, Safe = TRUE, addT0 = FALSE, inter = "Linear", ROutput) {

    if(missing(ROutput)) {
        
        load(system.file(package = "MangoNca", "data", "shapeROutput.RData"))
        
        if (!exists("shapeROutput")) { stop("Catastrophic Error: R incorrectly installed in System: cannot find data in package:MangoNca") }
        
        ROutput <- shapeROutput
        
        ROutput["ROutput_Error"] <- "error in ncaComplete: ROutput is missing with no default"
        
        return(ROutput)
    }

    checkSafe <- try(checkSingleLogical(Safe, description = "Safe", functionName = "ncaComplete"), silent = TRUE)

    if( class(checkSafe) == "try-error" ) {
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput["ROutput_Error"] <- paste(checkSafe, collapse = "\n")
        
        return(ROutput) 
    }

    if(Safe) {
        
        # Check data for gross errors
        
        check01 <- try(checkOrderedVector(Time, description = "Time", functionName = "ncaComplete"), silent = TRUE)
        
        check02 <- try(checkNumericSameLength(Time, Conc, "Time", "Concentration", "ncaComplete"), silent = TRUE)
        
        ROutput_Error <- paste(check01, check02, sep = "", collapse = "")
        
        if( ROutput_Error != "" ) {
            
            # return with error
            
            ROutput <- as.data.frame(as.list(ROutput))
            
            ROutput["ROutput_Error"] <- ROutput_Error
            
            return(ROutput)
            
        }
        
        if(is.null(PeakTrough)) {
            
            PeakTrough <- rep(0, times = length(Time))
        }        
            
        check03 <- try(checkNumericSameLength(Time, PeakTrough, "Time", "Peak/Trough", "ncaComplete"), silent = TRUE)
        
        check04 <- try(checkPeakTrough(PeakTrough, functionName = "ncaComplete"), silent = TRUE)
        
        check05 <- try(checkSingleNumeric(Dose, description = "Dose", "ncaComplete"), silent = TRUE)
        
        check06 <- try(checkSingleNumeric(Dof, description = "Duration of Infusion", "ncaComplete"), silent = TRUE)
        
        check07 <- try(checkSingleNumeric(numPoints, description = "Number of Points", "ncaComplete"), silent = TRUE)
        
        if(!is.null(usePoints)) { 
            
            check08 <- try(checkLogicalSameLength(usePoints, Conc, "usePoints", "Concentration", "ncaComplete"), silent = TRUE)
            
        } else {
            
            check08 <- NULL
            
        }
        
        if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
        
        check09 <- try(checkLogicalSameLength(excPoints, Time, "excPoints", "Time", "ncaComplete"), silent = TRUE)
        
        check10 <- try(checkSingleLogical(addT0, description = "Add Row at T = 0?", "ncaComplete"), silent = TRUE)
        
        check11 <- try(checkSingleCharacter(inter, "inter", "ncaComplete"), silent = TRUE)
    
        if(!identical(as.integer(39), length(ROutput)) || is.null(names(ROutput))) { 
            
            check12 <- "error in ncaComplete: ROutput should be a named vector of length 39" 
            
        } else { 
            
            check12 <- NULL 
        }
        
        # return if gross data errors present coerce output to data frame add errors and return
        
        ROutput_Error <- paste(check03, check04, check05, check06, check07, check08, check09, check10, check11, check12, sep = "", collapse = "")
        
        if( ROutput_Error != "" ) {
            
            # return with error
            
            ROutput <- as.data.frame(as.list(ROutput))
            
            ROutput["ROutput_Error"] <- ROutput_Error
            
            return(ROutput)
            
        }
        
    } else { ROutput_Error <- "" }
    
    # if addT0 is TRUE fix data errors (add T = 0 to data if it is missing and remove missing values)
    # otherwise throw exception if data errors are present
    # if no rows are returned by stripTrailingZeros, return empty data frame without error
    
    cleanData <- try(stripTrailingZeros(Conc = Conc, Time = Time, usePoints = usePoints, 
        excPoints = excPoints, addT0 = addT0, checkT0 = TRUE), silent = TRUE)
    
    if (is(cleanData, "try-error") || identical(nrow(cleanData), as.integer(0)) || sum(cleanData$Conc, na.rm = TRUE) == 0) {
        
        if (is(cleanData, "try-error")) {
            
            ROutput_Error <- paste(ROutput_Error, capture.output(show(cleanData)), collapse = "\n")
            
        } else {
            
            warning("sum zero, or zero rows returned from stripTrailingZeros in ncaComplete")
            
            ROutput_Error <- 0
        }
        
        # return
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    }

    Conc <- cleanData$Conc
    
    Time <- cleanData$Time
    
    excPoints <- cleanData$excPoints
    
    if (!is.null(usePoints))  { usePoints <- cleanData$usePoints }


    ##################################################################################################
    
    ###  Locate Peak and Trough if present
    
    PEAKCODE <- 2
    
    TROUGHCODE <- 1

    PeakIndex <- which( PeakTrough == PEAKCODE )[1]
    
    ROutput["ROutput_Peak"] <- Conc[PeakIndex]

    TroughIndex <- which( PeakTrough == TROUGHCODE )[1]
    
    ROutput["ROutput_Trough"] <- Conc[TroughIndex]

    ##################################################################################################
    
    ### Cmax and Cmin

    CmaxTmax_Out <- try(CmaxTmax(Conc, Time), silent = TRUE)
    
    CminTmin_Out <- try(CminTmin(Conc, Time), silent = TRUE)
    
    if(is(CmaxTmax_Out, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, CmaxTmax_Out, collapse = "\n")
        
    } else {
       
        ROutput["ROutput_Cmax"] <- CmaxTmax_Out$cmax
        
        ROutput["ROutput_Tmax"] <- CmaxTmax_Out$tmax
    }
    
    if(is(CminTmin_Out, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, CminTmin_Out, collapse = "\n")
        
    } else { 
        
        ROutput["ROutput_Cmin"] <- CminTmin_Out$cmin
        
        ROutput["ROutput_Tmin"] <- CminTmin_Out$tmin
    }
    
    
    ### Interpolated Paramters
    
    ## Clast
    
    ClastTlast_Out <- try(ClastTlast(Conc, Time), silent = TRUE)
    
    if( class(ClastTlast_Out)[1] == "try-error" ) {
        
        ROutput_Error <- paste(ROutput_Error, ClastTlast_Out, collapse = "\n")
        
    } else {
        
        ROutput["ROutput_CLast"] <- ClastTlast_Out$clast
        
        ROutput["ROutput_TLast"] <- ClastTlast_Out$tlast
    }
    
    ## AUCLast
    
    ROutput_AUCLast <- try(AUCLast(Conc = Conc, Time = Time, addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if (is(ROutput_AUCLast, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, ROutput_AUCLast, collapse = "\n")
        
    } else {
        
        ROutput["ROutput_AUCLast"] <- ROutput_AUCLast
    }

    ## AUMCLast

    ROutput_AUMCLast <- try(AUCLast(Conc = Conc * Time, Time = Time, addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if (is(ROutput_AUMCLast, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, ROutput_AUMCLast, collapse = "\n")
        
    } else {
        
        ROutput["ROutput_AUMCLast"] <- ROutput_AUMCLast
    }
    
    ## MRTLast
    
    value <- try(MRTSD(AUC = ROutput["ROutput_AUCLast"], AUMC = ROutput["ROutput_AUMCLast"], Dof = Dof), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    }
    
    # if errors have occured

    if( ROutput_Error != "" ) {
        
        # return with error
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    }
    
    ROutput["ROutput_MRTLast"] <- value
    
    ROutput["ROutput_Dose"] <- Dose
    
    ROutput["ROutput_Dof"] <- Dof
    
    
    ##################################################################################################
    
    ### Extrapolated Parameters
    
    ## Terminal phase calculation 
    
    doPoints <- list(ACTION = "fail")
    
    # T=0 not checked here
    
    doPoints <- chooseNumPointsAction(Conc = Conc, Time = Time, numPoints = numPoints, 
        usePoints = usePoints, excPoints = excPoints)
    
    result <- as.numeric(rep(NA, 9))
    
    lzColNames <- c("Lambdaz", "intercept", "r2", "adjr2", "rhoXY", "tPhaseHalfLife", "LambdazLower", "LambdazUpper", "numPoints")
    
    names(result) <- lzColNames
    
    numPoints_result <- try(switch(doPoints[["ACTION"]], 
        
            # if numPoints is zero or less, suppress terminal phase calculation
            
            none = list(numPoints = as.numeric(NA), result = result),
            
            # if numPoints is one or more, suppress automatic selection
            
            fixed = fixedPoints(Conc = Conc, Time = Time, numPoints = numPoints, excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]),
            
            # if numPoints is NA, perform automatic point selection
            
            auto = selectPoints(Conc = Conc, Time = Time, minPoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars", excPoints = excPoints),
            
            # if usePoints is logical, calculate lambdaz using specified subset of data
            
            used = usedPoints(Conc = Conc, Time = Time, usePoints = usePoints, excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]]), 
            
            # else error
            
            stop(paste("Error in ncaComplete: doPoints action was", doPoints[["ACTION"]], sep = "", collapse = ""))), 
        
        silent = TRUE)
    
    # if terminal phase is not required or cannot be calculated, return interpolated only
    # if error, return with error
    
    if (is(numPoints_result, "try-error") || is.na(numPoints_result$numPoints)) {
        
        if (ROutput_Error == "" & is.na(numPoints_result$numPoints)) { 
            
            ROutput_Error <- 0 
            
        } else {
            
            ROutput_Error <- paste(ROutput_Error, capture.output(numPoints_result), sep = "", collapse = "")
        }
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
    }
    
    # lambdaz has been calculated, so add elements to output vector
    
    roColNames <- c("ROutput_LambdaZ", "ROutput_intercept", "ROutput_r2", 
        "ROutput_adjr2", "ROutput_rhoXY", "ROutput_HalfLife", 
        "ROutput_LambdazLower", "ROutput_LambdazUpper", "ROutput_numPoints")
    
    ROutput[roColNames] <- numPoints_result$result[lzColNames]
    

    ## AUCInfObs
    
    value <- try(AUCInfObs(Conc = Conc, Time = Time, lambdaZStats = numPoints_result$result, calculation = "standard", addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if(is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_AUCInfObs"] <- value }
    
    
    value <- try(pcExtrap(ROutput["ROutput_AUCInfObs"], ROutput["ROutput_AUCLast"]), silent = TRUE)
    
    if(is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_AUC_Percent_Extrapolated_obs"] <- value }


    ## AUMCInfObs
       
    value <- try(AUCInfObs(Conc = Conc, Time = Time, lambdaZStats = numPoints_result$result, calculation = "moment", addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if (is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_AUMCInfObs"] <- value }

        
    ## check for errors and return if present    
        
    value <- try(pcExtrap(ROutput["ROutput_AUMCInfObs"], ROutput["ROutput_AUMCLast"]), silent = TRUE)
    
    if (is(value, "try-error") || ROutput_Error != "") {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")

        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput) 
        
    } else { ROutput["ROutput_AUMC_Percent_Extrapolated_obs"] <- value }


    ## AUCInfPred
    
    value <- try(AUCInfPred(Conc = Conc, Time = Time, lambdaZStats = numPoints_result$result, calculation = "standard", addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if (is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n") 
        
    } else { ROutput["ROutput_AUCInfPred"] <- value }
    
    
    value <- try(pcExtrap(ROutput["ROutput_AUCInfPred"], ROutput["ROutput_AUCLast"]), silent = TRUE)
    
    if (is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_AUC_Percent_Extrapolated_pred"] <- value }
    
    
    ## AUMCInfPred
    
    value <- try(AUCInfPred(Conc = Conc, Time = Time, lambdaZStats = numPoints_result$result, calculation = "moment", addT0 = addT0, Safe = Safe, inter = inter), silent = TRUE)
    
    if (is(value, "try-error")) {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_AUMCInfPred"] <- value }
    
    
    ## check for errors and return if present    
    
    value <- try(pcExtrap(ROutput["ROutput_AUMCInfPred"], ROutput["ROutput_AUMCLast"]), silent = TRUE)
    
    if (is(value, "try-error") || ROutput_Error != "") {
    
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
    
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    } else { ROutput["ROutput_AUMC_Percent_Extrapolated_pred"] <- value }
    


    ## Clearance
    
    value <- try(clearance(ROutput["ROutput_AUCInfObs"], Dose, Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_ClObs" ] <- value }
    
    
    value <- try(clearance(ROutput["ROutput_AUCInfPred"], Dose, Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_ClPred"] <- value }
    

    ##  Mean Residence Time Inf Obs

    value <- try(MRTSD(AUC = ROutput["ROutput_AUCInfObs"], AUMC = ROutput["ROutput_AUMCInfObs"], Dof = Dof), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_MRTInfObs" ] <- value }

    
    ## check for errors and return if present  

    ##  Mean Residence Time Inf Pred
    
    value <- try(MRTSD(AUC = ROutput["ROutput_AUCInfPred"], AUMC = ROutput["ROutput_AUMCInfPred"], Dof = Dof), silent = TRUE)
    
    if (is(value, "try-error") || ROutput_Error != "") {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    } else { ROutput["ROutput_MRTInfPred"] <- value }
    
    
    ## Terminal Volume Obs
    
    value <- try(VZ(lambdaz = ROutput["ROutput_LambdaZ"], AUCInf = ROutput["ROutput_AUCInfObs"], Dose = Dose, Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_VzObs" ] <- value }

    ## Terminal Volume Pred
    
    value <- try(VZ(lambdaz = ROutput["ROutput_LambdaZ"], AUCInf = ROutput["ROutput_AUCInfPred"], Dose = Dose, Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_VzPred"] <- value }


    ## Steady State Volume Obs
    
    value <- try(VSS(MRT = ROutput["ROutput_MRTInfObs" ], CL = ROutput["ROutput_ClObs" ], Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error")) {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
    } else { ROutput["ROutput_VssObs" ] <- value }


    ## check for errors and return if present  
    
    ## Steady State Volume Pred
    
    value <- try(VSS(MRT = ROutput["ROutput_MRTInfPred"], CL = ROutput["ROutput_ClPred" ], Safe = Safe), silent = TRUE)
    
    if (is(value, "try-error") || ROutput_Error != "") {
        
        ROutput_Error <- paste(ROutput_Error, value, collapse = "\n")
        
        ROutput <- as.data.frame(as.list(ROutput))
        
        ROutput[, "ROutput_Error"] <- ROutput_Error
        
        return(ROutput)
        
    } else { ROutput["ROutput_VssPred"] <- value }


    
    ##################################################################################################


    ROutput <- as.data.frame(as.list(ROutput))
    
    return(ROutput)

}


#' Decide whether to use or automatically calculate numPoints
#'
#' @title Choose how to handle numPoints
#' @param Conc Vector of concentrations
#' @param Time Vector of times, must be ordered in ascending order and should not have duplicates
#' @param numPoints single numeric value declaring number of points to use
#' @param usePoints If \code{NULL} (default) automatically select, else, logical vector of points to use for calculation of terminal phase. Used rows are flagged by usePoints as \code{TRUE}.
#' @param Logical vector (\code{FALSE} by default) excludeing \code{TRUE} rows from automatic calculation of terminal phase.
#' @return length 2 list with elements ACTION, a single character value, and MINROWSFORLAMBDAZ, a single numeric with value 3
#' @author Mango Solutions
#' @keywords math

chooseNumPointsAction <- function(Conc = Conc, Time = Time, numPoints = as.numeric(NA), 
    usePoints = NULL, excPoints = FALSE) {
    
    # determine whether lambdaz should be calculated and how (auto select points by default)
    
    checkNumericSameLength(Time, Conc, "Time", "Concentration", "chooseNumPointsAction")
    
    checkSingleNumeric(numPoints, description = "Number of Points", "chooseNumPointsAction")
    
    if(!is.null(usePoints)) {
        
        checkLogicalSameLength(usePoints, Conc, "Used Points", "Concentration", "chooseNumPointsAction")
    }
    
    if (identical(excPoints, FALSE)) { excPoints <- rep(FALSE, times = length(Time)) }
    
    checkLogicalSameLength(excPoints, Conc, "Excluded Points", "Concentration", "chooseNumPointsAction")
    
    doPoints <- list(ACTION = "fail", MINROWSFORLAMBDAZ = 3)
    
    if (any(!is.na(numPoints))) {
        
        # numPoints is not NA; user selection of numPoints, so usePoints should not be supplied
        
        if (any(!is.null(usePoints))) {
            
            stop("either numPoints or usePoints should be supplied, not both")
        }
        
        doPoints[["ACTION"]] <- "auto"
        
        if (any(numPoints <= 0)) { doPoints[["ACTION"]] <- "none" }
        
        if (all(numPoints > 0)) { doPoints[["ACTION"]] <- "fixed" }
        
        # If numPoints is less than MINROWSFORLAMBDAZ, do not calculate lambdaz
        
        if (numPoints < doPoints[["MINROWSFORLAMBDAZ"]]) { doPoints[["ACTION"]] <- "none" } 
        
    } else {
        
        # numPoints is NA; automatic selection
        
        doPoints[["ACTION"]] <- "auto"
        
        if (any(!is.null(usePoints))) {
            
            # range selection overrules automatic selection
            
            doPoints[["ACTION"]] <- "used"
            
        }
    }
    
    # If there are insufficient trailing endpoints for lambdaz calculation 
    # return ROutput without errors i.e. none
    
    if (!testTrailPoints(Conc = Conc, Time = Time, usePoints = usePoints, 
        excPoints = excPoints, minPoints = doPoints[["MINROWSFORLAMBDAZ"]])) { 
        
        doPoints[["ACTION"]] <- "none"
    }
    
    return(doPoints)

}

