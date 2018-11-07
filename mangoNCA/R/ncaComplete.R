#
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Perform Non-Compartmental Analysis for Complete concentration-time Datasets
#'
#' \code{ncaComplete} calculates NCA parameters for individuals returning
#' as many parameters as are calculable.
#' \code{CPEAK} and \code{CTROUGH} will always be returned where possible.
#' The final column of the return is \code{ERROR}. This should be zero.
#' \code{ncaComplete} will \code{try} to call the functions to calculate the
#' NCA parameters.
#' If it is not, an error has occured.
#' The text of each error will appear in this column as a single character string
#' with individual messages separated by a newline.
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
#' @title Non-Compartmental Analysis on concentration-time Data
#' @inheritParams getNCAnalysis
#' @return Data frame
#' @author Mango Solutions
#' @export
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' ncaComplete(conc = Theoph1$conc, time = Theoph1$Time,
#'     dose = Theoph1$Dose[1], duration = 1)

ncaComplete <- function(conc, time, dose, duration, lamznpt = NA_real_,
    usepoints = NULL, excpoints = FALSE, addt0 = FALSE, inter = "Linear", 
    maxdiffrsq = 1e-4, minr2adj = 0.8, numhalflife = 1, output) {

    error <- ""

    # Check data for gross errors

    check01 <- try(checkOrderedVector(time, description = "time",
        functionName = "ncaComplete"), silent = TRUE)

    check02 <- try(checkNumericSameLength(time, conc, "time", "concentration",
        "ncaComplete"), silent = TRUE)

    error <- paste(check01, check02, sep = "", collapse = "")

    if (!identical(x = error, y = "")) {

        output["ERROR"] <- error

        return(output)
    }
    # removed checks for PeakTrough

    check05 <- try(checkSingleNumeric(dose, description = "dose",
        "ncaComplete"), silent = TRUE)

    check06 <- try(checkSingleNumeric(duration,
        description = "Duration of Infusion", "ncaComplete"), silent = TRUE)

    check07 <- try(checkSingleNumeric(lamznpt,
        description = "Number of Points", "ncaComplete"), silent = TRUE)

    if(!is.null(usepoints)) {
        check08 <- try(checkLogicalSameLength(usepoints, conc,
            "usepoints", "concentration", "ncaComplete"), silent = TRUE)
    } else {
        check08 <- NULL
    }

    if (identical(excpoints, FALSE)) {
        excpoints <- rep(FALSE, times = length(time)) }

    check09 <- try(checkLogicalSameLength(excpoints, time, "excpoints",
        "time", "ncaComplete"), silent = TRUE)

    check10 <- try(checkSingleLogical(addt0, description = "Add Row at T = 0?",
        "ncaComplete"), silent = TRUE)

    check11 <- try(checkSingleCharacter(inter, "inter",
        "ncaComplete"), silent = TRUE)

    if(!identical(as.integer(39), length(output))
        || is.null(names(output))) {

        check12 <- "error in ncaComplete: output should be a named vector of length 39"

    } else {

        check12 <- NULL
    }

    # return if gross data errors present coerce output to data frame
    # add errors and return

    error <- paste(check05, check06, check07,
        check08, check09, check10, check11, check12,
        sep = "", collapse = "")

    if (!identical(x = error, y = "")) {

        # return with error

        output["ERROR"] <- error

        return(output)

    } else { error <- "" }

    # if addt0 is TRUE fix data errors (add T = 0 to data if it
    # is missing and remove missing values)
    # otherwise throw exception if data errors are present
    # if no rows are returned by stripTrailingZeros, return empty
    # data frame without error

    cleanData <- try(stripTrailingZeros(conc = conc, time = time,
            usepoints = usepoints, excpoints = excpoints, addt0 = addt0, checkT0 = TRUE),
        silent = TRUE)

    if (is(cleanData, "try-error") || identical(nrow(cleanData),
        as.integer(0)) || sum(cleanData$conc, na.rm = TRUE) == 0) {

        if (is(cleanData, "try-error")) {
            error <- paste(error, capture.output(show(cleanData)), collapse = "\n")
        } else {
            warning("sum zero, or zero rows returned from stripTrailingZeros in ncaComplete")
            error <- 0
        }
        # return
        output[, "ERROR"] <- error
        return(output)
    }
    conc <- cleanData$conc

    time <- cleanData$time

    excpoints <- cleanData$excpoints

    if (!is.null(usepoints))  { usepoints <- cleanData$usepoints }
    
    ### Cmax and Cmin
    CmaxTmax_Out <- try(CmaxTmax(conc, time), silent = TRUE)

    CminTmin_Out <- try(CminTmin(conc, time), silent = TRUE)

    if(is(CmaxTmax_Out, "try-error")) {

        error <- paste(error, CmaxTmax_Out, collapse = "\n")

    } else {

        output["CMAX"] <- CmaxTmax_Out$cmax

        output["TMAX"] <- CmaxTmax_Out$tmax
    }

    if(is(CminTmin_Out, "try-error")) {

        error <- paste(error, CminTmin_Out, collapse = "\n")

    } else {

        output["CMIN"] <- CminTmin_Out$cmin

        output["TMIN"] <- CminTmin_Out$tmin
    }
    ### Interpolated Paramters

    ## Clast
    ClastTlast_Out <- try(ClastTlast(conc, time), silent = TRUE)

    if( class(ClastTlast_Out)[1] == "try-error" ) {

        error <- paste(error, ClastTlast_Out, collapse = "\n")

    } else {

        output["CLST"] <- ClastTlast_Out$clast

        output["TLST"] <- ClastTlast_Out$tlast
    }

    ## AUCLast
    AUCLST <- try(AUCLast(conc = conc, time = time, addt0 = addt0, inter = inter), silent = TRUE)

    if (is(AUCLST, "try-error")) {

        error <- paste(error, AUCLST, collapse = "\n")

    } else {

        output["AUCLST"] <- AUCLST
    }
    ## AUMCLast
    AUMCLST <- try(AUCLast(conc = conc * time, time = time, addt0 = addt0, inter = inter), silent = TRUE)

    if (is(AUMCLST, "try-error")) {

        error <- paste(error, AUMCLST, collapse = "\n")

    } else {

        output["AUMCLST"] <- AUMCLST
    }

    ## MRTLast
    value <- try(MRTSD(AUC = output["AUCLST"],
        AUMC = output["AUMCLST"], duration = duration), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")
    }

    # if errors have occured
    if (!identical(x = error, y = "")) {

        # return with error

        output[, "ERROR"] <- error

        return(output)
    }

    output["MRTLST"] <- value

    output["DOSE"] <- dose

    output["INTDOSE"] <- duration

    ###########################################################################

    ### Extrapolated Parameters
    # TODO merge this block with getConcentration, getPartialAUC,
    # AUCPartial as getLambdaZStats **
    ## Terminal phase calculation

    doPoints <- list(ACTION = "fail")

    # T=0 not checked here

    doPoints <- chooseNumPointsAction(conc = conc, time = time, lamznpt = lamznpt,
        usepoints = usepoints, excpoints = excpoints)

    result <- rep(NA_real_, times = 9)

    lzColNames <- c("LAMZ", "intercept", "R2", "R2ADJ",
        "CORRXY", "LAMZHL",
        "LAMZLL", "LAMZUL", "lamznpt")

    names(result) <- lzColNames

    lamznpt_result <- try(switch(doPoints[["ACTION"]],

            # if lamznpt is zero or less, suppress terminal phase calculation

            none = list(lamznpt = NA_real_, result = result),

            # if lamznpt is one or more, suppress automatic selection

            fixed = fixedPoints(conc = conc, time = time, lamznpt = lamznpt,
                excpoints = excpoints, minpoints = doPoints[["MINROWSFORLAMBDAZ"]]),

            # if lamznpt is NA, perform automatic point selection

            auto = selectPoints(conc = conc, time = time,
                minpoints = doPoints[["MINROWSFORLAMBDAZ"]], method = "ars",
                maxdiffrsq = maxdiffrsq, minlambdaz = 0,
                minr2adj = minr2adj, numhalflife = numhalflife,
                excpoints = excpoints),

            # if usepoints is logical, calculate lambdaz using specified
            # subset of data

            used = usedPoints(conc = conc, time = time, usepoints = usepoints,
                excpoints = excpoints, minpoints = doPoints[["MINROWSFORLAMBDAZ"]]),

            # else error

            stop(paste("Error in ncaComplete: doPoints action was",
                    doPoints[["ACTION"]], sep = "", collapse = ""))),

        silent = TRUE)

    # if terminal phase is not required or cannot be calculated,
    # return interpolated only
    # if error, return with error
    if (is(lamznpt_result, "try-error") || is.na(lamznpt_result$lamznpt)) {

        if (!is(lamznpt_result, "try-error") &&
            error == "" && 
            is.na(lamznpt_result$lamznpt)) {
            error <- 0

        } else {
            error <- paste(error, capture.output(lamznpt_result),
                sep = "", collapse = "")
        }

        output <- as.data.frame(as.list(output))

        output[, "ERROR"] <- error

        return(output)
    }

    # lambdaz has been calculated, so add elements to output vector
    roColNames <- c("LAMZ", "INTERCEPT", "R2",
        "R2ADJ", "CORRXY", "LAMZHL",
        "LAMZLL", "LAMZUL", "LAMZNPT")

    output[roColNames] <- lamznpt_result$result[lzColNames]

    ## AUCInfObs
    value <- try(
        AUCInfObs(conc = conc, time = time,
            lambdaZStats = lamznpt_result$result,
            calculation = "standard", addt0 = addt0, inter = inter),
        silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["AUCIFO"] <- value }


    value <- try(pcExtrap(output["AUCIFO"], output["AUCLST"]), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["AUCPEO"] <- value }
    
    ## AUMCInfObs
    value <- try(
        AUCInfObs(conc = conc, time = time,
            lambdaZStats = lamznpt_result$result, calculation = "moment",
            addt0 = addt0, inter = inter),
        silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["AUMCIFO"] <- value }

    ## check for errors and return if present
    value <- try(pcExtrap(output["AUMCIFO"], output["AUMCLST"]), silent = TRUE)

    if (is(value, "try-error") || !identical(x = error, y = "")) {

        error <- paste(error, value, collapse = "\n")

        output[, "ERROR"] <- error

        return(output)

    } else { output["AUMCPEO"] <- value }
    
    ## AUCInfPred
    value <- try(
        AUCInfPred(conc = conc, time = time,
            lambdaZStats = lamznpt_result$result, calculation = "standard",
            addt0 = addt0, inter = inter),
        silent = TRUE)

    if (is(value, "try-error")) {
        error <- paste(error, value, collapse = "\n")
    } else { output["AUCIFP"] <- value }
    
    value <- try(pcExtrap(output["AUCIFP"], output["AUCLST"]), silent = TRUE)
    
    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["AUCPEP"] <- value }
    
    ## AUMCInfPred
    value <- try(
        AUCInfPred(conc = conc, time = time,
            lambdaZStats = lamznpt_result$result,
            calculation = "moment",
            addt0 = addt0, inter = inter), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["AUMCIFP"] <- value }
    
    ## check for errors and return if present
    value <- try(pcExtrap(output["AUMCIFP"], output["AUMCLST"]), silent = TRUE)

    if (is(value, "try-error") || !identical(x = error, y = "")) {

        error <- paste(error, value, collapse = "\n")

        output[, "ERROR"] <- error

        return(output)

    } else { output["AUMCPEP"] <- value }

    ## Clearance
    value <- try(clearance(output["AUCIFO"], dose), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["CLO" ] <- value }


    value <- try(clearance(output["AUCIFP"], dose), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["CLP"] <- value }

    ##  Mean Residence time Inf Obs
    value <- try(MRTSD(AUC = output["AUCIFO"], AUMC = output["AUMCIFO"], duration = duration), silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["MRTIFO" ] <- value }

    ## check for errors and return if present
    ##  Mean Residence time Inf Pred
    value <- try(
        MRTSD(AUC = output["AUCIFP"], AUMC = output["AUMCIFP"], duration = duration), silent = TRUE)
    
    if (is(value, "try-error") || !identical(x = error, y = "")) {
        
        error <- paste(error, value, collapse = "\n")
        
        output[, "ERROR"] <- error
        
        return(output)
        
    } else { output["MRTIFP"] <- value }
    
    ## Terminal Volume Obs
    value <- try(
        VZ(lambdaz = output["LAMZ"], AUCInf = output["AUCIFO"],
            dose = dose),
        silent = TRUE)
    
    if (is(value, "try-error")) {
        error <- paste(error, value, collapse = "\n")
    } else { output["VZO" ] <- value }
    
    ## Terminal Volume Pred
    value <- try(
        VZ(lambdaz = output["LAMZ"], AUCInf = output["AUCIFP"],
            dose = dose),
        silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["VZP"] <- value }
    
    ## Steady State Volume Obs
    value <- try(
        VSS(MRT = output["MRTIFO" ], CL = output["CLO" ]),
        silent = TRUE)

    if (is(value, "try-error")) {

        error <- paste(error, value, collapse = "\n")

    } else { output["VSSO" ] <- value }
    ## check for errors and return if present

    ## Steady State Volume Pred
    value <- try(VSS(MRT = output["MRTIFP"], CL = output["CLP" ]), silent = TRUE)

    if (is(value, "try-error") || !identical(x = error, y = "")) {

        error <- paste(error, value, collapse = "\n")

        output[, "ERROR"] <- error

        return(output)

    } else { output["VSSP"] <- value }

    ###########################################################################

    return(output)
}
#' @title Choose how to handle lamznpt
#'
#' @description Decide whether to use or automatically calculate lamznpt
#'
#' @inheritParams getNCAnalysis
#' @return length 2 list with elements ACTION, a single character value,
#' and MINROWSFORLAMBDAZ, a single numeric with value 3
#' @author Mango Solutions

chooseNumPointsAction <- function(conc = conc, time = time, lamznpt = NA_real_,
    usepoints = NULL, excpoints = FALSE) {

    # determine whether lambdaz should be calculated and how (auto select points by default)
    checkNumericSameLength(time, conc, "time", "concentration", "chooseNumPointsAction")

    checkSingleNumeric(lamznpt, description = "Number of Points", "chooseNumPointsAction")

    if(!is.null(usepoints)) {

        checkLogicalSameLength(usepoints, conc, "Used Points", "concentration", "chooseNumPointsAction")
    }

    if (identical(excpoints, FALSE)) { excpoints <- rep(FALSE, times = length(time)) }

    checkLogicalSameLength(excpoints, conc, "Excluded Points", "concentration", "chooseNumPointsAction")

    doPoints <- list(ACTION = "fail", MINROWSFORLAMBDAZ = 3)

    if (any(!is.na(lamznpt))) {

        # lamznpt is not NA; user selection of lamznpt,
        # so usepoints should not be supplied
        if (any(!is.null(usepoints))) {

            stop("either lamznpt or usepoints should be supplied, not both")
        }

        doPoints[["ACTION"]] <- "auto"

        if (any(lamznpt <= 0)) { doPoints[["ACTION"]] <- "none" }

        if (all(lamznpt > 0)) { doPoints[["ACTION"]] <- "fixed" }

        # If lamznpt is less than MINROWSFORLAMBDAZ, do not calculate lambdaz
        if (lamznpt < doPoints[["MINROWSFORLAMBDAZ"]]) {
            doPoints[["ACTION"]] <- "none"
        }
    } else {

        # lamznpt is NA; automatic selection
        doPoints[["ACTION"]] <- "auto"

        if (any(!is.null(usepoints))) {

            # range selection overrules automatic selection
            doPoints[["ACTION"]] <- "used"
        }
    }

    # If there are insufficient trailing endpoints for lambdaz calculation
    # return ROutput without errors i.e. none

    if (!testTrailPoints(conc = conc, time = time, usepoints = usepoints,
        excpoints = excpoints, minpoints = doPoints[["MINROWSFORLAMBDAZ"]])) {

        doPoints[["ACTION"]] <- "none"
    }

    return(doPoints)
}


#' @name shapeROutput
#' @title Expected Output Shape
#' @description list with names
#' @docType data
#' @format data frame with columns:
#'  \itemize{
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
#' @keywords datasets

NULL
