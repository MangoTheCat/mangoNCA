
# Original author: plobb
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Perform Non-Compartmental Analysis for a concentration-time repeated measures dataset.
#'
#' This is a wrapper function designed to be the interface for performing a complete Non-Compartmental Analysis.
#' The function has required arguments: \enumerate{
#'      \item conc a vector of concentration values of length equal to \code{length(time)}
#'      \item time a corresponding vector of measurement time values.
#'      \item dose a single numeric value declaring dose
#'      \item duration (duration of infusion), a single numeric value declaring duration of infusion.
#' }
#' Optional arguments are: \enumerate{
#'      \item lamznpt, a single integer value declaring number of points to use in \code{lambdazStatistics}
#'      when calculating terminal phase elimination.
#'      If lamznpt is 0 (zero), terminal phase calculations are supressed.
#'      By default, lamznpt is NULL, declaring that lamznpt should automatically be calculated by \code{selectPoints} using the default method.
#'      \item usepoints suppresses automatic selection of terminal phase;
#'      all rows which are \code{TRUE} will be used to calculate lambdaz. This argument must not be used with lamznpt.
#'      \item excpoints uses automatic selection, but all rows which are \code{TRUE} are excluded
#'      from the terminal phase calculation. If usepoints and excpoints are both supplied, then automatic selection will be supressed,
#'      but excpoints will be omitted in addition to rows excluded by usepoints.
#'      If supplied with lamznpt, excluded values will not be counted in points used for lambdaz.
#'      \item inter, a single character stating whether the interpolation method used is \code{"Linear"} (default) or \code{"Linear Log"}
#' }
#' No unit conversion is performed by these functions.
#' \code{getNCAnalysis} returns a data frame with 39 columns containing 1 row of numeric results.
#' The final column of the return is \code{ERROR}. This should be zero. If it is not, an error
#' has occured. The text of each error will appear in this column as a single character string with
#' individual messages separated by a newline.
#' If there are three or fewer rows, data is passed to \code{\link{ncaPeakTrough}} for Peak/Trough identification only.
#' If there are four or more rows, data is passed to \code{\link{ncaComplete}} for full non-compartmental analysis.
#' Returns a data frame with columns:
#' \enumerate{
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
#' @param conc Vector of conc
#' @param time Vector of time, must be ordered in ascending order and should not have duplicates
#' @param dose Single numeric value of dose
#' @param duration Single numeric value of duration of infusion
#' @param lamznpt If \code{NULL} (default) automatically select, else,
#' single numeric number of points for calculation of terminal phase.
#' @param usepoints If \code{NULL} (default) automatically select, else,
#' logical vector of points to use for calculation of terminal phase.
#' Used rows are flagged by usepoints as \code{TRUE}.
#' @param excpoints If \code{NULL} (default) automatically select, else,
#' logical vector of points to exclude from automatic calculation of terminal phase.
#' Excluded rows are flagged by excpoints as \code{TRUE}.
#' @param inter Single character stating whether the interpolation method used is
#' \code{"Linear"} (default), \code{"Lin up Log down"} or \code{"Linear Log"}
#' @param maxdiffrsq single numeric in range 0-1 The Adjusted R-squared method
#' will select the number of points
#' for terminal phase calculation for the set of trailing points with the
#' most points that is within maxdiffrsq of the maximum adjusted R-squared
#' (default 1e-4)
#' @param minr2adj single numeric Minimum value permitted for
#' adjusted R-squared for lambda-z to be calculated.
#' If NA or NULL, lambda-z calculation is supressed. (default 0.5)
#' @param numhalflife single numeric Multiplier for terminal phase half life,
#' when checking that terminal phase half life is not
#' larger than the range of interpolated data
#' (i.e. extrapolated area is not a large portion of AUC0_Inf).
#' If LAMZHL * numhalflife > LAMZUL - LAMZLL, terminal phase
#' statistics will not be returned.
#' To always return terminal phase statistics, set to 0.
#' To never return terminal phase statistics set to Inf, NULL or NA. (default 1)
#' @param type single character with value "Single", or "Multiple", does project
#' contain single dose or multiple dose data? (default "Single")
#' @param tau single numeric >0 or NULL must be NULL if type Single,
#' must be single numeric if type Multiple(default NULL)
#' @param addt0 single logical should t=0, c=0
#' be added to time if missing? (default FALSE)
#' @title API wrapper for NCA analysis pharmacodynamic data
#' @return Data frame
#' @export
#' @author Mango Solutions
#' @keywords nca
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' getNCAnalysis(conc = Theoph1$conc, time = Theoph1$Time,
#'     dose = 10, duration = 1)

getNCAnalysis <- function(conc = NULL, time = NULL,
    dose = NULL, duration = NULL,
    lamznpt = NULL, usepoints = NULL, excpoints = NULL,
    inter = c("Linear", "Lin up Log down", "Linear Log"),
    maxdiffrsq = 1e-4, minr2adj = 0.5, numhalflife = 1,
    type = c("Single", "Multiple"), tau = NULL, addt0 = FALSE)  {
    # Initialise data check return object
    error <- ""
    ROutput <- c(rep(NA_real_, times = 38L), 0L)

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
    check01 <- try(checkOrderedVector(time,
            description = "time",
            functionName = "getNCAnalysis"), silent = TRUE)

    check02 <- try(checkNumericSameLength(time, conc, "time", "concentration",
            functionName = "getNCAnalysis"), silent = TRUE)

    # return if gross data errors present

    error <- paste(check01, check02, sep = "", collapse = "")

    if (!identical(x = error, y = "")) {

        # coerce to data frame and return

        ROutput <- as.data.frame(as.list(ROutput))

        ROutput["ERROR"] <- error

        return(ROutput)
    }

    # check03, depracated

    check04 <- try(checkSingleNumeric(addt0,
        description = "addt0",
        functionName = "getNCAnalysis"), silent = TRUE)

    check05 <- try(checkSingleNumeric(dose,
            description = "dose",
            functionName = "getNCAnalysis"), silent = TRUE)

    check06 <- try(checkSingleNumeric(duration,
            description = "Duration of Infusion",
            functionName = "getNCAnalysis"), silent = TRUE)

    if (!is.null(lamznpt) & !is.null(usepoints)) {

        check07 <- "usepoints was provided to getNCAnalysis in addition to lamznpt"

    } else {

        if (is.null(lamznpt)) {

            lamznpt <- NA_real_
        }

        check07 <- try(checkSingleNumeric(lamznpt,
                description = "Number of Points",
                functionName = "getNCAnalysis"), silent = TRUE)
    }

    if (!is.null(usepoints)) {

        check08 <- try(checkLogicalSameLength(usepoints, conc, "usepoints", "concentration",
                functionName = "getNCAnalysis"), silent = TRUE)

    } else {

        check08 <- NULL
    }

    if (is.null(excpoints)) {

        excpoints <- rep(FALSE, times = length(time))
    }

    check09 <- try(checkLogicalSameLength(excpoints, conc, "excpoints", "concentration",
            functionName = "getNCAnalysis"), silent = TRUE)

    inter <- try(inter <- match.arg(inter), silent = TRUE)
    check10 <- ""
    if (is(inter, "try-error")) { check10 <- paste(inter) }

    check11 <- try(checkSingleNumeric(maxdiffrsq,
            description = "max difference in adj R-squared for greatest number of points",
            functionName = "getNCAnalysis"), silent = TRUE)

    check12 <- try(checkSingleNumeric(minr2adj,
            description = "min adj R-squared which allows lambda-z to be returned",
            functionName = "getNCAnalysis"), silent = TRUE)

    check13 <- try(checkSingleNumeric(numhalflife,
            description = "minimum proportion of halflife permitted for lambda-z data range",
            functionName = "getNCAnalysis"), silent = TRUE)

    type <- try(type <- match.arg(type), silent = TRUE)
    check14 <- ""
    if (is(inter, "try-error")) { check14 <- paste(inter) }

    if (!is.null(tau)) {

        check15 <- try(checkSingleNumeric(tau, description = "tau",
                functionName = "getNCAnalysis"), silent = TRUE)

    } else {

        check15 <- NULL
    }

    # return if gross data errors present

    error <- paste(check05, check06, check07,
        check08, check09, check10, check11, check12, check13, check14, check15,
        sep = "", collapse = "")

    if (!identical(x = error, y = "")) {

        # coerce to data frame and return

        ROutput <- as.data.frame(as.list(ROutput))

        ROutput["ERROR"] <- error

        return(ROutput)
    }

    # perform complete NCA provided there are more rows than THRESHOLDCOMPLETE,
    # otherwise only check for Peak and Trough return results from
    # ncaComplete or ncaPeakTrough provided they match expected structure

    THRESHOLDCOMPLETE <- 3L

    if (length(time) > THRESHOLDCOMPLETE) {

        ROutput_ncaComplete <- ncaComplete(conc = conc, time = time,
            dose = dose, duration = duration, lamznpt = lamznpt,
            usepoints = usepoints, excpoints = excpoints, ROutput = ROutput,
            inter = inter, maxdiffrsq = maxdiffrsq, minr2adj = minr2adj,
            numhalflife = numhalflife, addt0 = addt0)

        if (identical(names(ROutput_ncaComplete), names(ROutput)) &&
                identical(1L, nrow(ROutput_ncaComplete))) {
            ROutput <- ROutput_ncaComplete
        } else {
            ROutput["ERROR"] <- "error in getNCAnalysis: return object from ncaComplete did not have expected structure\n"
        }
    } else {

        ROutput_ncaPeakTrough <- ncaPeakTrough(conc = conc, time = time,
            dose = dose, duration = duration, ROutput = ROutput)

        if(identical(names(ROutput_ncaPeakTrough), names(ROutput)) && identical(1L, nrow(ROutput_ncaPeakTrough))) {
            ROutput <- ROutput_ncaPeakTrough
        } else {
            ROutput["ERROR"] <- "error in getNCAnalysis: return object from ncaPeakTrough did not have expected structure\n"
        }
    }
    return(ROutput)
}
