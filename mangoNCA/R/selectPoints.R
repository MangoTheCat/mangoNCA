
# Original author: ccampbell
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' @title Choose Number of Trailing Points to use for Terminal Rate Constant (lambda-z) Calculation.
#'
#' Calculates number of points to use for Terminal Rate Constant (lambda-z).
#' Calculation is based on Adjusted R-squared method.
#' \code{selectPoints} calls \code{\link{lambdaZStatistics}}
#' to perform regressions using the last three rows,
#' then the last four rows, last five, etc.
#' Cmax and prior rows are not used.
#' To manually select specific records, use \code{\link{usedPoints}} instead,
#' by providing usepoints to \code{\link{getNCAnalysis}}.
#' Exclusion of rows prior to the end of infusion is not currently implemented.
#' For each regression, an adjusted R-squared is computed.
#' The regression with the largest adjusted R-squared is
#' selected to estimate lambda-z, with these caveats:
#'  \enumerate{
#'      \item Number of rows with the greatest number of rows AND
#'            within maxdiffrsq (0.0001) of the largest adjusted R2 value is selected.
#'      \item Rows where conc <= 0 are not used in the calculation
#'      \item Trailing rows where conc <= 0 are removed by \code{stripTrailingZeros}
#'            and not counted in number returned
#'      \item Cmax is not included in lambda-z calculation (User Requirements PEX-010)
#'      \item lambda-z must be greater than minlambdaz (0)
#'      \item lambda-z must be calculated using at least minpoints (3) data rows
#'      \item R2ADJ must be at least minr2adj (0.5)
#'      \item LAMZHL * numhalflife (1) must be less than lambda-z interval
#'            (LAMZUL - LAMZLL)
#'  }
#' Zero or missing values will be removed with a warning.
#'
#' @inheritParams getNCAnalysis
#' @param minpoints Single positive integer. Minimum number of points to use for
#'     lambda-z calculation(s) (default 3)
#' @param method Method of selecting points (only Adjusted R-Squared Method implemented).
#' @param minlambdaz single numeric Minimum value permitted for lambda-z to be calculated.
#'     If NA or NULL, lambda-z calculation is supressed. (default 0)
#' @param excpoints Logical vector (\code{FALSE} by default)
#'     excluding \code{TRUE} rows from automatic calculation of terminal phase.
#' @return List consisting of lamznpt, a single numeric value, and result, a length 9 numeric vector.
#' @author Mango Solutions
#' @keywords math
#' @export
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' selectPoints(conc = Theoph1$conc, time = Theoph1$Time)

selectPoints <- function(conc, time, minpoints = 3, method = "ars",
    maxdiffrsq = 1e-4, minlambdaz = 0, minr2adj = 0.5, numhalflife = 1,
    excpoints = FALSE) {

    method <- match.arg(method)

	# data checks, or error
    checkNumericSameLength(time, conc, "time", "conc", functionName = "selectPoints")

    checkSingleNumeric(minpoints, description = "minpoints", functionName = "selectPoints")

    checkOrderedVector(time, description = "time", functionName = "selectPoints")

    if (identical(excpoints, FALSE)) { excpoints <- rep(FALSE, times = length(time)) }

    checkLogicalSameLength(excpoints, conc, "excpoints", "concentration", "selectPoints")

    if (minpoints < 2) {
        stop("Error in selectPoints: minpoints must be at least 2")
    }
    if (maxdiffrsq < 0 || maxdiffrsq > 1) {
        stop("Error in selectPoints: maxdiffrsq must be in range 0-1")
    }
    if (is.null(minlambdaz) || is.na(minlambdaz)) { minlambdaz <- Inf }
    if (is.null(minr2adj) || is.na(minr2adj)) { minr2adj <- Inf }
    if (is.null(numhalflife) || is.na(numhalflife)) { numhalflife <- Inf }

    timeconc <- try(cleanconctime(
            conc = conc, time = time,
            excpoints = excpoints, addt0 = FALSE, checkT0 = FALSE),
        silent = TRUE)

    if (is(timeconc, "try-error")) {
        stop(paste("call to cleanconctime failed, message is: ", timeconc))
    }
    # initialize outputs
    lamznpt <- NA_real_

    result <- rep(NA_real_, 9)

    names(result) <- c("LAMZ", "intercept", "R2", "R2ADJ", "CORRXY",
        "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt")

    # lambdaz must be calculated using at least minpoints (3) data rows after Cmax
    isSufficientRecords <- testTrailPoints(conc = timeconc$conc, time = timeconc$time,
        excpoints = timeconc$excpoints, minpoints = minpoints)

    if (!isSufficientRecords) {
        warning("There are fewer non-zero points between Cmax and Clast than minpoints")
    }

    # Inf minlambdaz, minr2adj or numhalflife suppress terminal phase calculation
    if (minlambdaz != Inf &&
        minr2adj != Inf &&
        numhalflife != Inf &&
        isSufficientRecords) {

        # flag rows before and including Cmax

        CmaxTmax <- CmaxTmax(conc = timeconc$conc, time = timeconc$time)

        CmaxIndex <- CmaxTmax$index

        timeconc$afterCmax <- c(rep(FALSE, times = CmaxIndex),
            rep(TRUE, times = nrow(timeconc) - CmaxIndex))

        # remove excluded values
        # proceed if any rows remain!

        nVals <- sum(timeconc$afterCmax & !timeconc$excpoints)
        if (nVals > 0L) {

            # find index of shortest allowed dataset and count
            # total number of models that will be created
            minIndex <- nVals - (minpoints - 1)
            # object to collect output
            # matrix of
            #[1] "LAMZ"
            #[2] "intercept"
            #[3] "R2"
            #[4] "R2ADJ"
            #[5] "CORRXY"
            #[6] "LAMZHL"
            #[7] "LAMZLL"
            #[8] "LAMZUL"
            #[9] "lamznpt"
            #[10] i

            # TODO implement
            # Minimum AIC
            #
            # AIC = n * (log(2 * pi) + 1) + n * log(RSS / n) + 2 * p
            # update lambdaZStatistics
            # rss is lmFitSummary$sigma

            vals <- matrix(0, nrow = minIndex, ncol = 10)

            vals[, 10] <- seq_len(minIndex)

            ### Regression comparison decision rules

            # 1: LAMZmust be greater than minlambdaz (0)
            # 2: R2ADJ must be at least minr2adj (0.8)
            # 3: LAMZHL * numhalflife must be less than
            #     LAMZcalculatedInterval (LAMZUL - LAMZLL)

            # perform regression for each data subset

            for (i in vals[, 10, drop = TRUE]) {

                # each model with data subsets from Cmax onwards is calculated
                lambdazOut <- lambdaZStatistics(conc = timeconc$conc,
                    time = timeconc$time,
                    lamznpt = nVals - i + 1,
                    checkcmax = FALSE, minpoints = minpoints)

                vals[i, 1:9] <- unlist(lambdazOut)
            }

            # ignore R2ADJ val [4] by replacing with NA based on
            # regression comparison decision rules:

            # 1: [1] lambdaz must be greater than minlambdaz (default 0)
            isLambdazTooSmall <- vals[, 1] <= minlambdaz
            # 2: [4] R2ADJ must be at least minr2adj (default 0.5)
            isAdjRSqTooSmall <- vals[, 4] < minr2adj
            # 3: [6] LAMZHL * numhalflife must be less than
            #     calculatedInterval [8] LAMZUL - [7] LAMZLL
            isHalflifeTooBig <- numhalflife * vals[, 6] > vals[, 8] - vals[, 7]

            vals[isLambdazTooSmall | isAdjRSqTooSmall | isHalflifeTooBig, 4] <- NA_real_

            # find max value if present and return
            # The number of points should INCLUDE omitted points so that index
            # corresponds to the input dataset

            if (!all(is.na(vals[, 4]))) {

                maxadjrs <- max(vals[, 4], na.rm = TRUE)

                valsWithinLimIndex <- which((maxadjrs - vals[, 4]) <= maxdiffrsq)

                if (length(valsWithinLimIndex) > 0) {

                    lamznptIndex <- min(valsWithinLimIndex)

                    lamznpt <- nVals - lamznptIndex + 1

                    result[1:9] <- vals[lamznptIndex, 1:9, drop = TRUE]
                }
            }
        }
    }
	return(list(lamznpt = lamznpt, result = result))
}


#' Use Fixed Number of Trailing Points for Terminal Rate Constant (lambda-z) Calculation.
#'
#' Returns input lamznpt and results for Terminal Rate Constant (lambda-z) calculation
#' fixedPoints calls \code{\link{lambdaZStatistics}} to perform regressions
#' using the specified number of trailing non-zero rows
#' Rows prior to Cmax may be used.
#' Exclusion of rows prior to the end of infusion is not currently implemented.
#' Zero or missing values will error.
#' @inheritParams getNCAnalysis
#' @param minpoints Minimum number of points to use for the lambda-z calculation(s),
#' 3 by default. Single positive integer.
#' @title Fixed Points for Calculating Terminal Rate Constant
#' @return List consisting of lamznpt, a single numeric value, and result,
#' a length 9 numeric vector.
#' @author Mango Solutions
#' @examples
#' Theoph1 <- subset(Theoph, Subject == 1)
#' mangoNCA:::fixedPoints(conc = Theoph1$conc, time = Theoph1$Time, lamznpt = 3)

fixedPoints <- function(conc, time, lamznpt, excpoints = FALSE, minpoints = 3) {

    # data checks, or error
    checkNumericSameLength(time, conc, "time", "conc",
        functionName = "fixedPoints")

    checkOrderedVector(time, description = "time",
        functionName = "fixedPoints")

    if (identical(excpoints, FALSE)) {
        excpoints <- rep(FALSE, times = length(time)) }

    checkLogicalSameLength(excpoints, conc, "excpoints", "concentration",
        functionName = "fixedPoints")

    checkSingleNumeric(lamznpt, description = "lamznpt",
        functionName = "fixedPoints")

    checkSingleNumeric(minpoints, description = "minpoints",
        functionName = "fixedPoints")

    if (minpoints < 2) {
        stop("Error in fixedPoints: minpoints must be at least 2")
    }
    timeconc <- try(cleanconctime(conc = conc, time = time,
        excpoints = excpoints, addt0 = FALSE, checkT0 = FALSE), silent = TRUE)

    if (is(timeconc, "try-error")) {
        stop(paste("call to cleanconctime failed, message is: ", timeconc))
    }

    # initialize outputs

    result <- as.numeric(rep(NA, 9))

    names(result) <- c("LAMZ", "intercept", "R2", "R2ADJ", "CORRXY", "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt")

    # remove excluded values
    timeconc <- timeconc[!timeconc$excpoints, ]

    if (identical(nrow(timeconc), as.integer(0))) {
        return(list(lamznpt = lamznpt, result = result))
    }

    # lambdaz must be calculated using at least minpoints (3) data rows
    # Cmax
    if (any(nrow(timeconc) < minpoints) || any(lamznpt < minpoints)) {

        if (any(nrow(timeconc) < minpoints)) {
            warning("There are fewer non-zero, non-excluded points in the data than minpoints")
        }
        if (any(lamznpt < minpoints)) {
            warning("lamznpt is less than than minpoints")
        }
        lamznpt <- as.numeric(NA)

    } else {
        result[1:9] <- unlist(lambdaZStatistics(conc = timeconc$conc,
            time = timeconc$time, lamznpt = lamznpt, checkcmax = FALSE))
    }
    return(list(lamznpt = lamznpt, result = result))
}


#' @title Use Selection to Calculate Terminal Phase Statistics
#'
#' Value is NA if selection would be less than minpoints.
#' The number of points will not usually be related to the index of the
#' points used, since any rows may be selected by the user
#'
#' @inheritParams getNCAnalysis
#' @param minpoints Single numeric declaring minimum number of points
#' required for lambdaz calculation.
#' @return Single numeric value
#' @author Mango Solutions
#' @examples
#' mangoNCA:::usedPoints(conc = c(435, 363, 113, 47,0),
#'     time = 0:4, usepoints = c(TRUE, TRUE, TRUE, FALSE, FALSE))
#' mangoNCA:::usedPoints(conc = c(435, 363, 113, 47,0),
#'     time = 0:4, usepoints = rep(TRUE, 5),
#'     excpoints = c(FALSE, FALSE, TRUE, FALSE, FALSE))

usedPoints <- function(conc, time, usepoints,
    excpoints = FALSE, minpoints = 3) {

    # data checks, or error
    checkNumericSameLength(time, conc, "time", "conc",
        functionName = "usedPoints")

    checkOrderedVector(time, description = "time",
        functionName = "usedPoints")

    checkLogicalSameLength(usepoints, conc, "usepoints", "concentration",
        functionName = "usedPoints")

    if (identical(excpoints, FALSE)) {
        excpoints <- rep(FALSE, times = length(time)) }

    checkLogicalSameLength(excpoints, conc, "excpoints", "concentration",
        functionName = "usedPoints")

    checkSingleNumeric(minpoints, description = "minpoints",
        functionName = "usedPoints")

    if (minpoints < 2) {
        stop("Error in usedPoints: minpoints must be at least 2") }

    # check data
    timeconc <- try(cleanconctime(conc = conc, time = time,
        excpoints = excpoints, usepoints = usepoints,
        addt0 = FALSE, checkT0 = FALSE), silent = TRUE)

    if (is(timeconc, "try-error")) {
        stop(paste("call to cleanconctime failed, message is: ", timeconc))
    }

    # initialize output
    lamznpt <- as.numeric(NA)

    result <- as.numeric(rep(NA, 9))

    names(result) <- c("LAMZ", "intercept", "R2", "R2ADJ",
        "CORRXY", "LAMZHL", "LAMZLL", "LAMZUL", "lamznpt")

    # remove excluded values
    timeconc <- timeconc[timeconc$usepoints & !timeconc$excpoints, ]

    # if all rows are gone, there are insufficient rows for lambdaz calculation
    if (identical(nrow(timeconc), as.integer(0))) {
        return(list(lamznpt = lamznpt, result = result))
    }

    # test using usepoints, ignoring Cmax restrictions and excluding excpoints
    lamznpt <- as.numeric(nrow(timeconc))

    if (lamznpt < minpoints) {
        warning("There are fewer non-zero, non-excluded points in the data than minpoints")
        lamznpt <- as.numeric(NA)
    } else {
        result[1:9] <- unlist(lambdaZStatistics(conc = timeconc$conc,
            time = timeconc$time, lamznpt = lamznpt, checkcmax = FALSE))
    }
    return(list(lamznpt = lamznpt, result = result))
}
