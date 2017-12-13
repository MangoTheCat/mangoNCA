#
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' @title Calculate lambdaz and related parameters
#'
#' Calculates the lambdaz estimate and related parameters.
#' The \code{lm} function is used to fit a linear regression of the last \code{lamznpt}
#' concentration against time.
#' These are counted from (and include) the last measurable concentration.
#' The number of points does not necessarily correspond to the index of the input dataset since rows may be removed at a previous step.
#' Components of the \code{"lm"} object are then used to compute lambdaz and related statistics.
#' Returns a list of single numeric values:
#' \enumerate{
#'      \item \code{LAMZ}: The negation of the slope of the regression line of the last
#'      \item \code{intercept}: The exponential of the intercept term of the regression
#'      \item \code{R2}: The R^2 of the regression fit
#'      \item \code{R2ADJ}: The adjusted R^2 of the regression fit
#'      \item \code{CORRXY}: The correlation coefficient of the observed fit.
#'      \item \code{LAMZHL}: Terminal phase half-life, equal to ln(2) / Lambdaz
#'      \item \code{LAMZLL}: Lower bound of time used in lambda z calculation
#'      \item \code{LAMZUL}: Upper bound of time used in lambda z calculation
#'      \item \code{lamznpt}: Number of points used in calculation
#' }
#' Note that the following additional error checking / additional
#' processing will be performed on the inputs:
#' \itemize{
#'     \item Missing elements (NA) of the time and concentration vectors will be removed during regression
#'         (a missing entry in one vector will cause the corresponding value in the other to be removed as well)
#'     \item If the sum of the (last \code{lamznpt}) concentrations is 0, missing values will be returned for all elements of the returned list.
#'     \item \code{conc} and \code{time} must be numeric vectors of equal length, or an exception will be generated
#'     \item \code{conc} has fewer than 2 elements, NA will be returned for all values
#'     \item If the linear regression fails for some reason, a warning will be emitted and NA will be returned for all values
#'     \item An exception will be generated if lamznpt is not a single integer numeric between 3 and  \code{length(conc)} (inclusive)
#' }
#' This function is called from \code{selectPoints} that removes rows containing Cmax before passing them to \code{lambdaZStatistics}.
#' Therefore there is the option to supress Cmax check by setting the argument checkcmax to \code{FALSE}
#'
#' @inheritParams getNCAnalysis
#' @param minpoints Single numeric declaring minimum acceptable number of rows for linear model. Default is 3, minimum accepted value is 2.
#' @param checkcmax Single logical value declaring whether Cmax is included in the data (default is TRUE).
#' @param doaic single logical should RSS and AIC be returned in addition to other stats (default is FALSE).
#' @return A list with a set of length 1 numeric vectors.
#' @export
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' lambdaZStatistics(conc = Theoph2$conc, time = Theoph2$Time)

lambdaZStatistics <- function(conc, time, lamznpt = 3,
    checkcmax = TRUE, minpoints = 3L, doaic = FALSE) {

    # data checks
    checkNumericSameLength(time, conc, "time", "concentration",
        "lambdaZStatistics")

    checkSingleNumeric(lamznpt, description = "lamznpt",
        "lambdaZStatistics")

    checkSingleLogical(checkcmax, description = "checkcmax",
        "lambdaZStatistics")

    checkSingleNumeric(minpoints, description = "minpoints",
        "lambdaZStatistics")

    checkSingleLogical(checkcmax, description = "doaic",
        "lambdaZStatistics")

    if (minpoints < 2) {
        stop("minpoints must be at least 2 in lambdaZStatistics")
    }
    # Initialize list of returned parameters
    # result : named list of parameters to return.
    # Each is initialized to NA, and is a length-1 numeric vector
    # see the comment under @return in the header for details

    result <- list(LAMZ = NA_real_, intercept = NA_real_,
        R2 = NA_real_, R2ADJ = NA_real_,
        CORRXY = NA_real_, LAMZHL = NA_real_,
        LAMZLL = NA_real_, LAMZUL = NA_real_,
        lamznpt = NA_real_)

    # is lamznpt is NA or less than minpoints,
    # all lambdaz statistics are NA
    if (is.na(lamznpt)) { lamznpt <- 0 }
    if (lamznpt < minpoints) {
        return(result)
    }
    if (sum(conc, na.rm = TRUE) <= 0 ) {
        warning("Warning in lambdazStatistics: sum of conc is <= 0")
        return(result)
    }
    timeconc <- stripTrailingZeros(conc = conc, time = time, checkT0 = FALSE)

    # check that there are at least 3 rows after Cmax,
    # otherwise return list of NAs
    if (!testTrailPoints(conc = timeconc$conc, time = timeconc$time,
        minpoints = minpoints, checkcmax = checkcmax)) {
        return(result)
    }
    # check that lamznpt is valid, return NA if it is not
    if (!(lamznpt <= nrow(timeconc) &&
            identical(floor(lamznpt), lamznpt))) {
        warning(paste(
            "Warning in lambdazStatistics: invalid value of lamznpt:",
            lamznpt, collapse = " "))
        return(result)
    }

    # use only lamznpt rows
    timeconc <- tail(timeconc, n = lamznpt)

    # take log and remove -Inf values
    timeconc$logconc <- log(timeconc$conc)

    timeconc[timeconc$logconc == -Inf, "logconc"] <- NA

    # lmFit : object of class lm holding regression results
    # (or try-error if something goes wrong)
    lmFit <- try(getLambdaz(timeconc), silent = TRUE)

    if (is(lmFit, "try-error")) {
        warning("Call to lm function failed, so regression could not be calculated successfully")
        return(result)
    }
    # lmFitSummary : summary.lm object holding summary of lmFit
    lmFitSummary <- summary(lmFit, correlation = TRUE)

    # Correlation Coefficient calc
    corrXY <- cor(timeconc[, c("logconc", "time")],
        use = "complete.obs")[2, 1]

    # populate output
    result$LAMZ <- unname(coef(lmFit)["time"]) * -1.0

    result$intercept <- exp(unname(coef(lmFit)["(Intercept)"]))

    result$R2 <- lmFitSummary$r.squared

    result$R2ADJ <- lmFitSummary$adj.r.squared

    result$CORRXY <- corrXY

    result$LAMZHL <- log(2) / unname(result$LAMZ)

    # NOTE : here we are using the data from the regression fit object, which has omitted missing values
    # from the original data set. This seems like the correct approach to me, though legacy code used
    # the original data.frame timeconc for extracting these data points
    # TODO Compare with WNL

    result$LAMZLL <- head(lmFit$model$time, 1)

    result$LAMZUL <- tail(lmFit$model$time, 1)

    # TODO update lambdaZStatistics to allow calculation of AIC
    # rss is lmFitSummary$sigma
    # perhaps return AIC

    # The number of points should INCLUDE omitted points
    # so that index corresponds to the input dataset

    numberRows <- nrow(lmFit$model)

    if (!setequal(numberRows, lamznpt)) {
        warning(paste("Number of rows used for lambdaz calculation (", numberRows,
            ") does not equal lamznpt (", lamznpt, ")", collapse = ""))
    }

    result$lamznpt <- lamznpt

    if (doaic) {
        result$rss <- lmFitSummary$sigma

        result$aic <- AIC(lmFit)
    }
    return(result)
}



#' @title Perform \code{lm} on logconc-time data
#'
#' Calculates lambdaz for a data frame consisting of logconc and time.
#' The \code{lm} function is used to fit a linear regression of the last \code{lamznpt}
#' concentration against time.  These are counted from (and include)
#' the last measurable concentration.
#'
#' @param timeconc Data frame with columns logconc and time used for calculating lambdaz.
#' @return \code{"lm"} object to be used by \code{getLambdaZStatistics}.

getLambdaz <- function(timeconc) {

    if (!all(is.element(c("time", "logconc"), names(timeconc)))) {
        stop("Error in getLambdaz: data frame timeconc passed from lambdaZStatistics does not have columns logconc and time")
    }

    lmFit <- lm(logconc ~ time, na.action = na.exclude, data = timeconc)

    return(lmFit)
}
