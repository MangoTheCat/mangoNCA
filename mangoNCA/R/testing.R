
# Original author: fgochez

# miscellaneous testing routines

#' Check if an object is of class "try-error", and that it contains a specific error message.
#' @param x Object to check
#' @param expectedMessage String with expected message
#' @title Check if object is error with given error message
#' @return TRUE or FALSE depending on whether or not x is as expected
#' @author Mango Solutions

isErrorWithMessage <- function(x, expectedMessage) {
    is(x, "try-error") & as.character(x) == expectedMessage
}

