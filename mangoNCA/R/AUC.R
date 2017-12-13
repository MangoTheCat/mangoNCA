
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

#' Calculates the area under a time-concentration curve (AUC) using the linear trapezium rule.
#'
#' A vector of areas under individual trapezoids (not the total area) is returned.
#' These will be the individual AUC values at a given time/concentration index.
#' In other words, auc[i] is the area under the trapezium bounded by time[i], conc[i] and time[i+1], conc[i+1].
#' This function is called by \code{AUCLast}.
#' For a given time interval (t1:t2), the AUC can be calculated as follows:
#'     AUC_t1_t2 = (C1 + C2) * (t2 - t1) / 2
#' 
#' @title Calculate AUC Using Linear Trapezoidal Rules
#' @param conc numeric vector of concentrations
#' @param time numeric vector of time points (same length as and parallel to conc).  
#' These elements should be sorted in ascending order, otherwise an exception will be generated.
#' @note It is assumed that these times and concentrations come from a single curve.
#' The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{conc} or \code{time} are of length 0, a single NA is returned
#'      \item \code{conc} and \code{time} should be equal length numeric vectors, 
#'      otherwise an exception will be generated
#'      \item if \code{time[i]} or \code{conc[i]} is \code{NA} and \code{auc <-  AUCLin(conc,time)}, 
#'      then \code{auc[i]} and \code{auc[i - 1]} will be \code{NA}.
#'  }
#' @return numeric vector of trapezium AUC values.
#' @keywords math 
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' x <- mangoNCA:::AUCLin(Theoph2$conc, Theoph2$time) 
#' sum(x) # 91.5268


AUCLin <- function(conc, time) {
    # Note that AUC returns a vector of trapezia
    # NA time or conc values will prevent corresponding trapezia from being calculated
    # This is not a desirable estimate of AUC
    # Vectors containing NAs should not be passed to AUCLin 

    # check data
    
    checkNumericSameLength(time, conc, "time", "concentration")
    checkOrderedVector(time, "time", "AUCLin")
    
    # conctime: data.frame holding columns of times and concentrations
    
    conctime <- data.frame(time, conc)
    
    nn <- nrow(conctime) - 1
    
    # there must be at least 2 elements to calculate AUC  
    # if there are fewer, return a single NA
    
    auc <- as.numeric(NA)
    
    if(nn == 0) { auc <- 0 }
    
    if (nn > 0) {
        
        auc <- (conctime[seq_len(nn), "conc"] + conctime[seq_len(nn) + 1, "conc"])  * 
            diff(conctime[, "time"]) / 2
    }
    

    return(auc)
}



#' Simple routine to compute trapezium rule integral ("area under curve") estimate of a set of x and y coordinates.  
#' No error handling whatsoever is performed.  Meant as a testing utility routine.
#' 
#' @param x x-coordinates 
#' @param y y-coordinates
#' @title Calculate Area Using Trapezium Rule
#' @return Single numeric with trapezium rule applied to x and y coordinates
#' @author fgochez
#' @noRd

trapezium <- function(x, y)
{
    # xIndex : integer vector of indices
    xIndex <- 2:length(x)
    as.double(((x[xIndex] - x[xIndex - 1]) %*% (y[xIndex ] + y[xIndex -1])) / 2)
}



#' Calculates the area under a time-concentration curve (AUC) using the logarithmic trapezoidal rule.
#'
#' The logarithmic trapezoidal method uses logarithmic interpolation between data points to calculate the AUC. 
#' This method represents more closely decreasing concentrations since they are equivalent to first order  
#' elimination  (linear on a logarithmic scale). This function is called by \code{AUCLast}.
#'
#' A vector of areas under individual trapezoids (not the total area) is returned.
#' These will be the individual AUC values at a given time/concentration index.
#' In other words, auc[i] is the area under the trapezium bounded by time[i], conc[i] and time[i + 1], conc[i + 1]. 
#' For a given time interval (t1:t2), the AUC can be calculated as follows:
#'     AUC_t1_t2 = (t2 - t1) * (C2 - C1) / log(C2 / C1)
#' 
#' @title Calculate AUC Using Logarithmic Trapezoidal Rule
#' @param conc numeric vector of concentrations
#' @param time numeric vector of time points (same length as and parallel to conc).  
#' These elements should be sorted in ascending order, otherwise an exception will be generated.
#' @note It is assumed that these times and concentrations come from a single curve. 
#' The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{conc} or \code{time} are of length 1 or 0, a single NA is returned
#'      \item \code{conc} and \code{time} should be equal length numeric vectors, 
#'      otherwise an exception will be generated
#'      \item if \code{time[i]} or \code{conc[i]} is \code{NA} and \code{auc =  AUCLin(conc,time)}, 
#'      then \code{auc[i]} and \code{auc[i - 1]} will be \code{NA}.
#'      \item If \code{conc[i]} equals \code{conc[i + 1]}, linear interpolation will be used
#'  }
#' @return numeric vector of trapezium AUC values.  
#' @keywords math 
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' x <- mangoNCA:::AUCLog( Theoph2$conc, Theoph2$time ) 
#' sum(x) # 

AUCLog <- function (conc, time) {
    
    checkNumericSameLength(time, conc, "time", "concentration", 
        functionName = "AUCLog")
    
    checkOrderedVector(time, "time", functionName = "AUCLog")

    conctime <- data.frame(time, conc)
    
    nn <- nrow(conctime) - 1
    
    auc <- as.numeric(NA)
    
    if (nn == 0) { auc <- 0 }
    
    if (nn > 0) {
        
        deltaconc <- conctime[seq_len(nn), "conc"] - conctime[seq_len(nn) + 1, "conc"]
        
        deltalogconc <- log(conctime[seq_len(nn), "conc"]) - log(conctime[seq_len(nn) + 1, "conc"])
        
        auc <- diff(conctime[, "time"]) * deltaconc / deltalogconc
        
        zeros <- deltaconc == 0
        
        # handle missing values in data
        
        zeros[is.na(conctime[seq_len(nn), "conc"])] <- FALSE
        
        zeros[is.na(conctime[seq_len(nn) + 1, "conc"])] <- FALSE
        
        # use linear interpolation where C1 == C2 (i.e. squares)
        
        if (any(zeros)) {
            
            squares <- diff(conctime[, "time"]) * conctime[seq_len(nn), "conc"]
            
            auc[zeros] <- squares[zeros]
        }
    }
    
    return(auc)
}
