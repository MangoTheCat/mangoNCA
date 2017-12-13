# SVN revision: $Rev:  $
# Date of last change: $LastChangedDate: 02/07/2013 $
# Last changed by: $LastChangedBy: ccampbell $
# 
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
#' @param Conc numeric vector of concentrations
#' @param Time numeric vector of time points (same length as and parallel to Conc).  
#' These elements should be sorted in ascending order, otherwise an exception will be generated.
#' @note It is assumed that these times and concentrations come from a single curve.
#' The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{Conc} or \code{Time} are of length 0, a single NA is returned
#'      \item \code{Conc} and \code{Time} should be equal length numeric vectors, 
#'      otherwise an exception will be generated
#'      \item if \code{Time[i]} or \code{Conc[i]} is \code{NA} and \code{auc <-  AUCLin(Conc,Time)}, 
#'      then \code{auc[i]} and \code{auc[i - 1]} will be \code{NA}.
#'  }
#' @return numeric vector of trapezium AUC values.
#' @keywords math 
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' x <- MangoNca:::AUCLin(Theoph2$conc, Theoph2$Time) 
#' sum(x) # 91.5268


AUCLin <- function(Conc, Time)
{
    # Note that AUC returns a vector of trapezia
    # NA Time or Conc values will prevent corresponding trapezia from being calculated
    # This is not a desirable estimate of AUC
    # Vectors containing NAs should not be passed to AUCLin 

    # check data
    
    checkNumericSameLength(Time, Conc, "Time", "Concentration")
    checkOrderedVector(Time, "Time", "AUCLin")
    
    # concTime: data.frame holding columns of times and concentrations
    
    concTime <- data.frame(Time, Conc)
    
    nn <- nrow(concTime) - 1
    
    # there must be at least 2 elements to calculate AUC  
    # if there are fewer, return a single NA
    
    auc <- as.numeric(NA)
    
    if(nn == 0) { auc <- 0 }
    
    if (nn > 0) {
        
        auc <- (concTime[seq_len(nn), "Conc"] + concTime[seq_len(nn) + 1, "Conc"])  * 
            diff(concTime[, "Time"]) / 2
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
#' @param Conc numeric vector of concentrations
#' @param Time numeric vector of time points (same length as and parallel to Conc).  
#' These elements should be sorted in ascending order, otherwise an exception will be generated.
#' @note It is assumed that these times and concentrations come from a single curve. 
#' The following error checks / processing will be performed:
#'  \enumerate{
#'      \item If \code{Conc} or \code{Time} are of length 1 or 0, a single NA is returned
#'      \item \code{Conc} and \code{Time} should be equal length numeric vectors, 
#'      otherwise an exception will be generated
#'      \item if \code{Time[i]} or \code{Conc[i]} is \code{NA} and \code{auc =  AUCLin(Conc,Time)}, 
#'      then \code{auc[i]} and \code{auc[i - 1]} will be \code{NA}.
#'      \item If \code{Conc[i]} equals \code{Conc[i + 1]}, linear interpolation will be used
#'  }
#' @return numeric vector of trapezium AUC values.  
#' @keywords math 
#' @examples
#' Theoph2 <- subset(Theoph, Subject == 2)
#' x <- MangoNca:::AUCLog( Theoph2$conc, Theoph2$Time ) 
#' sum(x) # 

AUCLog <- function (Conc, Time) {
    
    checkNumericSameLength(Time, Conc, "Time", "Concentration", functionName = "AUCLog")
    
    checkOrderedVector(Time, "Time", functionName = "AUCLog")

    concTime <- data.frame(Time, Conc)
    
    nn <- nrow(concTime) - 1
    
    auc <- as.numeric(NA)
    
    if (nn == 0) { auc <- 0 }
    
    if (nn > 0) {
        
        deltaConc <- concTime[seq_len(nn), "Conc"] - concTime[seq_len(nn) + 1, "Conc"]
        
        deltalogConc <- log(concTime[seq_len(nn), "Conc"]) - log(concTime[seq_len(nn) + 1, "Conc"])
        
        auc <- diff(concTime[, "Time"]) * deltaConc / deltalogConc
        
        zeros <- deltaConc == 0
        
        # handle missing values in data
        
        zeros[is.na(concTime[seq_len(nn), "Conc"])] <- FALSE
        
        zeros[is.na(concTime[seq_len(nn) + 1, "Conc"])] <- FALSE
        
        # use linear interpolation where C1 == C2 (i.e. squares)
        
        if (any(zeros)) {
            
            squares <- diff(concTime[, "Time"]) * concTime[seq_len(nn), "Conc"]
            
            auc[zeros] <- squares[zeros]
        }
    }
    
    return(auc)
}
