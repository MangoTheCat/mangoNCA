
# check CDISCPP validity
# 
# @param object CDISCPP object
# @return TRUE or single character

#validCDISCPP <- function(object) {
#    TRUE
#}



#' @title An S4 class to represent CDISCPP data.
#'
#' @description A data frame-like object with 38 named columns.
#' 
#' @name CDISCPP-class
#' @rdname CDISCPP-class
#' @exportClass CDISCPP
#' @importFrom methods callNextMethod is show validObject
#' @aliases CDISCPP
#' @examples
#' x <- new("CDISCPP")
#' print(x)

setClass(Class = "CDISCPP",
    representation = representation(
        "R2ADJ" = "numeric", 
        "INTERCEPT" = "numeric",
        "LAMZNPT" = "numeric", 
        "R2" = "numeric", 
        "CORRXY" = "numeric", 
        "AUCPEO" = "numeric", 
        "AUCPEP" = "numeric", 
        "AUCIFO" = "numeric", 
        "AUCIFP" = "numeric", 
        "AUCLST" = "numeric", 
        "AUMCPEO" = "numeric", 
        "AUMCPEP" = "numeric",
        "AUMCIFO" = "numeric", 
        "AUMCIFP" = "numeric", 
        "AUMCLST" = "numeric", 
        "CLST" = "numeric", 
        "CLO" = "numeric", 
        "CLP" = "numeric", 
        "CMAX" = "numeric", 
        "CMIN" = "numeric", 
        "INTDOSE" = "numeric", 
        "DOSE" = "numeric", 
        "LAMZHL" = "numeric", 
        "LAMZ" = "numeric", 
        "LAMZLL" = "numeric", 
        "LAMZUL" = "numeric", 
        "MRTIFO" = "numeric", 
        "MRTIFP" = "numeric", 
        "MRTLST" = "numeric", 
        "CPEAK" = "numeric", 
        "TLST" = "numeric", 
        "TMAX" = "numeric", 
        "TMIN" = "numeric", 
        "CTROUGH" = "numeric", 
        "VSSO" = "numeric", 
        "VSSP" = "numeric", 
        "VZO" = "numeric", 
        "VZP" = "numeric",
        "ERROR" = "character"))
    #validity = validCDISCPP)



# @title Constructor method of CDISCPP
# @name initialize-CDISCPP-method
# @describeIn CDISCPP-class
# @description Create CDISCPP
# @param .Object Object from which to create new object.
# @param \dots Additional data.
# @examples
# z <- new("CDISCPP")
# slot(object = z, name = "CMAX")

setMethod(f = "initialize", 
    signature = "CDISCPP", 
    definition = function(.Object, ...) {
    .Object <- callNextMethod()
    validObject(.Object)      
    return(.Object)
})

setMethod(f = "[<-", 
    signature = "CDISCPP", 
    definition = function(x, i, ..., value) {
        slot(object = x, name = i, value  = value)
        return(x)
    })
