##' Add Lines to a Plot to Indicate Where the Outer Margins Are
##' 
##' @param sides Numeric vector giving the sides of the plot to annotate. Defaults
##'              to 1:4.
##' @param lty Argument to be passed to graphics::abline. Defaults to 3.
##' @param col Argument to be passed to graphics::abline. Defaults to "purple".
##' @param ... Other arguments to be passed to graphics::abline
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' par(oma = 1:4, mfrow = 2:1)
##' plot(1:10)
##' plot(1:10)
##' par(xpd = NA)
##' showOuterMarginLines()
##' 
##' }
##' 
##' @export
##' 
##
showOuterMarginLines <- function(sides = 1:4, lty = 3, col = 'purple', ...){
    
    stopifnot(sides %in% 1:4, length(sides) > 0)
    
    oma <- par('oma')
    omaFloor <- floor(oma)
    omaRemainder <- oma - omaFloor
    boundaries <- getBoundaries('device')
    dataPerLine <- getDataPerLine()
    
    if (1 %in% sides && omaFloor[1] > 0){
        
        abline(h = boundaries[1] + (0:omaFloor[1] + omaRemainder[1]) * dataPerLine[2],
               lty = lty, col = col, ...)
        
    }
    
    if (3 %in% sides && omaFloor[3] > 0){
        
        abline(h = boundaries[3] - (0:omaFloor[3] + omaRemainder[3]) * dataPerLine[2],
               lty = lty, col = col, ...)
        
    }
    
    if (2 %in% sides && omaFloor[2] > 0){
        
        abline(v = boundaries[2] + (0:omaFloor[2] + omaRemainder[2]) * dataPerLine[1],
               lty = lty, col = col, ...)
        
    }
    
    if (4 %in% sides && omaFloor[4] > 0){
        
        abline(v = boundaries[4] - (0:omaFloor[4] + omaRemainder[4]) * dataPerLine[1],
               lty =lty, col = col, ...)
        
    }
    
}
