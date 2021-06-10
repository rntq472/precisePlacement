##' Add Lines to a Plot to Indicate Where the Margins Are
##' 
##' @param sides Numeric vector giving the sides of the plot to annotate. Defaults
##'              to 1:4.
##' @param lty Argument to be passed to graphics::abline. Defaults to 2.
##' @param ... Other arguments to be passed to graphics::abline
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' plot(1:10)
##' par(xpd = NA)
##' showMarginLines()
##' 
##' }
##' 
##' @export
##' 
##
showMarginLines <- function(sides = 1:4, lty = 2, ...){
    
    stopifnot(sides %in% 1:4, length(sides) > 0)
    
    mar <- par('mar')
    marFloor <- floor(mar)
    boundaries <- getBoundaries('plot')
    dataPerLine <- getDataPerLine()
    
    if (1 %in% sides && marFloor[1] > 0){
        
        x <- c(boundaries[2] - max(marFloor[2]) * dataPerLine[1],
               boundaries[4] + max(marFloor[4]) * dataPerLine[1]
               )
        y <- boundaries[1] - (0:marFloor[1]) * dataPerLine[2]
        
        for (ii in y)
            lines(x, rep(ii, 2), lty = lty, ...)
        
    }
    
    if (3 %in% sides && marFloor[3] > 0){
        
        x <- c(boundaries[2] - max(marFloor[2]) * dataPerLine[1],
               boundaries[4] + max(marFloor[4]) * dataPerLine[1]
               )
        y <- boundaries[3] + (0:marFloor[3]) * dataPerLine[2]
        
        for (ii in y)
            lines(x, rep(ii, 2), lty = lty, ...)
        
    }
    
    if (2 %in% sides && marFloor[2] > 0){
        
        y <- c(boundaries[1] - max(marFloor[1]) * dataPerLine[2],
               boundaries[3] + max(marFloor[3]) * dataPerLine[2]
               )
        x <- boundaries[2] - (0:marFloor[2]) * dataPerLine[1]
        
        for (ii in x)
            lines(rep(ii, 2), y, lty = lty, ...)
        
    }
    
    if (4 %in% sides && marFloor[4] > 0){
        
        y <- c(boundaries[1] - max(marFloor[1]) * dataPerLine[2],
               boundaries[3] + max(marFloor[3]) * dataPerLine[2]
               )
        x <- boundaries[4] + (0:marFloor[4]) * dataPerLine[1]
        
        for (ii in x)
            lines(rep(ii, 2), y, lty = lty, ...)
        
    }
    
}
