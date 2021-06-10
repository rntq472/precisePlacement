##' Convert Between Different Available Units That Measure Points on a Plot
##' 
##' @param from Units one wishes to convert from. Allowed values are "line", "data",
##'             and "proportion".
##' @param value Numeric value(s) of the coordinate(s) one wishes to convert.
##' @param to Units one wishes to convert from. Allowed values are "line", "data",
##'           and "proportion".
##' @param side Integer giving the side of the plot to count lines from.
##' @param axis One of "x" or "y", giving the axis a proportion should be calculated
##'             from.
##' @param region Required when either from or to has the value "proportion". Must
##'               be one of "device", "figure", "plot", or "data". Defaults to "plot".
##' 
##' @return Numeric value(s) of the input coordinates converted into the new units.
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' ## Illustrate where the lines fall when using mtext:
##' 
##' plot(1:10)
##' mtext(1:26, line = -(1:26), side = 1, col = 'blue')
##' x <- convertUnits('line', 0:(-26), 'data', side = 1)
##' abline(h = x, col = 'red', lty = 2)
##'
##' ## Show how proportions of a plot can be identified:
##' 
##' plot(seq(as.Date('2018-01-01'), as.Date('2019-01-01'), length.out = 10), 1:10,
##'      pch = 19)
##' 
##' ## Identify the "center" of the plot.
##' abline(h = convertUnits('proportion', 0.5, 'data', axis = 'y'),
##'        col = 'red', lwd = 4)
##' abline(v = convertUnits('proportion', 0.5, 'data', axis = 'x'),
##'        col = 'blue', lwd = 4)
##' 
##' print(convertUnits('proportion', 0.5, 'data', axis = 'y'))
##' ## as.Date is needed because convertUnits returns a numeric value.
##' print(as.Date(convertUnits('proportion', 0.5, 'data', axis = 'x'),
##'       origin = '1970-01-01'))
##' 
##' ## Change the region we are defining the proportions from.
##' abline(v = convertUnits('proportion', 0.75, 'data', axis = 'x', region = 'plot'),
##'        col = 'darkgreen', lwd = 4)
##' abline(v = convertUnits('proportion', 0.75, 'data', axis = 'x', region = 'device'),
##'        col = 'orange', lwd = 4)
##' 
##' }
##' 
##' @export
##' 
##
convertUnits <- function(from, value, to, side = NULL, axis = NULL, region = 'plot'){
    
    stopifnot(!is.null(from), !is.null(to), !is.null(value))
    
    from <- match.arg(from, choices = c('line', 'data', 'proportion'))
    to <- match.arg(to, choices = c('line', 'data', 'proportion'))
    
    if (inherits(value, c('Date', 'POSIXt')))
        value <- as.numeric(value)
    
    if (from == to)
        return(value)
    
    ## Put the checks here before a plot is created if one doesn't already exist.
    
    if (any(c(from, to) %in% 'proportion'))
        stopifnot(!is.null(region), length(region) == 1,
                  region %in% c('device', 'figure', 'plot', 'data'))

    if (from == 'line')
        stopifnot(!is.null(side), length(side) == 1, side %in% 1:4)

    if (from == 'data' & to == 'line')
        stopifnot(!is.null(side), length(side) == 1, side %in% 1:4)

    if (from == 'data' & to == 'proportion')
        stopifnot(!is.null(axis), length(axis) == 1, axis %in% c('x', 'y'))
    
    if (from == 'proportion')
        stopifnot(value >= 0, value <= 1)
    
    if (from == 'proportion' & to == 'data')
        stopifnot(!is.null(axis), length(axis) == 1, axis %in% c('x', 'y'))
    
    if (from == 'proportion' & to == 'line')
        stopifnot(!is.null(side), length(side) == 1, side %in% 1:4)

    plotBoundaries <- getBoundaries('plot')
    dataPerLine <- getDataPerLine()
    linesPerDatum <- getLinesPerDatum()
    
    if (from == 'line'){
        
        if (to == 'data'){
            
            if (side == 1){
                
                plotBoundaries[1] - value * dataPerLine[2]
                
            } else if (side == 2){
                
                plotBoundaries[2] - value * dataPerLine[1]
                
            } else if (side == 3){
                
                plotBoundaries[3] + value * dataPerLine[2]
                
            } else if (side == 4){
                
                plotBoundaries[4] + value * dataPerLine[1]
                
            }
            
        } else if (to == 'proportion'){
            
            boundaries <- getBoundaries(region)
            
            if (side == 1){
                
                y <- plotBoundaries[1] - value * dataPerLine[2]
                
                yProp <- (y - boundaries[1]) / diff(boundaries[c(1, 3)])
                
                yProp
                
            } else if (side == 2){
                
                x <- plotBoundaries[2] - value * dataPerLine[1]
                
                xProp <- (x - boundaries[2]) / diff(boundaries[c(2, 4)])
                
                xProp
                
            } else if (side == 3){
                
                y <- plotBoundaries[3] + value * dataPerLine[2]
                
                yProp <- (y - boundaries[1]) / diff(boundaries[c(1, 3)])
                
                yProp
                
            } else if (side == 4){
                
                x <- plotBoundaries[4] + value * dataPerLine[1]
                
                xProp <- (x - boundaries[2]) / diff(boundaries[c(2, 4)])
                
                xProp
                
            }
            
        }
        
    } else if (from == 'data'){
        
        if (to == 'line'){
            
            if (side == 1){
                
                0 - (value - plotBoundaries[1]) * linesPerDatum[2]
                
            } else if (side == 2){
                
                0 - (value - plotBoundaries[2]) * linesPerDatum[1]
                
            } else if (side == 3){
                
                0 + (value - plotBoundaries[3]) * linesPerDatum[2]
                
            } else if (side == 4){
                
                0 + (value - plotBoundaries[4]) * linesPerDatum[1]
                
            }
            
        } else if (to == 'proportion'){
            
            if (axis == 'x'){
                
                boundaries <- getBoundaries(region)
                
                xProp <- (value - boundaries[2]) / diff(boundaries[c(2, 4)])
                
                xProp
                
            } else if (axis == 'y'){
                
                boundaries <- getBoundaries(region)
                
                yProp <- (value - boundaries[1]) / diff(boundaries[c(1, 3)])
                
                yProp
                
            }
            
        }
        
    } else if (from == 'proportion'){
        
        if (to == 'data'){
            
            if (axis == 'x'){
                
                boundaries <- getBoundaries(region)
                
                x <- boundaries[2] + value * diff(boundaries[c(2, 4)])
                
                x
                
            } else if (axis == 'y'){
                
                boundaries <- getBoundaries(region)
                
                y <- boundaries[1] + value * diff(boundaries[c(1, 3)])
                
                y
                
            }
            
        } else if (to == 'line'){
            
            if (side %in% c(1, 3) ){
                axis <- 'y'
            } else if (side %in% c(2, 4) ){
                axis <- 'x'
            }
            
            if (axis == 'x'){
                
                boundaries <- getBoundaries(region)
                
                x <- boundaries[2] + value * diff(boundaries[c(2, 4)])
                
                if (side == 1){
                    
                    plotBoundaries[1] - x * linesPerDatum[2]
                    
                } else if (side == 2){
                    
                    plotBoundaries[2] - x * linesPerDatum[1]
                    
                } else if (side == 3){
                    
                    plotBoundaries[3] + x * linesPerDatum[2]
                    
                } else if (side == 4){
                    
                    plotBoundaries[4] + x * linesPerDatum[1]
                    
                }
                
            } else if (axis == 'y'){
                
                boundaries <- getBoundaries(region)
                
                y <- boundaries[1] + value * diff(boundaries[c(1, 3)])
                
                if (side == 1){
                    
                    plotBoundaries[1] - y * linesPerDatum[2]
                    
                } else if (side == 2){
                    
                    plotBoundaries[2] - y * linesPerDatum[1]
                    
                } else if (side == 3){
                    
                    plotBoundaries[3] + y * linesPerDatum[2]
                    
                } else if (side == 4){
                    
                    plotBoundaries[4] + y * linesPerDatum[1]
                    
                }
            }
        }
    }
}
