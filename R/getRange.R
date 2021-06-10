##' Determine the Width and Height of a Plot
##' 
##' @inheritParams getBoundaries
##' @param units Character string giving the units in which to define the range.
##'              Must be one of "in", "px", "data", or "lines".
##' 
##' @return Numeric vector of length two giving the range of the plotting
##'         region, in the order of x-axis, y-axis.
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' plot(1:10)
##' print(getRange('data', 'in'))
##' print(getRange('plot', 'px'))
##' print(getRange('figure', 'data'))
##' print(getRange('device', 'lines'))
##' 
##' }
##' 
##' @export
##' 
##
getRange <- function(region, units){

    stopifnot(!is.null(region), !is.null(units))
    
    region <- match.arg(region, choices = c('data', 'plot', 'figure', 'device'))
    units <- match.arg(units, choices = c('in', 'px', 'data', 'lines'))
    
    if (region == 'data'){
        
        if (units == 'data'){
            
            dataBoundaries <- getBoundaries('data')
            
            c(diff(dataBoundaries[c(2, 4)]), diff(dataBoundaries[c(1, 3)]))
            
        } else if (units == 'in'){
            
            dataFraction <- getRange('data', 'data') / getRange('device', 'data')
            
            dataFraction * getRange('device', 'in')
            
        } else if (units == 'px'){
            
            dataFraction <- getRange('data', 'data') / getRange('device', 'data')
            
            dataFraction * getRange('device', 'px')
            
        } else if (units == 'lines'){
            
            getRange('data', 'in') * getLinesPerInch()
            
        }
        
    } else if (region == 'plot'){
        
        if (units == 'data'){
            
            plotBoundaries <- getBoundaries('plot')
            
            c(diff(plotBoundaries[c(2, 4)]), diff(plotBoundaries[c(1, 3)]))
            
        } else if (units == 'in'){
            
            par('pin')
            
        } else if (units == 'px'){
            
            plotFraction <- getRange('plot', 'data') / getRange('device', 'data')
            
            plotFraction * getRange('device', 'px')
            
        } else if (units == 'lines'){
            
            getRange('plot', 'in') * getLinesPerInch()
            
        }
        
    } else if (region == 'figure'){
        
        if (units == 'data'){
            
            figureBoundaries <- getBoundaries('figure')
            
            c(diff(figureBoundaries[c(2, 4)]), diff(figureBoundaries[c(1, 3)]))
            
        } else if (units == 'in'){
            
            ##figureRange <- par('pin')
            
            ##figureRange[1] <- figureRange[1] + sum(par('mai')[c(2, 4)])
            ##figureRange[2] <- figureRange[2] + sum(par('mai')[c(1, 3)])
            
            ##figureRange
            
            ## Equivalent to:
            par('fin')
            
        } else if (units == 'px'){
            
            figureFraction <- getRange('figure', 'data') / getRange('device', 'data')
            
            figureFraction * getRange('device', 'px')
            
        } else if (units == 'lines'){
            
            getRange('figure', 'in') * getLinesPerInch()
            
        }
        
    } else if (region == 'device'){
        
        if (units == 'data'){
            
            deviceBoundaries <- getBoundaries('device')
            
            c(diff(deviceBoundaries[c(2, 4)]), diff(deviceBoundaries[c(1, 3)]))
            
        } else if (units == 'lines'){
            
            getRange('device', 'in') * getLinesPerInch()
            
        } else {
            
            dev.size(units)
            
        }
        
    }
}
