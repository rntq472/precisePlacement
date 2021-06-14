##' Generate Values for par("omi") That Will Place a New Plot in a Sub-Region of
##' an Existing One
##' 
##' @param bottom Boundary value for the bottom edge.
##' @param left Boundary value for the left edge.
##' @param top Boundary value for the top edge.
##' @param right Boundary value for the right edge.
##' @param units The units in which the boundary parameters are defined. Must be one
##'              one of "proportion" or "data". Defaults to "proportion".
##' @param region The region of the plot to use for defining the boundaries. Must
##'               be one of "device", "figure", "plot", or "data". Only necessary
##'               when using units of "proportion".
##' 
##' @details The choice of accepting proportions instead of data units by default
##'          is to more easily handle empty devices, otherwise there is a risk of
##'          getting confused by the fact that par("usr") defaults to c(0, 1, 0, 1).
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' plot(1:10, pch = 19, col = 'black')
##' oldPar = par()
##' par(omi = omiForSubFigure(0.6, 0.25, 0.8, 0.45, region = 'device'))
##' par(mar = c(0,0,0,0))
##' plot(1:10, pch = 19, col = 'red')
##' par(oldPar)
##' par(omi = omiForSubFigure(2, 6, 5, 10, units = 'data'))
##' par(mar = c(0,0,0,0))
##' plot(1:10, pch = 19, col = 'blue')
##' par(oldPar)
##' 
##' ## Illustrates how the proportions line up:
##' 
##' plot(1:10, pch = 19)
##' par(xpd = NA)
##' oldPar = par()
##' 
##' ## Show where the propotions are as a reference:
##' abline(v = convertUnits('proportion', seq(0, 1, by = 0.1), 'data',
##'        region = 'device', axis = 'x'), lty = 2, col = 'red')
##' abline(h = convertUnits('proportion', seq(0, 1, by = 0.1), 'data',
##'        region = 'device', axis = 'y'), lty = 2, col = 'red')
##' 
##' ## Create a new sub-plot.
##' par(omi = omiForSubFigure(0.2, 0.2, 0.8, 0.8, region = 'device'))
##' 
##' plot(1:10, pch = 19, col = 'red')
##' highlightFigureRegion()
##' 
##' par(oldPar)
##' 
##' }
##' 
##' @export
##' 
##
omiForSubFigure <- function(bottom, left, top, right, units = 'proportion',
                            region = 'device'){
    
    if (is.null(dev.list()))
        stop('No open graphics devices')
    
    units <- match.arg(units, choices = c('proportion', 'data'))
    
    deviceRange <- getRange(region, 'in')
    
    stopifnot(bottom < top, left < right)
    
    if (units == 'proportion'){
        
        bottomProp <- bottom
        leftProp <- left
        topProp <- top
        rightProp <- right
        
    } else {
        
        bottomProp <- convertUnits('data', bottom, 'proportion', axis = 'y',
                                   region = region)
        leftProp <- convertUnits('data', left, 'proportion', axis = 'x',
                                 region = region)
        topProp <- convertUnits('data', top, 'proportion', axis = 'y',
                                region = region)
        rightProp <- convertUnits('data', right, 'proportion', axis = 'x',
                                  region = region)
        
    }
    
    omi <- c(bottomProp * deviceRange[2],
             leftProp * deviceRange[1],
             (1 - topProp) * deviceRange[2],
             (1 - rightProp) * deviceRange[1]
             )
    
    omi
    
}
