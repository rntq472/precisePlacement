##' Wrapper Function for convertUnits Focused on Identifying the Locations of
##' the Margin Lines of a Plot
##'
##' @param side Integer giving the side of the plot to count lines from.
##' @param line Numeric vector giving margin lines one wishes to find the data
##' coordinates of.
##'
##' @note No attempt is made to limit the returned values to the device region.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' ## Illustrate where the lines fall when using mtext:
##' plot(1:10)
##' mtext(1:26, line = -(1:26), side = 1, col = 'blue')
##' x <- lineLocations(1, 0:(-26))
##' abline(h = x, col = 'red', lty = 2)
##' }
##'
##' @export
##'
##
lineLocations <- function(side, line) {

    stopifnot(side %in% 1:4, length(side) == 1, length(line) > 0)

    convertUnits("line", line, "data", side = side)

}
