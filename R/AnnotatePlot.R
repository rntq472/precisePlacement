##' Highlight the Data Region of a Plot
##'
##' @param border Parameter to be passed to graphics::rect. Defaults to "green".
##' @param col Parameter to be passed to graphics::rect. Defaults to
##' adjustcolor(border, 0.1).
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' highlightDataRegion()
##'
##' }
##'
##' @export
##'
##
highlightDataRegion <- function(border = "green", col = adjustcolor(border, 0.1)) {

    dataBoundaries <- getBoundaries("data")

    rect(xleft = dataBoundaries[2],
         ybottom = dataBoundaries[1],
         xright = dataBoundaries[4],
         ytop = dataBoundaries[3],
         col = col, border = border)

}

##' Highlight the Plotting Region of a Plot
##'
##' @param border Parameter to be passed to graphics::rect. Defaults to "green".
##' @param col Parameter to be passed to graphics::rect. Defaults to
##' adjustcolor(border, 0.1).
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' highlightPlotRegion()
##'
##' }
##'
##' @export
##'
##
highlightPlotRegion <- function(border = "red", col = adjustcolor(border, 0.1)) {

    plotBoundaries <- getBoundaries("plot")

    rect(xleft = plotBoundaries[2],
         ybottom = plotBoundaries[1],
         xright = plotBoundaries[4],
         ytop = plotBoundaries[3],
         col = col, border = border)
}

##' Highlight the Figure Region of a Plot
##'
##' @param border Parameter to be passed to graphics::rect. Defaults to "green".
##' @param col Parameter to be passed to graphics::rect. Defaults to
##' adjustcolor(border, 0.1).
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' par(xpd = NA)
##' highlightFigureRegion()
##'
##' }
##'
##' @export
##'
##
highlightFigureRegion <- function(border = "orange", col = adjustcolor(border, 0.1)) {

    figureBoundaries <- getBoundaries("figure")

    rect(xleft = figureBoundaries[2],
         ybottom = figureBoundaries[1],
         xright = figureBoundaries[4],
         ytop = figureBoundaries[3],
         col = col, border = border)
}

##' Highlight the Device Region of a Plot
##'
##' @param border Parameter to be passed to graphics::rect. Defaults to "green".
##' @param col Parameter to be passed to graphics::rect. Defaults to
##' adjustcolor(border, 0.1).
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' par(xpd = NA)
##' highlightDeviceRegion()
##'
##' }
##'
##' @export
##'
##
highlightDeviceRegion <- function(border = "skyblue", col = adjustcolor(border, 0.1)) {

    deviceBoundaries <- getBoundaries("device")

    rect(xleft = deviceBoundaries[2],
         ybottom = deviceBoundaries[1],
         xright = deviceBoundaries[4],
         ytop = deviceBoundaries[3],
         col = col, border = border)
}
