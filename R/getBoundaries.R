##' Determine the Boundaries of a Plot in Terms of the Data Units
##'
##' @param region The region of the plot to use for defining the boundaries. Must
##' be one of "device", "figure", "plot", or "data".
##' @param units Character string giving the units in which to define the range.
##' Must be either "data" or "lines".
##' @param sides Numeric vector giving the four sides to uses as a reference if
##' the requested units are "lines". Defaults to 1:4.
##'
##' @return A numeric vector of length four giving the coordinates of the plotting
##' boundary, in the order of bottom, left, top, right.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' par(oma = 1:4)
##' plot(1:10)
##' print(getBoundaries('data'))
##' print(getBoundaries('plot'))
##' print(getBoundaries('figure'))
##' print(getBoundaries('device'))
##'
##' print(getBoundaries('data', units = 'lines'))
##' print(getBoundaries('plot', units = 'lines'))
##' print(getBoundaries('figure', units = 'lines'))
##' print(getBoundaries('device', units = 'lines'))
##'
##' }
##'
##' @export
##'
##
getBoundaries <- function(region, units = "data", sides = 1:4) {

    stopifnot(!is.null(region), !is.null(units))

    if (units == "lines")
        stopifnot(!is.null(sides), length(sides) == 4, is.numeric(sides),
                  !is.na(sides))

    region <- match.arg(region, choices = c("data", "plot", "figure", "device"))
    units <- match.arg(units, choices = c("data", "lines"))

    if (region == "data") {

        plotBoundaries <- par("usr") ## x1, x2, y1, y2

        dataBoundaries <- plotBoundaries

        ## Documentation says 0.04 but this is apparently rounded from 0.037037037.
        if (par("xaxs") == "r") {
            dataBoundaries[1] <- plotBoundaries[1] +
                (diff(plotBoundaries[1:2]) * 0.037037037)
            dataBoundaries[2] <- plotBoundaries[2] -
                (diff(plotBoundaries[1:2]) * 0.037037037)
        }
        if (par("yaxs") == "r") {
            dataBoundaries[3] <- plotBoundaries[3] +
                (diff(plotBoundaries[3:4]) * 0.037037037)
            dataBoundaries[4] <- plotBoundaries[4] -
                (diff(plotBoundaries[3:4]) * 0.037037037)
        }

        out <- dataBoundaries[c(3, 1, 4, 2)] ## bottom, left, top, right

    } else if (region == "plot") {

        plotBoundaries <- par("usr") ## x1, x2, y1, y2

        out <- plotBoundaries[c(3, 1, 4, 2)] ## bottom, left, top, right

    } else if (region == "figure") {

        dataPerInch <- getDataPerInch()

        figureBoundaries <- c(par("usr")[3] - par("mai")[1] * dataPerInch[2],
                              par("usr")[1] - par("mai")[2] * dataPerInch[1],
                              par("usr")[4] + par("mai")[3] * dataPerInch[2],
                              par("usr")[2] + par("mai")[4] * dataPerInch[1]
                              )

        out <- figureBoundaries

    } else if (region == "device") {

        ## The boundaries are identified relative to the currently selected panel.

        oldMFG <- par("mfg")
        dataPerInch <- getDataPerInch()
        numRows <- par("mfg")[3]
        numCols <- par("mfg")[4]

        allFigureRanges <- list()
        allFigureRanges$x <- matrix(NA_real_, nrow = numRows, ncol = numCols)
        allFigureRanges$y <- matrix(NA_real_, nrow = numRows, ncol = numCols)

        for (ii in seq_len(numRows)) {

            for (jj in seq_len(numCols)) {

                par(mfg = c(ii, jj))

                currentFigureRange <- getRange("figure", "in")

                allFigureRanges$x[ii, jj] <- currentFigureRange[1]
                allFigureRanges$y[ii, jj] <- currentFigureRange[2]

            }
        }

        par(mfg = oldMFG)

        ## Corrections for other panels above, to the right, below, or to the left.
        c1 <- sum(allFigureRanges$y[seq_len(numRows) > oldMFG[1], oldMFG[2]])
        c3 <- sum(allFigureRanges$y[seq_len(numRows) < oldMFG[1], oldMFG[2]])
        c2 <- sum(allFigureRanges$x[oldMFG[1], seq_len(numCols) < oldMFG[2]])
        c4 <- sum(allFigureRanges$x[oldMFG[1], seq_len(numCols) > oldMFG[2]])

        ## bottom, left, top, right
        deviceBoundaries <- c(par("usr")[3] - (par("mai")[1] + c1 + par("omi")[1]) * dataPerInch[2],
                              par("usr")[1] - (par("mai")[2] + c2 + par("omi")[2]) * dataPerInch[1],
                              par("usr")[4] + (par("mai")[3] + c3 + par("omi")[3]) * dataPerInch[2],
                              par("usr")[2] + (par("mai")[4] + c4 + par("omi")[4]) * dataPerInch[1]
                              )

        out <- deviceBoundaries

    }

    if (units == "lines") {

        for (ii in 1:4)
            out[ii] <- convertUnits("data", out[ii], "line", side = sides[ii])

    }

    out

}
