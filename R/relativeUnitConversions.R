##' Find the Number of Margin Lines Per Inch in a Plot
##'
##' @return Numeric vector of length two giving the number of lines per inch,
##' for the x axis and y axis, respectively.
##'
##' @note The number of lines per inch is the same for both the x and y axes but
##' we return a vector of length two to maintain consistency with all of the
##' other getXperY style functions.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getLinesPerInch())
##'
##' }
##'
##' @export
##'
##
getLinesPerInch <- function() {

    out <- if (setequal(par("mar"), 0) && setequal(par("oma"), 0)) {

               if (setequal(par("mfg")[3:4], 1)) {

                   1 / par("cin")[2]

               } else {

                   stop("Cannot estimate lines per inch for a multi-panel plot with no",
                        "inner or outer margins.")
               }

           } else {

               if (setequal(par("mar"), 0)) {
                   (par("oma") / par("omi"))[par("oma") != 0][1]
               } else {
                   (par("mar") / par("mai"))[par("mar") != 0][1]
               }

           }

    rep(out, 2)

}


##' Find the Number of Inches Per Margin Line in a Plot
##'
##' @return Numeric vector of length two giving the number of inches per line,
##' for the x axis and y axis, respectively.
##'
##' @note The number of lines per inch is the same for both the x and y axes but
##' we return a vector of length two to maintain consistency with all of the
##' other getXperY style functions.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getInchesPerLine())
##'
##' }
##'
##' @export
##'
##
getInchesPerLine <- function()
    1 / getLinesPerInch()


##' Find the Number of Data Points Per Margin Line in a Plot
##'
##' @return Numeric vector of length two giving the number of data points per line,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getDataPerLine())
##'
##' }
##'
##' @export
##'
##
getDataPerLine <- function()
    getRange("plot", "data") / getRange("plot", "lines")


##' Find the Number of Margin Lines Per Data Point in a Plot
##'
##' @return Numeric vector of length two giving the number of lines per data point,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getLinesPerDatum())
##'
##' }
##'
##' @export
##'
##
getLinesPerDatum <- function()
    1 / getDataPerLine()


##' Find the Number of Data Points Per Inch in a Plot
##'
##' @return Numeric vector of length two giving the number of data points per inch,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getDataPerInch())
##'
##' }
##'
##' @export
##'
##
getDataPerInch <- function() {

    ## deviceSizeInches <- dev.size('in') ## (width, height)
    ## outerMarginSizeInches <- par('omi')[c(2, 1)] + par('omi')[c(4, 3)]
    ## innerMarginSizeInches <- par('mai')[c(2, 1)] + par('mai')[c(4, 3)]
    ## plotSizeInches <- deviceSizeInches - outerMarginSizeInches - innerMarginSizeInches

    ## Equivalent to:
    plotSizeInches <- par("pin")

    plotSizeUnits <- par("usr")[c(2, 4)] - par("usr")[c(1, 3)]

    dataPerInch <- plotSizeUnits / plotSizeInches

    dataPerInch

}

##' Find the Number of Inches Per Data Point in a Plot
##'
##' @return Numeric vector of length two giving the number of inches per data point,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getInchesPerDatum())
##'
##' }
##'
##' @export
##'
##
getInchesPerDatum <- function()
    1 / getDataPerInch()


##' Find the Number of Pixels Per Inch in a Plot
##'
##' @return Numeric vector of length two giving the number of pixels per inch,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getPixelsPerInch())
##'
##' }
##'
##' @export
##'
##
getPixelsPerInch <- function()
    1 / getInchesPerPixel()


##' Find the Number of Inches Per Pixel in a Plot
##'
##' @return Numeric vector of length two giving the number of inches per pixel,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getInchesPerPixel())
##'
##' }
##'
##' @export
##'
##
getInchesPerPixel <- function()
    dev.size("in") / dev.size("px")


##' Find the Number of Pixels Per Margin Line in a Plot
##'
##' @return Numeric vector of length two giving the number of pixels per line,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getPixelsPerLine())
##'
##' }
##'
##' @export
##'
##
getPixelsPerLine <- function()
    getRange("device", "px") / getRange("device", "lines")


##' Find the Number of Margin Lines Per Pixel in a Plot
##'
##' @return Numeric vector of length two giving the number of lines per pixel,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getLinesPerPixel())
##'
##' }
##'
##' @export
##'
##
getLinesPerPixel <- function()
    1 / getPixelsPerLine()


##' Find the Number of Pixels Per Data Point in a Plot
##'
##' @return Numeric vector of length two giving the number of pixels per data point,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getPixelsPerDatum())
##'
##' }
##'
##' @export
##'
##
getPixelsPerDatum <- function()
    1 / getDataPerPixel()


##' Find the Number of Data Points Per Pixel in a Plot
##'
##' @return Numeric vector of length two giving the number of data points per pixel,
##' for the x axis and y axis, respectively.
##'
##' @author Jasper Watson
##'
##' @examples
##' \dontrun{
##'
##' plot(1:10)
##' print(getDataPerPixel())
##'
##' }
##'
##' @export
##'
##
getDataPerPixel <- function() {

    deviceSizePixels <- dev.size("px") ## (width, height)

    plotFraction <- getRange("plot", "in") / getRange("device", "in")

    plotSizePixels <- deviceSizePixels * plotFraction

    plotSizeUnits <- getRange("plot", "data")

    dataPerPixel <- plotSizeUnits / plotSizePixels

    dataPerPixel

}
