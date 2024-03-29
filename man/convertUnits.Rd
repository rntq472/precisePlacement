% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertUnits.R
\name{convertUnits}
\alias{convertUnits}
\title{Convert Between Different Available Units That Measure Points on a Plot}
\usage{
convertUnits(from, value, to, side = NULL, axis = NULL, region = "plot")
}
\arguments{
\item{from}{Units one wishes to convert from. Allowed values are "line", "data",
and "proportion".}

\item{value}{Numeric value(s) of the coordinate(s) one wishes to convert.}

\item{to}{Units one wishes to convert to. Allowed values are "line", "data",
and "proportion".}

\item{side}{Integer giving the side of the plot to count lines from.}

\item{axis}{One of "x" or "y", giving the axis a proportion should be calculated
from.}

\item{region}{Required when either from or to has the value "proportion". Must
be one of "device", "figure", "plot", or "data". Defaults to "plot".}
}
\value{
Numeric value(s) of the input coordinates converted into the new units.
}
\description{
Convert Between Different Available Units That Measure Points on a Plot
}
\examples{
\dontrun{

## Illustrate where the lines fall when using mtext:

plot(1:10)
mtext(1:26, line = -(1:26), side = 1, col = 'blue')
x <- convertUnits('line', 0:(-26), 'data', side = 1)
abline(h = x, col = 'red', lty = 2)

## Show how proportions of a plot can be identified:

plot(seq(as.Date('2018-01-01'), as.Date('2019-01-01'), length.out = 10), 1:10,
     pch = 19)

## Identify the "center" of the plot.
abline(h = convertUnits('proportion', 0.5, 'data', axis = 'y'),
       col = 'red', lwd = 4)
abline(v = convertUnits('proportion', 0.5, 'data', axis = 'x'),
       col = 'blue', lwd = 4)

print(convertUnits('proportion', 0.5, 'data', axis = 'y'))
## as.Date is needed because convertUnits returns a numeric value.
print(as.Date(convertUnits('proportion', 0.5, 'data', axis = 'x'),
      origin = '1970-01-01'))

## Change the region we are defining the proportions from.
abline(v = convertUnits('proportion', 0.75, 'data', axis = 'x', region = 'plot'),
       col = 'darkgreen', lwd = 4)
abline(v = convertUnits('proportion', 0.75, 'data', axis = 'x', region = 'device'),
       col = 'orange', lwd = 4)

}

}
\author{
Jasper Watson
}
