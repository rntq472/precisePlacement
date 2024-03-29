% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relativeUnitConversions.R
\name{getDataPerLine}
\alias{getDataPerLine}
\title{Find the Number of Data Points Per Margin Line in a Plot}
\usage{
getDataPerLine()
}
\value{
Numeric vector of length two giving the number of data points per line,
        for the x axis and y axis, respectively.
}
\description{
Find the Number of Data Points Per Margin Line in a Plot
}
\examples{
\dontrun{

plot(1:10)
print(getDataPerLine())

}

}
\author{
Jasper Watson
}
