% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relativeUnitConversions.R
\name{getInchesPerDatum}
\alias{getInchesPerDatum}
\title{Find the Number of Inches Per Data Point in a Plot}
\usage{
getInchesPerDatum()
}
\value{
Numeric vector of length two giving the number of inches per data point,
        for the x axis and y axis, respectively.
}
\description{
Find the Number of Inches Per Data Point in a Plot
}
\examples{
\dontrun{

plot(1:10)
print(getInchesPerDatum())

}

}
\author{
Jasper Watson
}
