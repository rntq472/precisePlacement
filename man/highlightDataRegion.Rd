% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnnotatePlot.R
\name{highlightDataRegion}
\alias{highlightDataRegion}
\title{Highlight the Data Region of a Plot}
\usage{
highlightDataRegion(border = "green", col = adjustcolor(border, 0.1))
}
\arguments{
\item{border}{Parameter to be passed to graphics::rect. Defaults to "green".}

\item{col}{Parameter to be passed to graphics::rect. Defaults to
adjustcolor(border, 0.1).}
}
\description{
Highlight the Data Region of a Plot
}
\examples{
\dontrun{

plot(1:10)
highlightDataRegion()

}

}
\author{
Jasper Watson
}
