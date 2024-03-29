% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnnotatePlot.R
\name{highlightFigureRegion}
\alias{highlightFigureRegion}
\title{Highlight the Figure Region of a Plot}
\usage{
highlightFigureRegion(border = "orange", col = adjustcolor(border, 0.1))
}
\arguments{
\item{border}{Parameter to be passed to graphics::rect. Defaults to "green".}

\item{col}{Parameter to be passed to graphics::rect. Defaults to
adjustcolor(border, 0.1).}
}
\description{
Highlight the Figure Region of a Plot
}
\examples{
\dontrun{

plot(1:10)
par(xpd = NA)
highlightFigureRegion()

}

}
\author{
Jasper Watson
}
