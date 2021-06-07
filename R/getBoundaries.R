##' Determine the Boundaries of a Plot in Terms of the Data Units
##' 
##' @param region The region of the plot to use for defining the boundaries. Must
##'               be one of "device", "figure", "plot", or "data".
##' 
##' @return A numeric vector of length four giving the coordinates of the plotting
##'         boundary, in the order of bottom, left, top, right.
##' 
##' @author Jasper Watson
##' 
##' @examples
##' \dontrun{
##' 
##' plot(1:10)
##' print(getBoundaries('data'))
##' print(getBoundaries('plot'))
##' print(getBoundaries('figure'))
##' print(getBoundaries('device'))
##' 
##' }
##' 
##' @export
##' 
##
getBoundaries <- function(region){

    region <- match.arg(region, choices = c('data', 'plot', 'figure', 'device'))

    if (region == 'data'){
        
        plotBoundaries <- par('usr') ## x1, x2, y1, y2
        
        dataBoundaries <- plotBoundaries
        
        ## Documentation says 0.04 but this is apparently rounded from 0.037037037.
        if (par('xaxs') == 'r'){
            dataBoundaries[1] <- plotBoundaries[1] +
                (diff(plotBoundaries[1:2]) * 0.037037037)
            dataBoundaries[2] <- plotBoundaries[2] -
                (diff(plotBoundaries[1:2]) * 0.037037037)
        }
        if (par('yaxs') == 'r'){
            dataBoundaries[3] <- plotBoundaries[3] +
                (diff(plotBoundaries[3:4]) * 0.037037037)
            dataBoundaries[4] <- plotBoundaries[4] -
                (diff(plotBoundaries[3:4]) * 0.037037037)
        }
        
        dataBoundaries[c(3, 1, 4, 2)] ## bottom, left, top, right
        
    } else if (region == 'plot'){

        plotBoundaries <- par('usr') ## x1, x2, y1, y2

        plotBoundaries[c(3, 1, 4, 2)] ## bottom, left, top, right
        
    } else if (region == 'figure'){

        dataPerInch <- getDataPerInch()

        figureBoundaries <- c(par('usr')[3] - par('mai')[1] * dataPerInch[2],
                              par('usr')[1] - par('mai')[2] * dataPerInch[1],
                              par('usr')[4] + par('mai')[3] * dataPerInch[2],
                              par('usr')[2] + par('mai')[4] * dataPerInch[1]
                              )

        figureBoundaries
        
    } else if (region == 'device'){

        ## The boundaries are identified relative to the currently selected panel.
        
        oldMFG <- par('mfg')
        dataPerInch <- getDataPerInch()
        nrow <- par('mfg')[3]
        ncol <- par('mfg')[4]

        allFigureRanges <- list()
        allFigureRanges$x <- matrix(NA_real_, nrow = nrow, ncol = ncol)
        allFigureRanges$y <- matrix(NA_real_, nrow = nrow, ncol = ncol)
        
        for (ii in seq_len(nrow) ){
            
            for (jj in seq_len(ncol) ){

                par(mfg = c(ii, jj))

                currentFigureRange <- getRange('figure', 'in')
                
                allFigureRanges$x[ii, jj] <- currentFigureRange[1]
                allFigureRanges$y[ii, jj] <- currentFigureRange[2]
                
            }
        }
        
        par(mfg = oldMFG)

        ## Corrections for other panels above, to the right, below, or to the left.
        c1 <- sum(allFigureRanges$y[seq_len(nrow) > oldMFG[1], oldMFG[2]])
        c3 <- sum(allFigureRanges$y[seq_len(nrow) < oldMFG[1], oldMFG[2]])
        c2 <- sum(allFigureRanges$x[oldMFG[1], seq_len(ncol) < oldMFG[2]])
        c4 <- sum(allFigureRanges$x[oldMFG[1], seq_len(ncol) > oldMFG[2]])
        
        ## bottom, left, top, right
        deviceBoundaries <- c(par('usr')[3] - (par('mai')[1] + c1 + par('omi')[1]) * dataPerInch[2],
                              par('usr')[1] - (par('mai')[2] + c2 + par('omi')[2]) * dataPerInch[1],
                              par('usr')[4] + (par('mai')[3] + c3 + par('omi')[3]) * dataPerInch[2],
                              par('usr')[2] + (par('mai')[4] + c4 + par('omi')[4]) * dataPerInch[1]
                              )

        deviceBoundaries
        
    }
    
}
