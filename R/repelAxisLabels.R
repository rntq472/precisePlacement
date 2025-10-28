##' Distribute Axis Label Locations to Avoid Overlap.
##'
##' @param side Integer giving the side of the plot, as in graphics::axis.
##' @param at The points at which the axis labels and tickmarks would ordinarily be plotted.
##' @param labels Character vector of axis labels.
##' @param ... Other arguments passed to strheight and strwidth, for example font.
##' @param spacing Character string giving any spacing desired between the adjacent labels.
##' Defaults to an empty string.
##' @param reduceCex Logical indicating whether axis labels should be shrunk in size if
##' they don't all fit. Defaults to TRUE.
##'
##' @return List containing the fields "originalAt", "at", and "cex.axis". "originalAt" is
##' the input parameter "at" that has been sorted, "at" is a vector of new axis locations,
##' and "cex.axis" is the cex value used to fit the labels without overlap (only necessary
##' if reduceCex is TRUE).
##'
##' @note This function is experimental and just uses simple heuristics instead of
##' any physics logic. It worked the one time I wanted it but if it doesn't
##' generalise well I may not invest the time to try improving it.
##'
##' Also note that this function does not work when a plot is resized; you will have to
##' re-run it after resizing.
##'
##' @author Jasper Watson
##'
##' @importFrom stats setNames
##'
##' @export
##'
##
repelAxisLabels <- function(side, at, labels, ..., spacing = "", reduceCex = TRUE) {

    ## Ugly hack to catch, for example, both repelAxisLabels(las) where it is already
    ## defined and repelAxisLabels(las = 2).
    tmp1 <- setNames(list(...),
                     vapply(substitute(list(...)), deparse, character(1))[-1])
    tmp2 <- list(...)

    stringSizeArgs <- list()
    plotArgs <- list()

    ## hadj, padj, gap.axis, vfont ?

    for (pp in c("font.axis", "family")) {

        if (pp %in% names(tmp1)) {

            stringSizeArgs[[pp]] <- tmp1[[pp]]

        } else if (pp %in% names(tmp2)) {

            stringSizeArgs[[pp]] <- tmp2[[pp]]

        } else {
            stringSizeArgs[[pp]] <- par(pp)
        }

    }
    for (pp in c("cex.axis", "las", "ylbias")) {

        if (pp %in% names(tmp1)) {

            plotArgs[[pp]] <- tmp1[[pp]]

        } else if (pp %in% names(tmp2)) {

            plotArgs[[pp]] <- tmp2[[pp]]

        } else {
            plotArgs[[pp]] <- par(pp)
        }
    }

    stopifnot(side %in% 1:4, plotArgs$las %in% 0:3)

    if (plotArgs$las %in% 1 ||
        (side %in% c(1, 3) && plotArgs$las %in% 0) ||
        (side %in% c(2, 4) && plotArgs$las %in% 2)
        ) {

        orientation <- "Horizontal"

    } else if (plotArgs$las %in% 3 ||
               (side %in% c(1, 3) && plotArgs$las %in% 2) ||
               (side %in% c(2, 4) && plotArgs$las %in% 0)
               ) {

        orientation <- "Vertical"

    }

    originalOrder <- order(at)
    at <- sort(at)

    if (orientation == "Horizontal" && side %in% c(1, 3)) {

        FUN <- function(...)
            strwidth(..., units = "inches") * getDataPerInch()[1]

        allowedSpace <- getRange("plot", "data")[1]

    } else if (orientation == "Horizontal" && side %in% c(2, 4)) {

        FUN <- function(...)
            strheight(..., units = "inches") * getDataPerInch()[2]

        allowedSpace <- getRange("plot", "data")[2]

    } else if (orientation == "Vertical" && side %in% c(1, 3)) {

        FUN <- function(...)
            strheight(..., units = "inches") * getDataPerInch()[1]

        allowedSpace <- getRange("plot", "data")[1]

    } else if (orientation == "Vertical" && side %in% c(2, 4)) {

        FUN <- function(...)
            strwidth(..., units = "inches") * getDataPerInch()[2]

        allowedSpace <- getRange("plot", "data")[2]

    }

    allWidths <- do.call(FUN, append(list(s = paste0(labels, spacing)), stringSizeArgs))

    totalSize <- sum(allWidths)

    if (totalSize > allowedSpace) {

        if (reduceCex) {

            for (ii in seq(plotArgs$cex.axis, 0, by = -0.01)) {

                allWidths <- do.call(FUN, append(list(s = paste0(labels, spacing), cex = ii), stringSizeArgs))

                totalSize <- sum(allWidths)

                if (totalSize < allowedSpace) {

                    plotArgs$cex.axis <- ii

                    break

                }
            }
        } else {
            stop("Labels will not fit and reduceCex parameter is FALSE")
        }
    }

    originalAt <- at

    bindings <- rep(NA, length(at))
    cantMoveFurtherUp <- rep(FALSE, length(at))
    cantMoveFurtherDown <- rep(FALSE, length(at))

    noChanges <- FALSE

    count <- 0

    repeat {

        count <- count + 1

        upperBoundaries <- vapply(seq_along(at), function(ii) {
            if (is.na(bindings[ii])) {
                at[ii] + allWidths[ii] / 2
            } else {
                use <- bindings %in% bindings[ii]
                max(at[use] + allWidths[use] / 2)
            }
        }, numeric(1))
        lowerBoundaries <- vapply(seq_along(at), function(ii) {
            if (is.na(bindings[ii])) {
                at[ii] - allWidths[ii] / 2
            } else {
                use <- bindings %in% bindings[ii]
                min(at[use] - allWidths[use] / 2)
            }
        }, numeric(1))

        if (side %in% c(1, 3)) {

            bottomBoundary <- par("usr")[1]
            topBoundary <- par("usr")[2]

        } else if (side %in% c(2, 4)) {

            bottomBoundary <- par("usr")[3]
            topBoundary <- par("usr")[4]

        }

        tmp.use <- upperBoundaries >= topBoundary

        if (any(tmp.use)) {

            if (all(is.na(bindings))) {
                cantMoveFurtherUp[tmp.use] <- TRUE
            } else {
                cantMoveFurtherUp[which(!is.na(bindings) & bindings %in% bindings[tmp.use])] <- TRUE
            }
        }

        tmp.use <- lowerBoundaries <= bottomBoundary

        if (any(tmp.use)) {

            if (all(is.na(bindings))) {
                cantMoveFurtherDown[tmp.use] <- TRUE
            } else {
                cantMoveFurtherDown[which(!is.na(bindings) & bindings %in% bindings[tmp.use])] <- TRUE
            }
        }

        ## For each value of "at", check if any other labels overlap. If they do,
        ## identify the one that is closest.
        overlap <- lapply(seq_along(at), function(ii) {

            ## Can't use at[-ii] as that would mess with the which statement below.
            use <- at != at[ii] &
                lowerBoundaries < upperBoundaries[ii] & upperBoundaries > lowerBoundaries[ii]

            use <- use & !(!is.na(bindings) & bindings %in% bindings[ii])

            if (any(use)) {

                ## Pick the closest.
                ind <- which(use)
                ind[which.min(abs(ind - ii))]

            } else {
                FALSE
            }
        })

        hasOverlap <- vapply(overlap, is.numeric, logical(1))

        if (!any(hasOverlap) && count == 1) {
            noChanges <- TRUE
            break
        }

        ## For each value of "at" that has an overlapping value, measure the amount
        ## the offending value would need to move.
        needToShiftUp <- vapply(seq_along(at), function(ii) {
            if (hasOverlap[ii]) {
                upperBoundaries[ii] - lowerBoundaries[overlap[[ii]]]
            } else {
                Inf
            }
        }, numeric(1))

        needToShiftDown <- vapply(seq_along(at), function(ii) {
            if (hasOverlap[ii]) {
                lowerBoundaries[ii] - upperBoundaries[overlap[[ii]]]
            } else {
                -Inf
            }
        }, numeric(1))

        needToShiftUp[!hasOverlap] <- Inf
        needToShiftDown[!hasOverlap] <- -Inf

        needToShiftUp[needToShiftUp < 0] <- Inf
        needToShiftDown[needToShiftDown > 0] <- -Inf

        if (any(cantMoveFurtherUp))
            cantMoveFurtherUp <- cantMoveFurtherUp |
                (upperBoundaries >= min(lowerBoundaries[cantMoveFurtherUp]))
        if (any(cantMoveFurtherDown))
            cantMoveFurtherDown <- cantMoveFurtherDown |
                (lowerBoundaries <= max(upperBoundaries[cantMoveFurtherDown]))

        needToShiftUp[cantMoveFurtherUp] <- Inf
        needToShiftDown[cantMoveFurtherDown] <- -Inf

        ## Start with the value of "at" that has the smallest gap, i.e. the two labels
        ## that are the most tightly overlapped.

        if (all(is.infinite(needToShiftUp)) && all(is.infinite(needToShiftDown))) {

            break

        } else if (all(is.infinite(needToShiftDown)) ||
                   min(needToShiftUp) < min(abs(needToShiftDown))) {

            direction <- "Up"
            referencePoint <- which.min(needToShiftUp)
            oneToMove <- overlap[[referencePoint]]
            amountToMove <- needToShiftUp[referencePoint]

            if (referencePoint > oneToMove) {

                save.referencePoint <- referencePoint
                save.oneToMove <- oneToMove

                referencePoint <- save.oneToMove
                oneToMove <- save.referencePoint
                amountToMove <- needToShiftUp[referencePoint]

            }
        } else {

            direction <- "Down"
            referencePoint <- which.min(abs(needToShiftDown))
            oneToMove <- overlap[[referencePoint]]
            amountToMove <- needToShiftDown[referencePoint]

            if (referencePoint < oneToMove) {

                save.referencePoint <- referencePoint
                save.oneToMove <- oneToMove

                referencePoint <- save.oneToMove
                oneToMove <- save.referencePoint
                amountToMove <- needToShiftUp[referencePoint]

            }
        }

        if (any(cantMoveFurtherDown)) {
            boundaryLeft <- max(upperBoundaries[cantMoveFurtherDown])
        } else {
            boundaryLeft <- bottomBoundary
        }
        if (any(cantMoveFurtherUp)) {
            boundaryRight <- min(lowerBoundaries[cantMoveFurtherUp])
        } else {
            boundaryRight <- topBoundary
        }

        if (direction == "Up") {

            adjustment <- (upperBoundaries[oneToMove] + amountToMove) - boundaryRight

            amountToMove <- amountToMove + max(0, adjustment)

            notGood <- amountToMove < .Machine$double.eps^0.5

        } else {

            adjustment <- boundaryLeft - (lowerBoundaries[oneToMove] + amountToMove)
            amountToMove <- amountToMove + max(0, adjustment)

            notGood <- amountToMove > -.Machine$double.eps^0.5

        }

        ## Need to check if there is actually no room in that direction after the adjustment.

        if (notGood) {

            if (direction == "Up") {

                if (!is.na(bindings[oneToMove])) {
                    cantMoveFurtherUp[which(!is.na(bindings) & bindings %in% bindings[oneToMove])] <- TRUE
                } else {
                    cantMoveFurtherUp[oneToMove] <- TRUE
                }

            } else {
                if (!is.na(bindings[oneToMove])) {
                    cantMoveFurtherDown[which(!is.na(bindings) & bindings %in% bindings[oneToMove])] <- TRUE
                } else {
                    cantMoveFurtherDown[oneToMove] <- TRUE
                }
            }

            next
        }

        ## If there is an adjustment then we won't have moved enough to remove the overlap so we don't want any bindings?

        tmp <- which(!is.na(bindings))

        if (oneToMove %in% tmp) {

            indicesToCombine <- which(!is.na(bindings) &
                                      bindings %in% bindings[oneToMove]
                                      )

            if (adjustment <= .Machine$double.eps^0.5) {

                if (!is.na(bindings[referencePoint])) {
                    tempIndex <- which(!is.na(bindings) & bindings %in% bindings[referencePoint])
                } else {
                    tempIndex <- referencePoint
                }

                bindings[c(tempIndex, indicesToCombine)] <- bindings[oneToMove]

            }

            at[indicesToCombine] <- at[indicesToCombine] + amountToMove

        } else if (!is.na(bindings[referencePoint])) {

            indicesToCombine <- which(!is.na(bindings) &
                                      bindings %in% bindings[referencePoint]
                                      )

            if (adjustment <= .Machine$double.eps^0.5) {

                if (!is.na(bindings[oneToMove])) {
                    tempIndex <- which(!is.na(bindings) & bindings %in% bindings[oneToMove])
                } else {
                    tempIndex <- oneToMove
                }

                bindings[c(tempIndex, indicesToCombine)] <- bindings[referencePoint]

            }

            at[oneToMove] <- at[oneToMove] + amountToMove

        } else {

            indicesToCombine <- oneToMove

            if (adjustment <= .Machine$double.eps^0.5)
                ## Could have 1 and 2 then get bound together as 2.
                if (all(is.na(bindings))) {
                    bindings[c(referencePoint, indicesToCombine)] <- 1
                } else {
                    bindings[c(referencePoint, indicesToCombine)] <- max(bindings[!is.na(bindings)]) + 1
                }

            at[indicesToCombine] <- at[indicesToCombine] + amountToMove

        }

        if (count == 10000)
            break
    }

    ## Need to check everything for being able to shift closer to its original point without overlapping
    ## with anything, for cases where, to keep the ordering correct, we spaced things out excessively.

    count <- 0

    while (!noChanges) {

        count <- count + 1

        upperBoundaries <- do.call(c, lapply(seq_along(at), function(ii) {
            at[ii] + allWidths[ii] / 2
        }))
        lowerBoundaries <- do.call(c, lapply(seq_along(at), function(ii) {
            at[ii] - allWidths[ii] / 2
        }))

        cantMove <- rep(FALSE, length(at))

        for (ii in seq_along(at)) {

            if (ii == length(at) || at[ii] - originalAt[ii] > 0) {

                nearestBoundary <- upperBoundaries[ii - 1]
                adjustment <- lowerBoundaries[ii] - nearestBoundary

            } else {

                nearestBoundary <- lowerBoundaries[ii + 1]
                adjustment <- upperBoundaries[ii] - nearestBoundary

            }

            if (length(adjustment) == 0) {
                cantMove[] <- TRUE
                break
            }

            if (abs(adjustment) > (diff(range(at)) / 100)) {

                at[ii] <- at[ii] - adjustment

            } else {

                cantMove[ii] <- TRUE

            }
        }

        if (all(cantMove))
            break

        if (count == 10000)
            break
    }

    ind <- vapply(seq_along(at), function(ii) {
        which(originalOrder == ii)
    }, numeric(1))

    at <- at[ind]
    originalAt <- originalAt[ind]

    list(originalAt = originalAt, at = at, cex.axis = plotArgs$cex.axis)

}
