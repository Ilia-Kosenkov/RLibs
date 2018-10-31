#   MIT License
#
#   Copyright(c) 2017-2018 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' GrobsArrange
#'
#' @param grobs Input grobs.
#' @param ncol Number of columns.
#' @param nrow Number of rows.
#' @param tLab Top label margin.
#' @param rLab Right label margin.
#' @param bLab Bottom label margin.
#' @param lLab Left label margin.
#' @param tAxis Top axis margin.
#' @param rAxis Right axis margin.
#' @param bAxis Bottom axis margin.
#' @param lAxis Left axis margin.
#' @param vGap Vertical gap between plots.
#' @param hGap Horizontal gap between plots.
#' @param labsList Label margins in form of named list.
#' If not \code{NULL}, used instead of other provided values.
#' @param axisList Axis margins in form of named list.
#' If not \code{NULL}, used instead of other provided values.
#' @param topLab \code{textGrob} which can act as shared top axis label.
#' @param bottomLab \code{textGrob} which can act as shared bottom axis label.
#' @param leftLab \code{textGrob} which can act as shared left axis label.
#' @param rightLab \code{textGrob} which can act as shared right axis label.
#' @param labsMar A \code{margin} item used instead of \code{labsList}.
#' @param axisMar A \code{margin} item used instead of \code{axisList}.
#'
#' @return GTable of arranged grobs.
#' @importFrom gridExtra arrangeGrob
#' @importFrom magrittr %<>%
#' @importFrom purrr map
#' @import grid
#' @export
#'
GrobsArrange <- function(grobs, ncol, nrow = length(grobs) %/% ncol,
    tLab = unit(0.6, "cm"), rLab = unit(0.6, "cm"),
    bLab = unit(0.6, "cm"), lLab = unit(0.6, "cm"),
    tAxis = unit(0.6, "cm"), rAxis = unit(0.6, "cm"),
    bAxis = unit(0.6, "cm"), lAxis = unit(0.6, "cm"),
    vGap = unit(0.1, "cm"), hGap = unit(0.1, "cm"),
    labsList = NULL, axisList = NULL,
    labsMar = NULL, axisMar = NULL,
    topLab = NULL, bottomLab = NULL, leftLab = NULL, rightLab = NULL) {

    if (all(!is.null(labsMar)))
        labsList <- list(
            top = labsMar[1],
            right = labsMar[2],
            bottom = labsMar[3],
            left = labsMar[4])

    if (all(!is.null(axisMar)))
        axisList <- list(
            top = axisMar[1],
            right = axisMar[2],
            bottom = axisMar[3],
            left = axisMar[4])

    if (all(is.null(labsList))) {
        labsList <- list()
        if (!is.null(tLab))
            labsList$top <- tLab
        if (!is.null(rLab))
            labsList$right <- rLab
        if (!is.null(bLab))
            labsList$bottom <- bLab
        if (!is.null(lLab))
            labsList$left <- lLab
        }

    if (all(is.null(axisList))) {
        axisList <- list()
        if (!is.null(tAxis))
            axisList$top <- tAxis
        if (!is.null(rAxis))
            axisList$right <- rAxis
        if (!is.null(bAxis))
            axisList$bottom <- bAxis
        if (!is.null(lAxis))
            axisList$left <- lAxis
        }

    n <- ncol * nrow

    halfVgap <- 0.5 * vGap
    halfHgap <- 0.5 * hGap


    lInds <- 1 + ((1:nrow) - 1) * ncol
    rInds <- lInds + (ncol - 1)
    tInds <- (1:ncol)
    bInds <- n - (ncol:1) + 1


    grobs[lInds] %<>% map(GrobMarginSet,
            lLab = labsList$left, lAxis = axisList$left) %>%
        map(GrobSetGaps, left = unit(0, "pt"))
    grobs[-lInds] %<>% map(GrobMarginSet,
                           lLab = unit(0, "cm"), lAxis = unit(0, "cm")) %>%
        map(GrobZero, c("ylab-l")) %>%
        map(GrobSetGaps, left = halfHgap)

    grobs[rInds] %<>% map(GrobMarginSet,
            rLab = labsList$right, rAxis = axisList$right) %>%
        map(GrobSetGaps, right = unit(0, "pt"))
    grobs[-rInds] %<>% map(GrobMarginSet,
                           rLab = unit(0, "cm"), rAxis = unit(0, "cm")) %>%
        map(GrobZero, c("ylab-r")) %>%
        map(GrobSetGaps, right = halfHgap)

    grobs[tInds] %<>% map(GrobMarginSet,
            tLab = labsList$top, tAxis = axisList$top) %>%
        map(GrobSetGaps, top = unit(0, "pt"))
    grobs[-tInds] %<>% map(GrobMarginSet,
                           tLab = unit(0, "cm"), tAxis = unit(0, "cm")) %>%
        map(GrobZero, c("xlab-t")) %>%
        map(GrobSetGaps, top = halfVgap)

    grobs[bInds] %<>% map(GrobMarginSet,
            bLab = labsList$bottom, bAxis = axisList$bottom) %>%
        map(GrobSetGaps, bottom = unit(0, "pt"))
    grobs[-bInds] %<>% map(GrobMarginSet,
                           bLab = unit(0, "cm"), bAxis = unit(0, "cm")) %>%
        map(GrobZero, c("xlab-b")) %>%
        map(GrobSetGaps, bottom = halfVgap)

    vpWidthCm <- convertX(current.viewport()$width, "cm", TRUE)
    hExtraSpaceCm <- convertX(labsList$left + axisList$left +
                              labsList$right + axisList$right +
                            hGap * (ncol - 1), "cm", TRUE)

    hSize <- (vpWidthCm - hExtraSpaceCm) / ncol

    vpHeightCm <- convertY(current.viewport()$height, "cm", TRUE)
    vExtraSpaceCm <- convertY(labsList$top + axisList$top +
                              labsList$bottom + axisList$bottom +
                            vGap * (nrow - 1), "cm", TRUE)

    vSize <- (vpHeightCm - vExtraSpaceCm) / nrow

    oneWidth <- 1 / ncol * (unit(1, "npc") - (labsList$right + axisList$right +
        labsList$left + axisList$left + (ncol-1) * hGap))

    widths <- rep(oneWidth, ncol)
    widths[1] <- 0.5 * hGap + labsList$left + axisList$left + widths[1]
    widths[ncol] <- 0.5 * hGap + labsList$right + axisList$right + widths[ncol]
    for (i in (1 + seq_len(max(ncol - 2, 0))))
        widths[i] <- hGap + widths[i]


    oneHeight <- 1 / nrow * (unit(1, "npc") - (labsList$top + labsList$bottom +
        axisList$top + axisList$bottom +
        (nrow - 1) * vGap))

    heights <- rep(oneHeight, nrow)
    heights[1] <- 0.5 * vGap + labsList$top + axisList$top +
        heights[1]
    heights[nrow] <- 0.5 * vGap + labsList$bottom + axisList$bottom +
        heights[nrow]
    for (i in (1 + seq_len(max(nrow - 2, 0))))
        heights[i] <- vGap + heights[i]


    grb <- arrangeGrob(grobs = grobs, widths = widths, heights = heights)

    if (!all(is.null(topLab) &&
             is.null(bottomLab) &&
             is.null(leftLab) &&
             is.null(rightLab))) {
        gExtra <- arrangeGrob(topLab, rightLab, bottomLab, leftLab,
            ncol = 3, nrow = 3)

        gExtra$grobs %<>% map_if(is.null, ~textGrob(""))
        gExtra$widths[1] <- labsList$left
        gExtra$widths[2] <- unit(1, "null")
        gExtra$widths[3] <- labsList$right

        gExtra$heights[1] <- labsList$top
        gExtra$heights[2] <- unit(1, "null")
        gExtra$heights[3] <- labsList$bottom

        gExtra$layout[1, ] <- c(1, 1, 1, 3, 1, "off", "top")
        gExtra$layout[2, ] <- c(1, 3, 3, 3, 2, "off", "right")
        gExtra$layout[3 ,] <- c(3, 1, 3, 3, 3, "off", "bottom")
        gExtra$layout[4 ,] <- c(1, 1, 3, 1, 4, "off", "left")

        grb$grobs %<>% append(list(gExtra))
        grb$layout %<>% rbind(c(1, 1, nrow, ncol,
            nrow + ncol + 1, "off", "labs"))


    }
    return (grb)
}