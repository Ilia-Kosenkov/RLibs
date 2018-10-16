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
    labsList = NULL, axisList = NULL, extraGrobs = NULL) {

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

    #grobs %<>% map(GrobSetGaps,
    #right = halfHgap, left = halfHgap,
    #top = halfVgap, bottom = halfVgap)

    #grobs <<- grobs
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

    if (ncol == 1)
        widths <- 1
    else if (ncol == 2)
        widths <- c(
            convertX(labsList$left + axisList$left +
                halfHgap + unit(hSize, "cm"), "cm", TRUE),
            convertX(labsList$right + axisList$right +
                halfHgap + unit(hSize, "cm"), "cm", TRUE)) /
                    vpWidthCm

    else widths <- c(
            convertX(labsList$left + axisList$left +
                halfHgap + unit(hSize, "cm"), "cm", TRUE),
            rep(convertX(unit(hSize, "cm") + hGap, "cm", TRUE), ncol - 2),
            convertX(labsList$right + axisList$right +
                halfHgap + unit(hSize, "cm"), "cm", TRUE)) /
                    vpWidthCm


    if (nrow == 1)
        heights <- 1
    else if (nrow == 2)
        heights <- c(
            convertY(labsList$top + axisList$top +
                halfVgap + unit(vSize, "cm"), "cm", TRUE),
            convertY(labsList$bottom + axisList$bottom +
                halfVgap + unit(vSize, "cm"), "cm", TRUE)) /
                    vpHeightCm
    else heights <- c(
            convertY(labsList$top + axisList$top +
                halfVgap + unit(vSize, "cm"), "cm", TRUE),
            rep(convertY(unit(vSize, "cm") + vGap, "cm", TRUE), nrow - 2),
            convertY(labsList$bottom + axisList$bottom +
                halfVgap + unit(vSize, "cm"), "cm", TRUE)) /
                    vpHeightCm

    grb <- arrangeGrob(grobs = grobs, widths = widths, heights = heights)

    for (g in extraGrobs) {
        grb$grobs %<>% append(list(g))
        grb$layout %<>% rbind(c(1, 1, nrow, ncol,
            ncol * nrow + 1, "off", "extra"))
    }
    return (grb)
}