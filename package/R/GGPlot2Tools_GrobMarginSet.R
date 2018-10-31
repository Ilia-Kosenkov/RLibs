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


utils::globalVariables("gtable")
#' GrobMarginSet
#'
#' @param grob Input grob.
#' @param tLab Top label margin.
#' @param rLab Right label margin.
#' @param bLab Bottom label margin.
#' @param lLab Left label margin.
#' @param tAxis Top axis margin.
#' @param rAxis Right axis margin.
#' @param bAxis Bottom axis margin.
#' @param lAxis Left axis margin.
#' @param labsList When not NULL, is used instead of other label parameters.
#' @param axisList When not NULL, is used instead of other axis parameters.
#' @param labsMar A \code{margin} item used instead of \code{labsList}.
#' @param axisMar A \code{margin} item used instead of \code{axisList}.
#'
#' @return Modified grob.
#' @importFrom rlang invoke
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @export
#'
GrobMarginSet <- function(grob,
    tLab = NULL, rLab = NULL, bLab = NULL, lLab = NULL,
    tAxis = NULL, rAxis = NULL, bAxis = NULL, lAxis = NULL,
    labsList = NULL, axisList = NULL,
    labsMar = NULL,
    axisMar = NULL) {

    if (grob %is% gtable &&
        grob %is% gTree)
        worker <- function(...)
            invoke(GrobSizeSet, append(list("grob" = grob), list(...)),
            .bury = NULL)
    else
        worker <- function(...) {
            args <- list(...)
            grob %>%
                map(~invoke(GrobSizeSet, append(list("grob" = .x), args),
                .bury = NULL))
        }

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

    if (all(is.null(labsList)))
        labsList <- list(top = tLab, right = rLab,
            bottom = bLab, left = lLab)

    if (all(is.null(axisList)))
        axisList <- list(top = tAxis, right = rAxis,
            bottom = bAxis, left = lAxis)

    if (!is.null(labsList$top))
        #grob %<>% GrobSizeSet("xlab-t", height = labsList$top)
        grob <- worker(name = "xlab-t", height = labsList$top)
    if (!is.null(labsList$right))
        #grob %<>% GrobSizeSet("ylab-r", width = labsList$right)
        grob <- worker(name = "ylab-r", width = labsList$right)
    if (!is.null(labsList$bottom))
        #grob %<>% GrobSizeSet("xlab-b", height = labsList$bottom)
        grob <- worker(name = "xlab-b", height = labsList$bottom)
    if (!is.null(labsList$left))
        #grob %<>% GrobSizeSet("ylab-l", width = labsList$left)
        grob <- worker(name = "ylab-l", width = labsList$left)

    if (!is.null(axisList$top))
        #grob %<>% GrobSizeSet("axis-t", height = axisList$top)
        grob <- worker(name = "axis-t", height = axisList$top)
    if (!is.null(axisList$right))
        #grob %<>% GrobSizeSet("axis-r", width = axisList$right)
        grob <- worker(name = "axis-r", width = axisList$right)
    if (!is.null(axisList$bottom))
        #grob %<>% GrobSizeSet("axis-b", height = axisList$bottom)
        grob <- worker(name = "axis-b", height = axisList$bottom)
    if (!is.null(axisList$left))
        #grob %<>% GrobSizeSet("axis-l", width = axisList$left)
        grob <- worker(name = "axis-l", width = axisList$left)


    invisible(grob)
}