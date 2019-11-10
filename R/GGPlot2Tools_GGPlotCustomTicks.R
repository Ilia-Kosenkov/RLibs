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


#' @title GGPlotCustomTicks
#' @description
#' Creates custom ticks with labels. Requires a finished ggproto object.
#' Can be piped with %>%.
#' @param plt Target plot.
#' @param side Indicates axis. Can be either \code{1,2,3,4},
#' \code{"b", "l", "t", "r"}, or strings containing
#' \code{"bot", "lef", "top", "rig"} substring.
#' @param breaks Breaks in the axis scale.
#' @param labels Respective text labels.
#' @param tckSz Relative size of the tick.
#' @param trnsf Trnsformation function. Should match \code{scale_*} of
#' the target axis. E.g. for \code{scale_x_log10} \code{trnsf}
#' should be \code{log10}.
#' @param gp A \code{gpar} set of parameters passed to the text methods.
#' Affects rendering of the labels.
#' @param rot Rotation of labels, deg.
#' @param deltaH Additional horizontal text shift. If affected by \code{rot}.
#' @param deltaV Additional vertical text shift. Is affected by \code{rot}.
#' @param tickGp \code{gpar} that controls ticks.
#' @export
#' @import ggplot2 grid
GGPlotCustomTicks <- function(plt, side, breaks, labels, tckSz,
                              trnsf = identity, gp = gpar(),
                              rot = 0, deltaH = 0, deltaV = 0,
                              tickGp = gpar()) {

    lifecycle::deprecate_warn("0.6.3", "RLibs::GGPlotCustomTicks()")


    if (!(tckSz %is% unit))
        tckSz <- unit(tckSz, "npc")
    lb <- NULL
    br <- NULL

    if (length(breaks) != length(labels) && length(labels) > 1)
        stop("Length of [breaks] and [labels] should be equal.")
    if (length(labels) == 1)
        labels <- rep(labels, length(breaks))

    rngs <- GGPlotGetRange(plt)

    if (side == 1 ||
        side == "b" ||
        regexpr("bot", side) > 0) {
        breaks <- (trnsf(breaks) - min(rngs$x)) / diff(rngs$x)
        # Return
        plt +
            annotation_custom(
                grob = textGrob(label = labels, gp = gp, rot = rot,
                    hjust = 0.5 + deltaH, vjust = 1.5 + deltaV,
                    x = breaks, y = 0)) +
            annotation_custom(
                grob = segmentsGrob(
                    x0 = breaks, x1 = breaks,
                    y0 = unit(0, "npc"), y1 = tckSz,
                    gp = tickGp))
    }
    else if (side == 3 ||
             side == "t" ||
             regexpr("top", side) > 0) {
        breaks <- (trnsf(breaks) - min(rngs$x)) / diff(rngs$x)
        # Return
        plt +
            annotation_custom(
                grob = textGrob(label = labels, gp = gp, rot = rot,
                    hjust = 0.5 + deltaH, vjust = -0.7 + deltaV,
                    x = breaks, y = 1)) +
            annotation_custom(
                grob = segmentsGrob(
                    x0 = breaks, x1 = breaks,
                    y0 = unit(1, "npc"), y1 = unit(1, "npc") - tckSz,
                    gp = tickGp))
    }
    else if (side == 2 ||
             side == "l" ||
             regexpr("lef", side) > 0) {
        breaks <- (trnsf(breaks) - min(rngs$y)) / diff(rngs$y)
        # Return
        plt +
            annotation_custom(
                grob = textGrob(label = labels, gp = gp, rot = rot,
                    hjust = 1.5 + deltaH, vjust = 0.5 + deltaV,
                    x = 0, y = breaks)) +
            annotation_custom(
                grob = segmentsGrob(
                    x0 = unit(0, "npc"), x1 = tckSz,
                    y0 = breaks, y1 = breaks,
                    gp = tickGp))
    }
    else if (side == 4 ||
             side == "r" ||
             regexpr("rig", side) > 0) {
        breaks <- (trnsf(breaks) - min(rngs$y)) / diff(rngs$y)
        # Return
        plt +
            annotation_custom(
                grob = textGrob(label = labels, gp = gp, rot = rot,
                    hjust = -0.4 + deltaH, vjust = 0.5 + deltaV,
                    x = 1, y = breaks)) +
            annotation_custom(
                grob = segmentsGrob(
                    x0 = unit(1, "npc"), x1 = unit(1, "npc") - tckSz,
                    y0 = breaks, y1 = breaks,
                    gp = tickGp))
    }
    else
        stop(sprintf("Unknown axis %s", as.character(side)))
}