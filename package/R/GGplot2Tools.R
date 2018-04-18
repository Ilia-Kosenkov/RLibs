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

#' @export
Lookup <- function(object, ...) UseMethod("Lookup")

#' @import grid
Lookup.gtable <- function(grob, ...) {
    what <- unlist(list(...))
    inds <- sapply(what, function(i) which(grob$layout$name == i))
    return(list(GrobDesc = grob$layout[inds, ], Index = inds))
}

#' @export
GetGrob <- function(grob, ...) {
    inds <- Lookup(grob, ...)$Index
    if (all(is.na(inds)))
        return(NULL)
    if (length(inds) == 1)
        return(grob$grobs[[inds]])
    return(grob$grobs[inds])
}

#' @export
IsGrobNull <- function(...) {
    args <- list(...)
    return(sapply(args, function(x) "zeroGrob" %in% class(x), simplify = TRUE))
}

#' @export
#' @import grid
GetMargins <- function(grob, type = c("inner", "outer")) {

    if (is.null(grob) || !("gDesc" %in% class(grob)))
        stop("`grob` is invalid.")
    worker <- function(txtX, txtY) {
        ax.lr <- Lookup(grob, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb <- Lookup(grob, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr <- ax.lr$GrobDesc$l
        inds.tb <- ax.tb$GrobDesc$t
        return(list(t = grob$heights[inds.tb[1]], r = grob$widths[inds.lr[2]],
            b = grob$heights[inds.tb[2]], l = grob$widths[inds.lr[1]]))
    }

    if (!all(type %in% c("inner", "outer")))
        stop("Wrong `type`.")
    if (all(type == "inner"))
        return(worker("axis", "axis"))
    if (all(type == "outer"))
        return(worker("ylab", "xlab"))
    if (all(type %in% c("inner", "outer")))
        return(list(Inner = worker("axis", "axis"), Outer = worker("ylab", "xlab")))

}

#' @export
#' @import grid
SetMargins <- function(grob, type, margins) {
    worker <- function(txtX, txtY) {
        ax.lr <- Lookup(grob, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb <- Lookup(grob, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr <- ax.lr$GrobDesc$l
        inds.tb <- ax.tb$GrobDesc$t
        return(list(t = inds.tb[1], r = inds.lr[2],
            b = inds.tb[2], l = inds.lr[1]))

    }

    if (all(type == "inner"))
        inds <- worker("axis", "axis")
    else if (all(type == "outer"))
        inds <- worker("ylab", "xlab")
    else
        stop("Wrong `type`.")

    if (!is.null(margins$t))
        grob$heights[inds$t] <- margins$t
    if (!is.null(margins$b))
        grob$heights[inds$b] <- margins$b
    if (!is.null(margins$l))
        grob$widths[inds$l] <- margins$l
    if (!is.null(margins$r))
        grob$widths[inds$r] <- margins$r

    return(grob)
}

#' @export
#' @import ggplot2
DefaultTheme <- function() {
    return(theme_bw() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                theme(axis.ticks.length = unit(-3.5, "pt")) +
                theme(axis.text.x =
                    element_text(size = 10, margin = margin(t = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y =
                    element_text(size = 10, margin = margin(r = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y.right =
                    element_text(size = 10, margin = margin(l = unit(10, "pt")), colour = "#000000")))
}

#' @export
#' @import ggplot2
scale_x_custom <- function(type = "continuous", breaks, except = c(), ...) {

    params <- list(...)
    inds <- which(!breaks %in% except)
    params$breaks <- breaks[inds]
    params$labels <- rep("", length(inds))

    do.call(paste0("scale_x_", type), args = params)
}

#' @export
#' @import ggplot2
scale_y_custom <- function(type = "continuous", breaks, ...) {

    params <- list(...)
    params$breaks <- breaks
    params$labels <- rep("", length(breaks))

    do.call(paste0("scale_y_", type), args = params)
}

#' @export
#' @import ggplot2
GGPlotGetRange <- function(plt) {
    compact(
            setNames(
                ggplot_build(plt)$layout$panel_params[[1]][
                    c("x.range", "y.range", "x.sec.range", "y.sec.range")],
                c("x", "y", "x2", "y2")))
}

GGPlotCustomTicks <- function(plt, side, breaks, labels, relSz, delta = 0) {

}

#' @export
#' @import ggplot2 foreach
GGCustomLargeTicks <- function(side, breaks, labels,
                               start, end, delta = 0) {
    lb <- NULL
    br <- NULL

    if (length(breaks) != length(labels) && length(labels) > 1)
        stop("Length of [breaks] and [labels] should be equal.")
    if (length(labels) == 1)
        labels <- rep(labels, length(breaks))

    if (side == 1 ||
        side == "b" ||
        regexpr("bot", side) > 0) {
        append(
            foreach(br = breaks,
                    lb = labels) %do% {
                annotation_custom(
                            grob = textGrob(label = lb,
                                hjust = 0.5, vjust = 1.5 + delta),
                            xmin = br, xmax = br,
                            ymin = -Inf, ymax = -Inf)
            },
            annotate(geom = "segment",
                            x = breaks, xend = breaks,
                            y = start, yend = end))
    }
    else if (side == 3 ||
             side == "t" ||
             regexpr("top", side) > 0) {
        append(
            foreach(br = breaks,
                    lb = labels) %do% {
                annotation_custom(
                            grob = textGrob(label = lb,
                                hjust = 0.5, vjust = -0.7 + delta),
                            xmin = br, xmax = br,
                            ymin = Inf, ymax = Inf)
            },
            annotate(geom = "segment",
                            x = breaks, xend = breaks,
                            y = end, yend = start))
    }
    else if (side == 2 ||
             side == "l" ||
             regexpr("lef", side) > 0) {
        append(
            foreach(br = breaks,
                    lb = labels) %do% {
                annotation_custom(
                            grob = textGrob(label = lb,
                                hjust = 1.5 + delta, vjust = 0.5),
                            xmin = -Inf, xmax = -Inf,
                            ymin = br, ymax = br)
            },
            annotate(geom = "segment",
                            x = start, xend = end,
                            y = breaks, yend = breaks))
    }
    else if (side == 4 ||
             side == "r" ||
             regexpr("rig", side) > 0) {
        append(
            foreach(br = breaks,
                    lb = labels) %do% {
            annotation_custom(
                            grob = textGrob(label = lb,
                                hjust = -0.4 + delta, vjust = 0.5),
                            xmin = Inf, xmax = Inf,
                            ymin = br, ymax = br)
            },
            annotate(geom = "segment",
                            x = end, xend = start,
                            y = breaks, yend = breaks))
    }
    else
        stop(sprintf("Unknown axis %s", as.character(side)))
}