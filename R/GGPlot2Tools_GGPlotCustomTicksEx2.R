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


utils::globalVariables(c("b", "lbl", "Trans"))
#' @title GGPlotCustomTicksEx
#' @description
#' Creates custom ticks with labels. Requires a finished ggproto object.
#' Can be piped with %>%.
#' @param plt Target plot.
#' @param side Indicates axis. Can be either \code{1,2,3,4},
#' \code{"b", "l", "t", "r"}, or strings containing
#' \code{"bot", "lef", "top", "rig"} substring.
#' @param breaks Breaks in the axis scale.
#' @param labels Respective text labels.
#' @param tckSz Tick size (in units).
#' @param trnsf Inverse trnsformation function. Used to handle secondary axes.
#' @param gp A \code{gpar} set of parameters passed to the text methods.
#' Affects rendering of the labels.
#' @param rot Rotation of labels, deg.
#' @param offset Tick label offset from the axis.
#' @param just Overrides default justification.
#' @param tickGp \code{gpar} that controls ticks.
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid textGrob segmentsGrob
#' @importFrom stringr str_detect regex
#' @importFrom rlang eval_tidy quo abort
#' @export
GGPlotCustomTicksEx2 <- function(plt, side, breaks, labels,
                                trnsf = identity,
                                tckSz = unit(7.5, "pt"),
                                gp = gpar(),
                                rot = 0,
                                offset = unit(0.15, "cm"),
                                just = NULL,
                                tickGp = gpar()) {
    lifecycle::deprecate_warn("0.6.3", "RLibs::GGPlotCustomTicksEx2()")
    if (vec_is_integerish(side))
        sideId <- side[1]
    else if (vec_is(side, character()))
        sideId <- which(
            str_detect(c("top", "right", "bottom", "left"),
                regex("\\b" %&% side[1], ignore_case = TRUE)))
    else
        rlang::abort(paste0("`side` should be either integer ",
        "or string name of one of the sides."), "RLibs_invalid_arg")

    if (vec_is_empty(sideId))
        rlang::abort("Invalid `side` value provided", "RLibs_invalid_arg")

    if (!(is.unit(tckSz)))
        tckSz <- unit(tckSz, "npc")

    GetScale <- function(side) {
        trnsf <- plt$scales$scales %>%
            keep(~.x$position %==% side &&
                str_detect(.x$scale_name, "position")) %??% NULL %>%
            extract2(1)
    }

    if (!missing(breaks) && missing(labels))
        labels <- vec_recycle("", len(breaks))

    if (len(breaks) %!==% len(labels) && len(labels) > 1)
        rlang::abort("Length of [breaks] and [labels] should be equal.", "RLibs_invalid_arg")

    if (len(labels) %==% 1L)
        labels <- vec_recycle(labels[1], len(breaks))

    if (vec_is_empty(labels))
        labels <- vec_recycle("", len(breaks))

    template <- tibble(
        Side = cc("t", "r", "b", "l"),
        Full = cc("top", "right", "bottom", "left"),
        x = list(unit(0.5, "npc"), unit(1, "npc") + offset,
                 unit(0.5, "npc"), unit(0, "npc") - offset),
        y = list(unit(1, "npc") + offset, unit(0.5, "npc"),
                 unit(0, "npc") - offset, unit(0.5, "npc")),
        Just = if (is_null(just)) cc("bottom", "left", "top", "right")
            else rep(just, 4),
        xmin = list(quo(b), - Inf, quo(b), Inf),
        xmax = list(quo(b), Inf, quo(b), -Inf),
        ymin = list(Inf, quo(b), - Inf, quo(b)),
        ymax = list(-Inf, quo(b), Inf, quo(b)),
        x0 = list(unit(0, "npc"), unit(1, "npc"),
            unit(0, "npc"), unit(0, "npc")),
        x1 = list(unit(1, "npc"), unit(1, "npc") - tckSz,
                  unit(1, "npc"), tckSz),
        y0 = list(unit(1, "npc"), unit(0, "npc"),
            unit(0, "npc"), unit(0, "npc")),
        y1 = list(unit(1, "npc") - tckSz, unit(1, "npc"),
                  tckSz, unit(1, "npc")),
        Trans = list(GetScale("bottom")$trans,
            GetScale("left")$trans)[cc(1, 2, 1, 2)],
        BreaksTrans = list(
            function(x) EvalTrans(Trans[[1]], EvalTrans(trnsf, x)),
            function(x) EvalTrans(Trans[[2]], EvalTrans(trnsf, x)),
            function(x) EvalTrans(Trans[[3]], x),
            function(x) EvalTrans(Trans[[4]], x)))

    selection <- template %>% slice(sideId)

    plt +
    map2(selection$BreaksTrans[[1]](breaks), labels, function(b, lbl) {
        xmin <- eval_tidy(selection$xmin[[1]], list(b = b))
        xmax <- eval_tidy(selection$xmax[[1]], list(b = b))
        ymin <- eval_tidy(selection$ymin[[1]], list(b = b))
        ymax <- eval_tidy(selection$ymax[[1]], list(b = b))

        result <- list(
            annotation_custom(
                grob = segmentsGrob(
                    x0 = selection$x0[[1]],
                    x1 = selection$x1[[1]],
                    y0 = selection$y0[[1]],
                    y1 = selection$y1[[1]],
                    gp = tickGp),
                xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax))

        if (nzchar(lbl))
            result <- append(result, list(
                annotation_custom(
                    grob = textGrob(
                        label = lbl, gp = gp, rot = rot,
                        x = selection$x[[1]],
                        y = selection$y[[1]],
                        just = selection$Just),
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
            ymax = ymax)))

        result
    })
}