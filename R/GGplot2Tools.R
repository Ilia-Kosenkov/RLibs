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

utils::globalVariables(c(".", "Key", "Val",
    "lbl", "xmn", "xmx",
    "g", "ymn", "ymx",
    "hjst", "vjst", "rt"))

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

    if (is_null(grob) || !inherits_any(grob, "dGdesc"))
        rlang::abort("`grob` is invalid.", "RLibs_invalid_arg")
    worker <- function(txtX, txtY) {
        ax.lr <- Lookup(grob, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb <- Lookup(grob, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr <- ax.lr$GrobDesc$l
        inds.tb <- ax.tb$GrobDesc$t
        return(list(t = grob$heights[inds.tb[1]], r = grob$widths[inds.lr[2]],
            b = grob$heights[inds.tb[2]], l = grob$widths[inds.lr[1]]))
    }

    if (!all(type %vec_in% c("inner", "outer")))
        stop("Wrong `type`.")
    if (type %===% "inner")
        return(worker("axis", "axis"))
    if (type %===% "outer")
        return(worker("ylab", "xlab"))
    if (all(type %vec_in% c("inner", "outer")))
        return(list(Inner = worker("axis", "axis"),
            Outer = worker("ylab", "xlab")))

}

#' @title SetMargins
#' @param grob Grob to process.
#' @param type Type of the margin.
#' Allowed values: \code{"inner"} and \code{"outer"}.
#' @param margins Values of margins.
#' @return Modified grob. Can be piped e.g. with \code{dplyr::`\%>\%`}.
#' @export
SetMargins <- function(grob, type, margins) {
    lifecycle::deprecate_warn("0.7.13", "RLibs::SetMargins()", "RLibs::GrobMarginSet()")
    worker <- function(g, txtX, txtY) {
        ax.lr <- Lookup(g, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb <- Lookup(g, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr <- ax.lr$GrobDesc$l
        inds.tb <- ax.tb$GrobDesc$t
        return(list(t = inds.tb[1], r = inds.lr[2],
            b = inds.tb[2], l = inds.lr[1]))

    }

    isUnit <- is.unit(margins) ||
        (vec_is(margins, list()) && every(margins, is.unit))


    if (!isUnit)
        rlang::abort("`margins` should be at least of class `unit`.", "RLibs_invalid_arg")

    if (inherits_any(margins, "margin"))
        rawMargins <- list(
            t = margins[1],
            r = margins[2],
            b = margins[3],
            l = margins[4])
    else
        rawMargins <- margins


    if (type %===% "inner")
        inds <- worker(grob, "axis", "axis")
    else if (type %===% "outer")
        inds <- worker(grob, "ylab", "xlab")
    else
        rlang::abort("Wrong `type`.", "RLibs_invalid_arg")


    if (!is_null(rawMargins$t))
        grob$heights[inds$t] <- rawMargins$t
    if (!is_null(rawMargins$b))
        grob$heights[inds$b] <- rawMargins$b
    if (!is_null(rawMargins$l))
        grob$widths[inds$l] <- rawMargins$l
    if (!is_null(rawMargins$r))
        grob$widths[inds$r] <- rawMargins$r

    return(grob)
}

#' @title DefaultTheme
#' @description Generates default theme used in scientific publications.
#' @param ticks Controls the tick size (and direction).
#' @param textSz Text font size by default.
#' @param titleSz Title (labels') font size by default.
#' @return A theme object.
#' @export
#' @import ggplot2
DefaultTheme <- function(
    ticks = unit(-3.5, "pt"),
    textSz = 10,
    titleSz = 15) {
    return(theme_bw() +
                theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
                theme(axis.ticks.length = ticks) +
                theme(axis.text.x =
                    element_text(size = textSz,
                        margin = margin(t = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.text.y =
                        element_text(size = textSz,
                        margin = margin(r = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.text.y.right =
                        element_text(size = textSz,
                        margin = margin(l = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.text.x.top =
                        element_text(size = textSz,
                        margin = margin(l = unit(10, "pt")),
                        colour = "#000000")) +
                #------------------------------------#
                theme(axis.title.x =
                        element_text(size = titleSz,
                        margin = margin(t = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.title.y =
                        element_text(size = titleSz,
                        margin = margin(r = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.title.y.right =
                        element_text(size = titleSz,
                        margin = margin(l = unit(10, "pt")),
                        colour = "#000000")) +
                theme(axis.title.x.top =
                        element_text(size = titleSz,
                        margin = margin(l = unit(10, "pt")),
                        colour = "#000000")))
}

#' @export
#' @import ggplot2
scale_x_custom <- function(...) {
    lifecycle::deprecate_stop("0.6.3", "RLibs::scale_x_custom()")
}

#' @export
#' @import ggplot2
scale_y_custom <- function(...) {
    lifecycle::deprecate_stop("0.6.3", "RLibs::scale_y_custom()")
}

#' @title GGPlotGetRange
#' @description Builds provided \code{ggplot2} object and determines
#' actual ranges of all of its axes.
#' @param plt Plot to measure.
#' @return A \code{list()} of ranges with names \code{"x", "y", "x2", "y2"}.
#' Some values (like *2) can be \code{NULL} if no respective axis is present.
#' @export
GGPlotGetRange <- function(plt) {
    result <- set_names(
                ggplot_build(plt)$layout$panel_params[[1]][
                    c("x.range", "y.range", "x.sec.range", "y.sec.range")],
                c("x", "y", "x2", "y2"))
    result <- result[!sapply(result, is.null)]
}




#' @title GGPlot2Grob
#' @description
#' Converts \code{ggplot2} objects into \code{gTables}
#' and sets up margins.
#' @param plots Plots to convert.
#' @param innerMar If present, sets inner margins.
#' @param outerMar If present, sets outer margins.
#' @param clip If \code{TRUE}, clips everything outside of the plot ares
#' (affects custom tick labels and so on), otherwise, allows contents outside 
#' of the plot area.
#' @return A collection of \code{grobs}.
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @export
GGPlot2Grob <- function(plots, innerMar, outerMar, clip = FALSE) {
    warning("This function is obsolete. Use [GGPlot2GrobEx].")
    worker <- function(p, setInner, setOuter) {
        grob <- ggplot_gtable(ggplot_build(p))

        grob$layout$clip[grob$layout$name == "panel"] <-
            ifelse(clip, "on", "off")

        if (setInner)
            grob <- SetMargins(grob, "inner", innerMar)
        if (setOuter)
            grob <- SetMargins(grob, "outer", outerMar)

        return(grob)
    }

    setInner <- !missing(innerMar)
    setOuter <- !missing(outerMar)

    if (vec_is(plots, list()))
        return(plots %>%
            map(~worker(.x, setInner, setOuter)))
    else
        worker(plots, setInner, setOuter)
}

#' @title GrobPlot
#' @description
#' Plots provide \code{gTable} objects, one per page
#' @param grobs Figures to plot.
#' @param noNewPageDevList Names of devices that produce empty page
#' if \code{grid::grid.newage()} is called before first call
#' of \code{grid::grid.draw(...)}.
#' @importFrom grid grid.newpage grid.draw
#' @importFrom dplyr %>%
#' @importFrom purrr every
#' @importFrom rlang !!
#' @export
GrobPlot <- function(grobs, noNewPageDevList = c("pdf")) {
    # Typical classes of passed grob
    grobClassIds <- c("gtable", "gTree", "grob", "gDesc")
    # If there is a match, then received one grob (not list of grobs)

    isStandAloneGrob <- inherits_all(grobs, grobClassIds)

    # If current device is part of the list, than
    # no first page should be drawn.
    # First call to [grid.newpage] produces empty page.
    drawNewPage <- !any(sapply(noNewPageDevList, grepl,
        x = names(dev.cur()), ignore.case = TRUE))

    # Case of a single plot
    if (isStandAloneGrob) {
        if (drawNewPage)
            grid.newpage()
        grid.draw(grobs)
    } else
        for (ind in seq_int_len(length(grobs))) {
            # If first new page should be drawn or
            # it is not the first plot of a collection.
            if (ind != 1L || drawNewPage)
                grid.newpage()

            grid.draw(grobs[[ind]])
    }
}

#' @title TrimLineData
#' @param .data Input table.
#' @param .var_x x variable.
#' @param .var_y y variable.
#' @param lim Limits.
#' @importFrom rlang enquo quo_name !! := sym
#' @importFrom dplyr %>% mutate slice n filter
#' @export
TrimLineData <- function(.data, .var_x, .var_y, lim) {
    x <- quo_name(enquo(.var_x))
    x2 <- x %>% paste0("2")
    y <- quo_name(enquo(.var_y))
    y2 <- y %>% paste0("2")
    y_c <- y %>% paste0("_clamped")

    isMissing <- missing(lim)
    .data %>%
        mutate(!!x2 := (!!sym(x))[c(2:n(), n())]) %>%
        mutate(!!y2 := (!!sym(y))[c(2:n(), n())]) %>%
        slice(-n()) %>% {
            if (isMissing)
                .
            else
                filter(., !!sym(y2) <= lim[2]) %>%
                filter(!!sym(y2) >= lim[1]) %>%
                filter(!!sym(y) <= lim[2]) %>%
                filter(!!sym(y) >= lim[1])
            }
}


#' @title AsSegments
#' @param .data Input table.
#' @param ... Columns to process.
#' @param suffix Suffix to the end value column names.
#' @return Modified table.
#' @importFrom rlang enquos !! :=
#' @importFrom dplyr %>% mutate n
#' @importFrom purrr map reduce
#' @export
AsSegments <- function(.data, ..., suffix = "_end") {
    .dots <- enquos(...)
    .names <- .dots %>%
        map(quo_squash)
    .data %>% {
        reduce(.names, .init = .,
               .f = function(x, y) {
                   mutate(x, !!paste0(a_ch(y), suffix) :=
                        (!!y)[c(2:n(), n())])
               })
    } %>%
    slice(-n())
}



#' @title GGCustomTextAnnotation
#' @param labels Labels.
#' @param x,y X and Y coordinates of labels
#' @param xmin,xmax X coordinates of labels.
#' @param ymin,ymax Y coordinates of labels.
#' @param vjust Vertical justification.
#' @param hjust Horizontal justification.
#' @param rot Rotation angle.
#' @param gp Additional graphical parameters.
#' @return List of \code{annotation_custom} objects.
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid textGrob gpar
#' @export
GGCustomTextAnnotation <- function(labels, x, y,
                                   xmin = x, xmax = x,
                                   ymin = y, ymax = y,
                                   vjust = 0, hjust = 0,
                                   rot = 0,
                                   gp = list(gpar())) {
    n <- length(labels)

    if (length(xmin) == 1L && n != 1L)
        xmin <- rep(xmin[1], n)
    if (length(xmax) == 1L && n != 1L)
        xmax <- rep(xmax[1], n)
    if (length(ymin) == 1L && n != 1L)
        ymin <- rep(ymin[1], n)
    if (length(ymax) == 1L && n != 1L)
        ymax <- rep(ymax[1], n)
    if (length(vjust) == 1L && n != 1L)
        vjust <- rep(vjust[1], n)
    if (length(hjust) == 1L && n != 1L)
        hjust <- rep(hjust[1], n)
    if (length(rot) == 1L && n != 1L)
        rot <- rep(rot[1], n)
    if (class(gp) == "gpar")
        gp <- rep(list(gp), n)
    else if (length(gp) == 1L && n != 1L)
        gp <- rep(gp[1], n)

    pmap(list(lbl = labels,
            xmn = xmin, xmx = xmax,
            ymn = ymin, ymx = ymax,
            vjst = vjust,
            hjst = hjust,
            rt = rot,
            g = gp),
        function(lbl, xmn, xmx, ymn, ymx, vjst, hjst, rt, g) {
                annotation_custom(textGrob(lbl,
                        hjust = hjst,
                        vjust = vjst,
                        rot = rt,
                        gp = g),
                    xmn, xmx, ymn, ymx)
            })
}


#' GrobSizeGet
#'
#' @param grob Input grob.
#' @param name Name of the grob.
#' @param id Alternatively, id of the grob.
#' Useful when there are multiple grobs of the same name.
#' @return Size of the grob.
#' @export
#'

GrobSizeGet <- function(grob, name, id) {
    if (!missing(name))
        id <- (grob$layout$name %in% name)

    layout <- grob$layout[id, ]

    result <- list(Width = NULL, Height = NULL)

    if (nrow(layout) != 1)
        return(result)

    if (layout$t == layout$b)
        result$Height <- grob$heights[layout$t]

    if (layout$r == layout$l)
        result$Width <- grob$widths[layout$r]

    return(result)
}

#' GrobSizeSet
#'
#' @param grob Input grob.
#' @param name Name of the grob.
#' @param id Alternatively, id of the grob.
#' Useful when there are multiple grobs of the same name.
#' @param width Width to set.
#' @param height Height to set.
#' @return Modfied grob.
#' @export
#'

GrobSizeSet <- function(grob, name, width = NULL, height = NULL, id) {
    if (!missing(name))
        id <- (grob$layout$name %in% name)

    layout <- grob$layout[id, ]

    result <- list(Width = NULL, Height = NULL)

    if (nrow(layout) != 1)
        stop("Cannot identify a single grob using provided name/id.")

    if (!is.null(width)) {
        if (layout$r != layout$l)
            stop("The grob spans over multiple grid cells.")
        grob$widths[layout$r] <- width
    }
    if (!is.null(height)) {
        if (layout$t != layout$b)
            stop("The grob spans over multiple grid cells.")
        grob$heights[layout$t] <- height
    }

    invisible(grob)
}

#' GrobZero
#'
#' @param grob Grob collection.
#' @param name Name of grob to remove.
#'
#' @return Modified grob collection.
#' @export
#'

GrobZero <- function(grob, name) {
    inds <- which(grob$layout$name %in% name)

    grob$grobs %<>%
        map2(seq_len(length(.)),
             function(g, i) if (i %in% inds) zeroGrob() else g)

    return(grob)
}

#' GrobSetGaps
#'
#' @param grob Inout grob.
#' @param top Top gap.
#' @param right Right gap.
#' @param bottom Bottom gap.
#' @param left Left gap.
#' @param asList When not NULL, used instead of other parameters to set gaps.
#' @param asMargin When not NULL, used instead of other parameters to set gaps.
#' @return Grob.
#' @export
#'
GrobSetGaps <- function(grob,
    top = NULL, right = NULL, bottom = NULL, left = NULL,
    asList = NULL, asMargin = NULL) {

    if (!is.null(asMargin))
        asList <- list(
            "top" = asMargin[1],
            "right" = asMargin[2],
            "bottom" = asMargin[3],
            "left" = asMargin[4])

    if (all(is.null(asList)))
        asList <- list(top = top, right = right,
            bottom = bottom, left = left)

    if (!is.null(asList$top))
        grob$heights[1] <- asList$top
    if (!is.null(asList$right))
        grob$widths[length(grob$widths)] <- asList$right
    if (!is.null(asList$bottom))
        grob$heights[length(grob$heights)] <- asList$bottom
    if (!is.null(asList$left))
        grob$widths[1] <- asList$left

    return(grob)
}

#' GGPlot2GrobEx
#'
#' @param plots Input plots.
#' @param clip Clip flag.
#'
#' @return Colelction of grobs.
#'
#' @export
#'
GGPlot2GrobEx <- function(plots, clip = FALSE) {
    worker <- function(p) {
        grob <- ggplot_gtable(ggplot_build(p))

        grob$layout$clip[grob$layout$name == "panel"] <-
            ifelse(clip, "on", "off")

        return(grob)
    }


    if (vec_is(plots, list()))
        return(map(plots, worker))
    else
        worker(plots)
    }

#' GGPlotPanelLabs
#'
#' @param p Input plots.
#' @param labels Labels to place on each plot, one per plot.
#' @param x X cordinates of the labels. Can be +-Inf.
#' @param y Y cordinates of the labels. Can be +-Inf.
#' @param hjust Horizontal justification. + moves right.
#' @param vjust Vertical justification. + moves down.
#' @param gp Text parameters
#'
#' @return Modifed plots.
#' @export
GGPlotPanelLabs <- function(p, labels = "X",
                            x = Inf, y = Inf,
                            hjust = 1, vjust = 1, gp = gpar()) {
    if (vec_is(p, list()))
        n <- len(p)
    else {
        p <- p + GGCustomTextAnnotation(labels[1],
            x[1], y[1], vjust = vjust[1], hjust = hjust[1], gp = gp[1])

        return (p)
    }


    FillIfSmaller <- function(val) {
        if (length(val) != n)
            val <- rep(val[1], n)

        return(val)
        }

    labels %<>% FillIfSmaller
    x %<>% FillIfSmaller
    y %<>% FillIfSmaller
    hjust %<>% FillIfSmaller
    vjust %<>% FillIfSmaller
    if (inherits_any(gp, "gpar"))
        gp <- rep(list(gp), n)

    lLabs <- NULL
    lX <- NULL
    lY <- NULL
    lHjust <- NULL
    lVjust <- NULL
    lGp <- NULL
    pmap(list(x = p, lLabs = labels, lX = x, lY = y,
                   lHjust = hjust, lVjust = vjust,
                   lGp = gp), function(x, lLabs, lX, lY, lHjust, lVjust, lGp) {
                       x + GGCustomTextAnnotation(lLabs,
                        lX, lY, vjust = lVjust, hjust = lHjust, gp = lGp)
                   })

}
