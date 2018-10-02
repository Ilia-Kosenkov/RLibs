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
    "hjst", "vjst"))

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
#' @import grid
#' @importFrom purrr map
#' @importFrom dplyr %>%
SetMargins <- function(grob, type, margins) {
    worker <- function(g, txtX, txtY) {
        ax.lr <- Lookup(g, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb <- Lookup(g, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr <- ax.lr$GrobDesc$l
        inds.tb <- ax.tb$GrobDesc$t
        return(list(t = inds.tb[1], r = inds.lr[2],
            b = inds.tb[2], l = inds.lr[1]))

    }

    isUnit <- (margins %is% unit) ||
        ((margins %is% list) &&
            (margins %>% every(~.x %is% unit)))


    if (!isUnit)
        stop("`margins` should be at least of class `unit`.")

    if (margins %is% margin)
        rawMargins <- list(
            t = margins[1],
            r = margins[2],
            b = margins[3],
            l = margins[4])
    else
        rawMargins <- margins


    if (all(type == "inner"))
        inds <- worker(grob, "axis", "axis")
    else if (all(type == "outer"))
        inds <- worker(grob, "ylab", "xlab")
    else
        stop("Wrong `type`.")


    if (!is.null(rawMargins$t))
        grob$heights[inds$t] <- rawMargins$t
    if (!is.null(rawMargins$b))
        grob$heights[inds$b] <- rawMargins$b
    if (!is.null(rawMargins$l))
        grob$widths[inds$l] <- rawMargins$l
    if (!is.null(rawMargins$r))
        grob$widths[inds$r] <- rawMargins$r

    return(grob)
}

#' @title DefaultTheme
#' @description Generates default theme used in scientific publications.
#' @return A theme object.
#' @export
#' @import ggplot2
DefaultTheme <- function() {
    return(theme_bw() +
                theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
                theme(axis.ticks.length = unit(-3.5, "pt")) +
                theme(axis.text.x =
                    element_text(size = 10,
                        margin = margin(t = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y =
                    element_text(size = 10,
                        margin = margin(r = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y.right =
                    element_text(size = 10,
                        margin = margin(l = unit(10, "pt")), colour = "#000000")))
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

#' @title GGPlotGetRange
#' @description Builds provided \code{ggplot2} object and determines
#' actual ranges of all of its axes.
#' @param plt Plot to measure.
#' @return A \code{list()} of ranges with names \code{"x", "y", "x2", "y2"}.
#' Some values (like *2) can be \code{NULL} if no respective axis is present.
#' @export
#' @import ggplot2
#' @importFrom stats setNames
GGPlotGetRange <- function(plt) {
    result <- setNames(
                ggplot_build(plt)$layout$panel_params[[1]][
                    c("x.range", "y.range", "x.sec.range", "y.sec.range")],
                c("x", "y", "x2", "y2"))
    result <- result[!sapply(result, is.null)]
}

#' @title GenerateBreaks
#' @description
#' Generates large and small breaks within given range.
#' @param range Range of breaks (typically a scale of an axis).
#' @param largeStep Step for large breaks (the ones with labels).
#' @param smallStep If specified, used as step for small breaks.
#' @param ticks If \code{smallStep} is not specified, then \code{ticks} are used
#' to create small breaks.
#' @param op Used in combination with \code{ticks} as: per each large break produces
#' \code{op(ticks, break)} small breaks and then limits them to \code{range}
#' @return A list containing either $Large, $Small collections of breaks, or both.
#' @export
GenerateBreaks <- function(range, largeStep, smallStep, ticks, op = `*`) {
    result <- list()
    if (!missing(largeStep)) {
        temp <- largeStep *
            (ceiling(range[1] / largeStep):floor(range[2] / largeStep))
        result <- append(result, list(Large = temp))
    }
    if (!missing(smallStep)) {
        temp <- smallStep *
            (floor(range[1] / smallStep):ceiling(range[2] / smallStep))
        result <- append(result, list(Small = Within(temp, range)))
    }
    else if (!missing(ticks) && !missing(largeStep)) {
        sq <- c(min(result$Large) - largeStep,
                result$Large,
                max(result$Large) + largeStep)
        temp <- unlist(lapply(sq, function(x) op(ticks, x)))
        result <- append(result, list(Small = Within(temp, range)))
    }

    if (all(names(result) %in% c("Large", "Small")))
        result$Small <- result$Small[!result$Small %in% result$Large]

    return(result)
}

#' @title GGPlotCustomTicls
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
                    y0 = 0, y1 = tckSz,
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
                    y0 = 1, y1 = 1 - tckSz,
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
                    x0 = 0, x1 = tckSz,
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
                    x0 = 1, x1 = 1 - tckSz,
                    y0 = breaks, y1 = breaks,
                    gp = tickGp))
    }
    else
        stop(sprintf("Unknown axis %s", as.character(side)))
}

#' @export
#' @import ggplot2 foreach
GGCustomLargeTicks <- function(...)
    stop("GGCustomLargeTicks is deprecated. Use GGPlotCustomTicks")


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

    if (plots %is% list)
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

    isStandAloneGrob <- grobClassIds %>%
        every(~grobs %is% !!.x)

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

#' @title TrimRibbonData
#' @param .data Input table.
#' @param x \code{x} column.
#' @param y \code{y} column.
#' @param xlim \code{x} limits.
#' @param ylim \code{y} limits.
#' @param lwr Lower \code{y} limit of ribbon.
#' @param upp Upper \code{y} limit of ribbon
#' @param ... Additional columns to preserve.
#' @return Table ready to be plotted.
#' @importFrom rlang enquo quo_squash := !! is_empty
#' @importFrom purrr map reduce
#' @importFrom magrittr extract is_less_than is_greater_than %<>%
#' @import dplyr
#' @export
TrimRibbonData <- function(.data, x, y, xlim, ylim, lwr, upp, ...) {
    .dots <- enquos(...) %>%
        map(quo_squash)

    nm <- list()
    nm$x <- quo_squash(enquo(x))
    nm$y <- quo_squash(enquo(y))
    nm$lwr <- quo_squash(enquo(lwr))
    nm$upp <- quo_squash(enquo(upp))

    .data %<>%
        arrange(!!nm$x)
    xInds <- .data %>%
        mutate(!!as.name("w") := !!nm$x >= xlim[1] & !!nm$x <= xlim[2]) %>%
        pull("w") %>%
        which %>%
        c(min(.) - 1, max(.) + 1) %>%
        Clamp(1, nrow(.data)) %>%
        unique %>%
        Order

    pltData <- .data %>%
        slice(xInds)
    m <- nrow(pltData)

    if (pltData %>% extract(1, a_ch(nm$x)) %>%
            is_less_than(xlim[1])) {
        arg <- pltData %>% extract(1:2, a_ch(nm$x))
        val <- pltData %>% extract(1:2, a_ch(nm$y))
        lVal <- pltData %>% extract(1:2, a_ch(nm$lwr))
        uVal <- pltData %>% extract(1:2, a_ch(nm$upp))

        y0 <- Lin(xlim[1], arg, val)
        l0 <- Lin(xlim[1], arg, lVal)
        u0 <- Lin(xlim[1], arg, uVal)

        pltData %<>%
            mutate(!!nm$x := replace(!!nm$x, row_number() == 1L, xlim[1])) %>%
            mutate(!!nm$y := replace(!!nm$y, row_number() == 1L, y0)) %>%
            mutate(!!nm$lwr := replace(!!nm$lwr, row_number() == 1L, l0)) %>%
            mutate(!!nm$upp := replace(!!nm$upp, row_number() == 1L, u0))
    }
    if (pltData %>% extract(m, a_ch(nm$x)) %>%
            is_greater_than(xlim[2])) {
        arg <- pltData %>% extract(m - 1:0, a_ch(nm$x))
        val <- pltData %>% extract(m - 1:0, a_ch(nm$y))
        lVal <- pltData %>% extract(m - 1:0, a_ch(nm$lwr))
        uVal <- pltData %>% extract(m - 1:0, a_ch(nm$upp))

        y0 <- Lin(xlim[2], arg, val)
        l0 <- Lin(xlim[2], arg, lVal)
        u0 <- Lin(xlim[2], arg, uVal)

        pltData %<>%
            mutate(!!nm$x := replace(!!nm$x, row_number() == m, xlim[2])) %>%
            mutate(!!nm$y := replace(!!nm$y, row_number() == m, y0)) %>%
            mutate(!!nm$lwr := replace(!!nm$lwr, row_number() == m, l0)) %>%
            mutate(!!nm$upp := replace(!!nm$upp, row_number() == m, u0))
    }

    Interpolate <- function(.dt, name, lim) {

        .dt %>%
        pull(a_ch(name)) %>% {
            ((.[1:(length(.) - 1)] < lim) &
             (.[2:length(.)] > lim)) |
            ((.[1:(length(.) - 1)] > lim) &
             (.[2:length(.)] < lim))
        } %>%
        which %>% {
            if (is_empty(.))
                .dt
            else {
                map(., ~ .x + c(0, 1)) %>%
                map(function(x) {
                    vals <- c(
                        Lin(lim,
                            extract(.dt, x, a_ch(name)),
                            extract(.dt, x, a_ch(nm$x))),
                        Lin(lim,
                            extract(.dt, x, a_ch(name)),
                            extract(.dt, x, a_ch(nm$y))),
                        Lin(lim,
                            extract(.dt, x, a_ch(name)),
                            extract(.dt, x, a_ch(nm$lwr))),
                        Lin(lim,
                            extract(.dt, x, a_ch(name)),
                            extract(.dt, x, a_ch(nm$upp))))

                    tibble(!!nm$x := vals[1], !!nm$y := vals[2],
                       !!nm$lwr := vals[3], !!nm$upp := vals[4]) %>%
                    mutate(!!name := lim) %>% {
                        reduce(.dots, function(d, nm)
                                    mutate(d, !!nm :=
                                        extract(.dt, x[1], a_ch(nm)) %>%
                                        unlist),
                                .init = .)
                    }
                }) %>%
                reduce(bind_rows) %>%
                bind_rows(.dt) %>%
                arrange(!!nm$x)
            }
        }
    }

    pltData %>%
        Interpolate(nm$y, ylim[1]) %>%
        Interpolate(nm$lwr, ylim[1]) %>%
        Interpolate(nm$upp, ylim[1]) %>%
        Interpolate(nm$y, ylim[2]) %>%
        Interpolate(nm$lwr, ylim[2]) %>%
        Interpolate(nm$upp, ylim[2]) %>%
        Clamp(!!nm$lwr, ylim) %>%
        Clamp(!!nm$upp, ylim) %>%
        mutate(!!paste0(a_ch(nm$y), "_clamped") := !!nm$y) %>%
        Clamp(!!sym(paste0(a_ch(nm$y), "_clamped")), ylim) %>%
        arrange(!!nm$x) %>%
        slice(UniqueWhichTol(!!nm$x))
}

#' @title TrimLineData
#' @param .data Input table.
#' @param .var_x x variable.
#' @param .var_y y variable.
#' @param lim Limits.
#' @importFrom rlang enquo quo_name !! := sym
#' @import dplyr
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

#' @title ClipXY
#' @param .dt Input table.
#' @param x \code{x} column name.
#' @param y code{y} column name.
#' @param xlim Limits on \code{x}.
#' @param ylim Limits on \code{y}.
#' @param ... Additional columns to preserve.
#' @importFrom rlang quo_squash enquo enquos !! := sym
#' @importFrom magrittr extract
#' @importFrom purrr map reduce
#' @import dplyr
#' @export
ClipXY <- function(.dt, x, y, xlim, ylim, ...) {
    .dots <- enquos(...) %>% map(quo_squash)
    nx <- quo_squash(enquo(x))
    ny <- quo_squash(enquo(y))

    .dt %<>%
        arrange(!!nx)

    process <- function(name, val) {
        .dt %>%
        pull(a_ch(name)) %>%
        BetweenWhich(val) %>% {
            if (all(is.na(.)))
                tibble(!!nx := numeric(0),
                       !!ny := numeric(0))
            else
                map(., ~ tibble(
                            !!nx := Lin(val,
                                .dt %>% extract(.x, a_ch(name)),
                                .dt %>% extract(.x, a_ch(nx))),
                            !!ny := Lin(val,
                                .dt %>% extract(.x, a_ch(name)),
                                .dt %>% extract(.x, a_ch(ny)))) %>% {
                reduce(.dots, function(acc, nm)
                                    acc %>%
                                        mutate(!!nm :=
                                            extract(.dt, .x[1], a_ch(nm)) %>%
                                            unlist),
                                   .init = .)
            })
        } %>%
        reduce(bind_rows)
    }

    x_l <- process(nx, xlim[1])
    x_u <- process(nx, xlim[2])
    y_l <- process(ny, ylim[1])
    y_u <- process(ny, ylim[2])

    nx2 <- nx %>% a_ch %>% paste0("2") %>% sym
    ny2 <- ny %>% a_ch %>% paste0("2") %>% sym

    .dt %>%
        bind_rows(x_l) %>%
        bind_rows(x_u) %>%
        bind_rows(y_l) %>%
        bind_rows(y_u) %>%
        filter(!is.na(!!nx)) %>%
        arrange(!!nx) %>%
        AsSegments(!!nx, !!ny, suffix = "2") %>%
        FilterRange(!!nx, xlim) %>%
        FilterRange(!!nx2, xlim) %>%
        FilterRange(!!ny, ylim) %>%
        FilterRange(!!ny2, ylim)

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

#' @title Segments2Points
#' @param .dt Input table.
#' @param x \code{x} column name.
#' @param y \code{y} column name.
#' @param xend \code{x} end column name.
#' @param yend \code{y} end column name.
#' @return Trnasformed table.
#' @importFrom rlang enquo quo_squash !!
#' @importFrom dplyr %>% mutate select slice row_number if_else n
#' @importFrom tidyr gather spread
#' @export
Segments2Points <- function(.dt, x, y, xend, yend) {
    nx <- quo_squash(enquo(x))
    ny <- quo_squash(enquo(y))
    nx2 <- quo_squash(enquo(xend))
    ny2 <- quo_squash(enquo(yend))

    .dt %>%
        gather(Key, Val, !!nx, !!nx2, !!ny, !!ny2) %>%
        mutate(Key = if_else(row_number() <= (n() / 2), a_ch(nx), a_ch(ny)),
               ID = rep(1:(n() / 2), 2)) %>%
        spread(Key, Val) %>%
        select(-ID) %>%
        slice(UniqueWhichTol(!!nx))
}

#' @title GGCustomTextAnnotation
#' @param labels Labels.
#' @param xmin,xmax X coordinates of labels.
#' @param ymin,ymax Y coordinates of labels.
#' @param vjust Vertical justification.
#' @param hjust Horizontal justification.
#' @param gp Additional graphical parameters.
#' @return List of \code{annotation_custom} objects.
#' @importFrom foreach foreach %do%
#' @importFrom ggplot2 annotation_custom
#' @importFrom grid textGrob gpar
#' @export
GGCustomTextAnnotation <- function(labels, x, y,
                                   xmin = x, xmax = x,
                                   ymin = y, ymax = y,
                                   vjust = 0, hjust = 0,
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
    if (class(gp) == "gpar")
        gp <- rep(list(gp), n)
    else if (length(gp) == 1L && n != 1L)
        gp <- rep(gp[1], n)

    foreach(lbl = labels,
            xmn = xmin, xmx = xmax,
            ymn = ymin, ymx = ymax,
            vjst = vjust,
            hjst = hjust,
            g = gp) %do% {
                annotation_custom(textGrob(lbl,
                        hjust = hjst,
                        vjust = vjst,
                        gp = g),
                    xmn, xmx, ymn, ymx)
            }
}



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

GrobMarginSet <- function(grob,
    tLab, rLab, bLab, lLab,
    tAxis, rAxis, bAxis, lAxis) {

    if (!missing(tLab))
        grob %<>% GrobSizeSet("xlab-t", height = tLab)
    if (!missing(rLab))
        grob %<>% GrobSizeSet("ylab-r", width = rLab)
    if (!missing(bLab))
        grob %<>% GrobSizeSet("xlab-b", height = bLab)
    if (!missing(lLab))
        grob %<>% GrobSizeSet("ylab-l", width = lLab)

    if (!missing(tAxis))
        grob %<>% GrobSizeSet("axis-t", height = tAxis)
    if (!missing(rAxis))
        grob %<>% GrobSizeSet("axis-r", width = rAxis)
    if (!missing(bAxis))
        grob %<>% GrobSizeSet("axis-b", height = bAxis)
    if (!missing(lAxis))
        grob %<>% GrobSizeSet("axis-l", width = lAxis)

    invisible(grob)
}


GrobZero <- function(grob, name) {
    inds <- which(grob$layout$name %in% name)

    grob$grobs %<>%
        map2(seq_len(length(.)),
             function(g, i) if (i %in% inds) zeroGrob() else g)

    return(grob)
}

GrobSetGaps <- function(grob, top, right, bottom, left) {
    if (!missing(top))
        grob$heights[1] <- top
    if (!missing(right))
        grob$widths[length(grob$widths)] <- right
    if (!missing(bottom))
        grob$heights[length(grob$heights)] <- bottom
    if (!missing(left))
        grob$widths[1] <- left

    return(grob)
}

GrobsArrange <- function(grobs, ncol, nrow = length(grobs) %/% ncol,
    tLab = unit(0.6, "cm"), rLab = unit(0.6, "cm"),
    bLab = unit(0.6, "cm"), lLab = unit(0.6, "cm"),
    tAxis = unit(0.4, "cm"), rAxis = unit(0.4, "cm"),
    bAxis = unit(0.4, "cm"), lAxis = unit(0.4, "cm"),
    vGap = unit(0.1, "cm"), hGap = unit(0.1, "cm")) {

    n <- ncol * nrow

    halfVgap <- unit(0.5 * convertY(vGap, "cm", TRUE), "cm")
    halfHgap <- unit(0.5 * convertX(hGap, "cm", TRUE), "cm")


    lInds <- 1 + ((1:nrow) - 1) * ncol
    rInds <- lInds + (ncol - 1)
    tInds <- (1:ncol)
    bInds <- n - (ncol:1) + 1

    grobs[lInds] %<>% map(GrobMarginSet, lLab = lLab, lAxis = lAxis)
    grobs[-lInds] %<>% map(GrobMarginSet,
                           lLab = unit(0, "cm"), lAxis = unit(0, "cm")) %>%
                       map(GrobZero, c("axis-l", "ylab-l"))

    grobs[rInds] %<>% map(GrobMarginSet, rLab = rLab, rAxis = rAxis)
    grobs[-rInds] %<>% map(GrobMarginSet,
                           rLab = unit(0, "cm"), rAxis = unit(0, "cm")) %>%
                       map(GrobZero, c("axis-r", "ylab-r"))

    grobs[tInds] %<>% map(GrobMarginSet, tLab = tLab, tAxis = tAxis)
    grobs[-tInds] %<>% map(GrobMarginSet,
                           tLab = unit(0, "cm"), tAxis = unit(0, "cm")) %>%
                       map(GrobZero, c("axis-t", "xlab-t"))

    grobs[bInds] %<>% map(GrobMarginSet, bLab = bLab, bAxis = bAxis)
    grobs[-bInds] %<>% map(GrobMarginSet,
                           bLab = unit(0, "cm"), bAxis = unit(0, "cm")) %>%
                       map(GrobZero, c("axis-b", "xlab-b"))

    grobs %<>% map(GrobSetGaps,
                   right = halfHgap, left = halfHgap,
                   top = halfVgap, bottom = halfVgap)


    vpWidthCm <- convertX(current.viewport()$width, "cm", TRUE)
    hExtraSpaceCm <- convertX(lLab + lAxis + rLab + rAxis +
                            hGap * ncol, "cm", TRUE)

    hSize <- (vpWidthCm - hExtraSpaceCm) / ncol

    vpHeightCm <- convertY(current.viewport()$height, "cm", TRUE)
    vExtraSpaceCm <- convertY(tLab + tAxis + bLab + bAxis +
                            vGap * nrow, "cm", TRUE)

    vSize <- (vpHeightCm - vExtraSpaceCm) / nrow

    if (ncol == 1)
        widths <- 1
    else if (ncol == 2)
        widths <- c(
            convertX(lLab + lAxis + hGap + unit(hSize, "cm"), "cm", TRUE),
            convertX(rLab + rAxis + hGap + unit(hSize, "cm"), "cm", TRUE)) /
                vpWidthCm

    else widths <- c(
            convertX(lLab + lAxis + hGap + unit(hSize, "cm"), "cm", TRUE),
            rep(convertX(unit(hSize, "cm") + hGap, "cm", TRUE), ncol - 2),
            convertX(rLab + rAxis + hGap + unit(hSize, "cm"), "cm", TRUE)) /
                vpWidthCm


    if (nrow == 1)
        heights <- 1
    else if (nrow == 2)
        heights <- c(
            convertY(tLab + tAxis + vGap + unit(vSize, "cm"), "cm", TRUE),
            convertY(bLab + bAxis + vGap + unit(vSize, "cm"), "cm", TRUE)) /
                vpHeightCm
    else heights <- c(
            convertY(tLab + tAxis + vGap + unit(vSize, "cm"), "cm", TRUE),
            rep(convertY(unit(vSize, "cm") + vGap, "cm", TRUE), nrow - 2),
            convertY(bLab + bAxis + vGap + unit(vSize, "cm"), "cm", TRUE)) /
                vpHeightCm

    grid.newpage()
    aGrobs <<- arrangeGrob(grobs = grobs, widths = widths, heights = heights)
    grid.draw(aGrobs)
}