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

#' @title ClipXY
#' @param .dt Input table.
#' @param x \code{x} column name.
#' @param y \code{y} column name.
#' @param xlim Limits on \code{x}.
#' @param ylim Limits on \code{y}.
#' @param ... Additional columns to preserve.
#' @param saveWhich Which value to assign to the non-interpolated
#' column. Can be 1 (preceeding) and 2 (succeeding)
#' @importFrom rlang quo_squash enquo enquos !! !!! := sym
#' @importFrom magrittr extract %<>%
#' @importFrom purrr map reduce map_chr keep
#' @importFrom dplyr %>% mutate pull arrange bind_rows filter distinct
#' @export
ClipXY <- function(.dt, x, y, xlim, ylim, ..., saveWhich = 1) {

    if (saveWhich != 1 && saveWhich != 2)
        stop("`saveWhich` can be either 1 or 2.")

    .dots <- enquos(...) %>%
        map(quo_squash) %>%
        map(sym)
    nx <- sym(quo_squash(enquo(x)))
    ny <- sym(quo_squash(enquo(y)))
    lny <- sym("l" %+% a_ch(ny))
    uny <- sym("u" %+% a_ch(ny))
    lnx <- sym("l" %+% a_ch(nx))
    unx <- sym("u" %+% a_ch(nx))

    nms <- list(nx, ny, lny, uny, lnx, unx) %>%
        map_chr(as.character) %>% keep(~.x %in% names(.dt))

    nmSymb <- nms %>% map(sym)

    args <- nms %>%
        map(~.dt %>% pull(.x) %>% class) %>%
        setNames(nms)


    .dt %<>%
        arrange(!!nx)

    process <- function(name, val) {
        .dt %>%
        pull(!!name) %>%
        BetweenWhich(val) %>%
        map(function(ids) {
            if (any(is.na(ids)))
                tbl <- do.call(tibble, args %>% map(~do.call(.x, list(0))))
            else {
                tbl <- do.call(tibble, args %>% map(~do.call(.x, list(1))))
                tbl <- nms %>%
                    reduce(~mutate(.x,
                                !!sym(.y) := Lin(val,
                                    .dt %>% pull(!!name) %>% extract(ids),
                                    .dt %>% pull(.y) %>% extract(ids))),
                            .init = tbl)
                tbl <- reduce(
                    .dots,
                    ~ mutate(.x,
                        !!.y := .dt %>% pull(!!.y) %>% extract(ids[saveWhich])),
                    .init = tbl) %>% mutate(flag__ = TRUE)
            }


        })
    }

    x_l <- process(nx, xlim[1])
    x_u <- process(nx, xlim[2])
    y_l <- process(ny, ylim[1])
    y_u <- process(ny, ylim[2])

    nx2 <- nx %>% a_ch %>% paste0("2") %>% sym
    ny2 <- ny %>% a_ch %>% paste0("2") %>% sym

    safeClamp <- function(.dt, col, lim) {
        if (as.character(col) %in% names(.dt))
            .dt <- Clamp(.dt, !!col, lim)
        return (.dt)
    }


    .dt %>%
        bind_rows(x_l) %>%
        bind_rows(x_u) %>%
        bind_rows(y_l) %>%
        bind_rows(y_u) %>%
        filter(!is.na(!!nx)) %>%
        distinct(!!nx, !!ny, .keep_all = TRUE) %>%
        arrange(!!nx) %>%
        safeClamp(lny, ylim) %>%
        safeClamp(uny, ylim) %>%
        safeClamp(lnx, xlim) %>%
        safeClamp(unx, xlim) %>%
        AsSegments(!!!nmSymb, suffix = "2") %>%
        FilterRange(!!nx, xlim) %>%
        FilterRange(!!nx2, xlim) %>%
        FilterRange(!!ny, ylim) %>%
        FilterRange(!!ny2, ylim)
}

#data <- tibble(id = 1L:50L, x = 1:50,
               #y = sin(4 * pi * x / length(x)) + rnorm(length(x), sd = 0.75),
               #ly = y - 1,
               #uy = y + 1)

#xlim <- c(5, 45)
#ylim <- c(-1, 1)

#data2 <- ClipXY(data, x, y, xlim, ylim, id)
#data2 %>% Segments2PointsEx(x, y, ly, uy)

#data3 <- TrimRibbonData(data, x, y, xlim, ylim, ly, uy)

#plt <- ggplot(data, aes(x, y)) +
        #geom_polygon(aes(x, y),
            #data = tibble(
                #x = c(xlim, rev(xlim)),
                #y = ylim[c(1, 1, 2, 2)]),
            #alpha = 0.30) +
        #geom_ribbon(aes(ymin = ly, ymax = uy),
            #alpha = 0.25, fill = "#0000A0") +
        #geom_ribbon(aes(ymin = ly, ymax = uy),
            #alpha = 0.45, fill = "#108010",
            #data = data3) +
        #geom_line(color = "#707070", size = 1.15) +
        #geom_point(color = "#70FF70", size = 4) +
        #DefaultTheme() +
        #geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
            #col = "#F00000",
            #size = 1.15,
            #data2) +
        #geom_point(aes(x = x, y = y),
            #col = "#F00000",
            #size = 2,
            #data2 %>% Segments2PointsEx(x, y, ly, uy)) +
        #geom_errorbar(aes(ymin = ly, ymax = uy),
            #color = "#A00000",
            #size = 0.95,
            #linetype = 2,
            #data2 %>% Segments2PointsEx(x, y, ly, uy)) #+
        ##coord_cartesian(xlim = xlim, ylim = ylim, FALSE)


#print(plt)