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
#' @importFrom dplyr %>% arrange mutate pull slice row_number
#' @importFrom tibble tibble
#' @export
TrimRibbonData <- function(.data, x, y, xlim, ylim, lwr, upp, ...) {

    .dots <- ensyms(...)

    nm <- list()
    nm$x <- ensym(x)
    nm$y <- ensym(y)
    nm$lwr <- ensym(lwr)
    nm$upp <- ensym(upp)

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
        slice(xInds) %>%
        filter(!!nm$lwr <= ylim[2], !!nm$upp >= ylim[1])
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