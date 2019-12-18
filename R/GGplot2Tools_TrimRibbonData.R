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



#' @title Trim ribbon data
#' @rdname trim_ribbon_data
#' @param .data Input table.
#' @param x \code{x} column.
#' @param y \code{y} column.
#' @param xlim \code{x} limits.
#' @param ylim \code{y} limits.
#' @param lwr Lower \code{y} limit of ribbon.
#' @param upp Upper \code{y} limit of ribbon
#' @param ... Additional columns to preserve.
#' @param .exclude_outliers If \code{TRUE}, \code{y}-outliers are assigned NA values.
#' @return Table ready to be plotted.
#' @importFrom rlang ensyms quo_text := !! is_empty
#' @importFrom purrr map_dfr reduce
#' @importFrom magrittr extract is_less_than is_greater_than %<>%
#' @importFrom dplyr %>% arrange mutate pull slice row_number
#' @importFrom tibble tibble
#' @export
trim_ribbon_data <- function(.data, x, y, xlim, ylim, lwr, upp, ..., .exclude_outliers = FALSE) {

    .dots <- ensyms(...)

    arrange(.data, {{ x }}) -> .data

    which(pull(.data, {{ x }}) %withini% xlim) %>%
        cc(min(.) - 1, max(.) + 1) %>%
        clamp(cc(1, len(.data))) %>%
        unique %>%
        sort -> x_inds


    plt_data <- slice(.data, x_inds)

    if (.exclude_outliers %===% TRUE) {
        plt_data <-
            mutate_at(
                plt_data, 
                vars({{ lwr }}, {{ upp }}, {{ y }}),
                ~ if_else({{ lwr }} <= ylim[2] & {{ upp }} >= ylim[1], ., vec_cast(NA, .)))
    }
    else {
        plt_data <-
            filter(plt_data, {{ lwr }} <= ylim[2], {{ upp }} >= ylim[1])
    }

    m <- len(plt_data)


    if (pull(plt_data, {{ x }})[1] < xlim[1]) {
        arg <-  pull(plt_data, {{ x}})[1:2]
        val <- pull(plt_data, {{ y }})[1:2]
        lVal <- pull(plt_data, {{ lwr}})[1:2]
        uVal <- pull(plt_data, {{ upp }})[1:2]

        y0 <- lin(xlim[1], arg, val)
        l0 <- lin(xlim[1], arg, lVal)
        u0 <- lin(xlim[1], arg, uVal)

        plt_data %<>%
            mutate({{ x }} := replace({{ x }}, row_number() == 1L, xlim[1])) %>%
            mutate({{ y }} := replace({{ y }}, row_number() == 1L, y0)) %>%
            mutate({{ lwr }} := replace({{ lwr }}, row_number() == 1L, l0)) %>%
            mutate({{ upp }} := replace({{ upp }}, row_number() == 1L, u0))
    }

    if (pull(plt_data, {{ x }})[m] > xlim[2]) {

        arg <- pull(plt_data, {{ x }})[m - 1:0]
        val <-  pull(plt_data, {{ y }})[m - 1:0]
        lVal <- pull(plt_data, {{ lwr }})[m - 1:0]
        uVal <- pull(plt_data, {{ upp }})[m - 1:0]

        y0 <- Lin(xlim[2], arg, val)
        l0 <- Lin(xlim[2], arg, lVal)
        u0 <- Lin(xlim[2], arg, uVal)

        plt_data %<>%
            mutate({{ x }} := replace({{ x }}, row_number() %==% m, xlim[2])) %>%
            mutate({{ y }} := replace({{ y }}, row_number() %==% m, y0)) %>%
            mutate({{ lwr }} := replace({{ lwr }}, row_number() %==% m, l0)) %>%
            mutate({{ upp }} := replace({{ upp }}, row_number() %==% m, u0))
    }
    Interpolate <- function(.dt, name, lim) {

        pull(.dt, {{ name }}) %>% {
            ((.[1:(len(.) - 1)] < lim) &
             (.[2:len(.)] > lim)) |
            ((.[1:(len(.) - 1)] > lim) &
             (.[2:len(.)] < lim))
        } %>%
        which %>% {
            if (is_empty(.))
                .dt 
            else {
                map_dfr(., function(x_loc) {
                    x_loc <- x_loc + 0:1
                    vals <- c(
                        lin(lim,
                            pull(.dt, {{ name }})[x_loc],
                            pull(.dt, {{ x }})[x_loc]),
                        lin(lim,
                            pull(.dt, {{ name }})[x_loc],
                            pull(.dt, {{ y }})[x_loc]),
                        lin(lim,
                            pull(.dt, {{ name }})[x_loc],
                            pull(.dt, {{ lwr }})[x_loc]),
                        lin(lim,
                            pull(.dt, {{ name }})[x_loc],
                            pull(.dt, {{ upp }})[x_loc]))

                    tibble({{ x }} := vals[1], {{ y }} := vals[2], {{ lwr }} := vals[3], {{ upp }} := vals[4]) %>%
                    mutate({{ name }} := lim) %>% {
                        reduce(
                            .dots,
                            function(d, nm) mutate(d, !!nm := pull(.dt, !!nm)[x_loc[1]]),
                            .init = .)
                    }
                }) %>%
                bind_rows(.dt) %>%
                arrange({{ x }})
            }
        }
    }

    y_clmp <- sym(quo_text(ensym(y)) %&% "_clamped")

    plt_data %>%
        Interpolate({{ y }}, ylim[1]) %>%
        Interpolate({{ lwr }}, ylim[1]) %>%
        Interpolate({{ upp }}, ylim[1]) %>%
        Interpolate({{ y }}, ylim[2]) %>%
        Interpolate({{ lwr }}, ylim[2]) %>%
        Interpolate({{ upp }}, ylim[2]) %>%
        clamp({{ lwr }}, ylim) %>%
        clamp({{ upp }}, ylim) %>%
        mutate(!!y_clmp := {{ y }}) %>%
        clamp(!!y_clmp, ylim) %>%
        arrange({{ x }})
}

#' @rdname trim_ribbon_data
#' @export
TrimRibbonData <- function(.data, x, y, xlim, ylim, lwr, upp, ..., .exclude_outliers = FALSE) {
    lifecycle::deprecate_warn("0.7.10", "RLibs::TrimRibbonData()", "RLibs::trim_ribbon_data()")

    trim_ribbon_data(.data = .data, x = {{x}}, y = {{y}},
                     xlim = xlim, ylim = ylim,
                     lwr = {{lwr}}, upp = {{upp}},
                     .exclude_outliers = .exclude_outliers, ...)
}