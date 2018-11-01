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

#' @title GGPlotLims
#' @param plt Input \code{ggplot} plot.
#' @return \code{list} with x and y limits.
#' @importFrom dplyr %>%
#' @importFrom rlang %||% eval_tidy is_empty is_null
#' @importFrom purrr map transpose flatten discard
#' @export
GGPlotLims <- function(plt) {
    parentData <- plt$data %||% list()
    parentAes <- plt$mapping %||% list() %>% flatten
    xNms <- c("x", "xend", "xmin", "xmax")
    yNms <- c("y", "yend", "ymin", "ymax")

    plt$layers %>%
        discard(~is_null(.x$mapping)) %T>% print %>%
        map(function(lyr) {
            aes <- lyr$mapping %||% list() %>% flatten
            if (lyr$inherit.aes)
                aes <- append(parentAes, aes)

            if (is_empty(lyr$data))
                data <- plt$data
            else
                data <- lyr$data

            yAes <- aes[names(aes) %in% yNms]
            xAes <- aes[names(aes) %in% xNms]

            xlim <- xAes %>%
                map(~eval_tidy(.x, data)) %>%
                map(range) %>%
                range(na.rm = TRUE)

            ylim <- yAes %>%
                map(~eval_tidy(.x, data)) %>%
                map(range) %>%
                range(na.rm = TRUE)

            return(list(xlim = xlim, ylim = ylim))
    })  %>%
    transpose %>%
    map(range)
}
