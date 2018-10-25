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

utils::globalVariables(c("Key__", "Val__", "ID__"))
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
        gather(Key__, Val__, !!nx, !!nx2, !!ny, !!ny2) %>%
        mutate(Key__ = if_else(row_number() <= (n() / 2), a_ch(nx), a_ch(ny)),
               ID__ = rep(1:(n() / 2), 2)) %>%
        spread(Key__, Val__) %>%
        select(-ID__) %>%
        slice(UniqueWhichTol(!!nx))
}