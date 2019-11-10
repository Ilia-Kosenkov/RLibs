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

#' @title Segments2Points
#' @param .dt Input table.
#' @param ... Column names to transform.
#' @param suffix Suffix of paired column names.
#' @return Trnasformed table.
#' @importFrom purrr map
#' @importFrom rlang enquos quo_squash !!! sym syms
#' @importFrom dplyr %>% mutate select arrange n distinct
#' @importFrom tidyr gather spread
#' @importFrom stringr str_replace
#' @export
Segments2PointsEx <- function(.dt, ..., suffix = "2") {
    cols <- enquos(...) %>% map(quo_squash) %>% syms
    cols2 <- cols %>% map(~sym(as.character(.x) %+% suffix))

    joined <- append(cols, cols2)

    .dt %>%
        gather("Key__", "Value__", !!!joined) %>%
        mutate(Key__ = str_replace(Key__, suffix %+% "$", "")) %>%
        arrange(Key__) %>%
        mutate(ID__ = rep(seq(1, n() / length(cols)), length(cols))) %>%
        spread("Key__", "Value__") %>%
        select(-ID__) %>%
        distinct(!!!cols, .keep_all = TRUE)
}