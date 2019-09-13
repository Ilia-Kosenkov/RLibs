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


#' @title fitler_range
#' @param .data Input table.
#' @param .var Column to filter.
#' @param .range Limits on the column.
#' @return FIltered table.
#' @importFrom rlang enquo quo_squash !!
#' @importFrom dplyr %>% filter
#' @export
filter_range <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        filter(!!expr >= .range[1] & !!expr <= .range[2])
}

#' @rdname filter_range
#' @export
FilterRange <- deprecate_function(FilterRange, filter_range)

#' @title Clamp.data.frame
#' @param .data Input \code{data.frame} or \code{tibble}.
#' @param .var Variable to clamp.
#' @param .range Clamp range.
#' @return \code{.data} whith clamped within \code{.range} column \code{.var}.
#' @importFrom dplyr mutate %>% if_else
#' @importFrom rlang enquo quo_squash !! :=
#' @export
Clamp.data.frame <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        mutate(!!expr := if_else(!!expr > .range[2], .range[2], !!expr)) %>%
        mutate(!!expr := if_else(!!expr < .range[1], .range[1], !!expr))
}