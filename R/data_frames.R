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
#' @param .strict \code{logical}; If \code{TRUE}, boundaries are not included.
#' @return Filtered table.
#' @importFrom dplyr filter
#' @export
filter_range <- function(.data, .var, .range, .strict = FALSE) {
    vctrs::vec_assert(.range, size = 2L)
    vctrs::vec_assert(.strict, logical(), 1L)

    gr <- if (.strict) `>` else `>=`
    le <- if (.strict) `<` else `<=`

    dplyr::filter(.data, gr({{ .var }}, .range[1]) & le({{ .var }}, .range[2]))
}

#' @rdname filter_range
#' @export
FilterRange <- function(.data, .var, .range, .strict = FALSE) {
    lifecycle::deprecate_warn("0.6.0", "RLibs::FilterRange", "RLibs::filter_range")
}

clamp.data.frame <- function(...) {

    args <- rlang::enquos(...)
    assertthat::assert_that(len(args) == 3L)
    names <- names(args)
    data_id <- which(names %==% ".data") %0% 1L
    col_id <- which(names %==% ".col") %0% 2L
    range_id <- which(names %==% ".range") %0% 3L

    data <- rlang::eval_tidy(args[[data_id]])
    expr <- args[[col_id]]
    range <- rlang::eval_tidy(args[[range_id]])

    vctrs::vec_assert(range, size = 2L)

    dplyr::mutate(data,
                  !!expr := dplyr::if_else(`>`(!!expr, range[2]), range[2], !!expr),
                  !!expr := dplyr::if_else(`<`(!!expr, range[1]), range[1], !!expr))
}

clamp.tibble <- function(...)
    clamp.data.frame(...)