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

#' @title Null/empty-coalescing operator
#' @description Improves the \code{rlang::\%||\%} operator by
#' handling also cases of zero-length objects.
#' @param x Left side of the operator. To be tested.
#' @param y Right side of the operator. Is returned if left side is
#' null or empty.
#' @return Either x or y.
#' @importFrom rlang is_empty is_null
#' @export
`%??%` <- function(x, y) {
    if (is_null(x) ||
        is_empty(x))
            y
    else
        x
}

#' @title Dot-product
#' @description A \code{vctrs}-typed replacement to \code{base::`\%*\%`}.
#' @param x LHS.
#' @param y RHS.
#'
#' @return A dot product of two vectors.
#' @export
`%.%` <- function(x, y) {
    r <- vec_recycle_common(!!!vec_cast_common(x = x, y = y))

    sum(r$x * r$y)
}