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

#' are_equal_f
#' @description
#' Checks for floating-point equality of two collections.
#' @param x First floating point collection.
#' @param y Second collection. By default has all zeroes
#' to enable shortcut comparison to zero.
#' @param tol Optional tolerance, defaulted to machine precision.
#'
#' @return Logical vector.
#' @importFrom purrr map2_lgl
#' @export
are_equal_f <- function(x, y = rep(0.0, length(x)), tol = .Machine$double.eps) {
    if (!is_double(x) && !is_integer(x))
        stop("`x` should be either floating point or integer vector.")

    if (!is_double(y) && !is_integer(y))
        stop("`y` should be either floating point or integer vector.")

    if (!is_scalar_double(tol) && !is_scalar_integer(tol))
        stop("`tol` should be a small floating point number.")

    map2_lgl(x, y,
        function(q, u) {
            if (abs(u) < tol)
                abs(q) < tol
            else
                abs(q - u) < max(min(abs(q), abs(u)) * tol, tol)
        })
}