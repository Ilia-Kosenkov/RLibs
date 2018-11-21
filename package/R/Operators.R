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


#' @title \code{is} interfix operator
#' @param object Object to test.
#' @param class Target type (supports \code{rlang} quosure).
#' @description An interfix version of \link{is} method.
#' @return \code{logical} \code{TRUE} if
#' \code{object} is of class \code{class}, \code{FALSE} otherwise.
#' @importFrom rlang quo_squash enquo
#' @export
`%is%` <- function(object, class) {
    nm <- as.character(quo_squash(enquo(class))) %??% "NULL"
    return(is(object, nm))
}

#' @title Concat/add infix operator.
#' @param x Left summand.
#' @param y Right summand.
#' @description Performs (possibly) a vectorized summation operation,
#'  which depends on the class of operators.
#'  Following methods are implemented:
#'  \code{character} + \code{character},
#'        1-to-1 vectorized, concatenation of strings
#' @return Result of the aapropriate summation/concatenation.
#' @importFrom purrr map2_chr
#' @export
`%+%` <- function(x, y) {
    warning("Consider switching to RLibs::`%&%` to avoid name masking.")
    if (x %is% character && y %is% character) {
        if (length(x) == length(y))
            return(map2_chr(x, y, ~ paste0(.x, .y)))
        else
            stop(paste("Unable to concat string collections:",
                "different collection lengths."))
        }
    else
        stop(sprintf(paste0("Unable to add objects of following types:",
            "\r\n[%s]\r\n[%s]\r\nNo rule defined.\r\n"),
            paste(class(x), collapse = ", "),
            paste(class(y), collapse = ", ")))
}

#' @title Concat/add infix operator.
#' @param x Left summand.
#' @param y Right summand.
#' @description Performs (possibly) a vectorized summation operation,
#'  which depends on the class of operators.
#'  Following methods are implemented:
#'  \code{character} + \code{character},
#'        1-to-1 vectorized, concatenation of strings.
#' Does the same as `%+%`.
#' @return Result of the aapropriate summation/concatenation.
#' @importFrom purrr map2_chr
#' @export
`%&%` <- function(x, y) {
    RLibs::`%+%`(x, y)
}

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
