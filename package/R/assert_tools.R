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

#' @title passes
#'
#' @param x Test expression in the form of \code{is_tibble(mtcars)}
#'
#' @description Should be used in conjunction with \code{assertthat::assert_that};
#' provides a more detailed error explanation.
#' @return Returns the result of the test. 
#' @examples \dontrun{assert_that(passes(is_tibble(mtcars)))}
#' @importFrom stringr str_match
#' @export
passes <- function(x) {
    x
}

#' @title has_size
#'
#' @param x Tested object
#' @param len Expected size
#'
#' @return Result of the \code{vctrs::vec_size(x) == len} test
#' @examples \dontrun{assertthat::assert_that(has_size(1, 2))}
#' @importFrom vctrs vec_size
#' @export
has_size <- function(x, len) {
    vec_size(x) == len
}

#' @title is_numeric
#'
#' @param x Vector to test
#'
#' @return \code{TRUE} if \code{x} is either \code{double} or \code{integer}.
#' @importFrom rlang is_double is_integer
#' @export
is_numeric <- function(x) {
    is_double(x) || is_integer(x)
}

#' @title is_positive
#' @param x Numeric vector to test
#' @export
is_positive <- function(x) {
    is_numeric(x) && all(x > 0)
}

#' @title is_negative
#' @param x Numeric vector to test
#' @export
is_negative <- function(x) {
    is_numeric(x) && all(x < 0)
}

#' @title is_nonnegative
#' @param x Numeric vector to test
#' @export
is_nonnegative <- function(x) {
    is_numeric(x) && all(x >= 0)
}

#' @title is_nonpositive
#' @param x Numeric vector to test
#' @export
is_nonpositive <- function(x) {
    is_numeric(x) && all(x <= 0)
}


assertthat::on_failure(passes) <- function(call, env) {
    callStr <- deparse(call$x)
    parsed <- array(str_match(callStr, "(is)[\\._](.+)\\((.+)\\)"))
    if (any(is.na(parsed)))
        return(callStr)
    else
        glue_fmt("{parsed[4]} is not a {parsed[3]}")
    }

assertthat::on_failure(has_size) <- function(call, env) {
    len <- eval(call$len, env)
    callStr <- deparse(call$x)
    glue_fmt("{callStr} should have size of {len}")
}


