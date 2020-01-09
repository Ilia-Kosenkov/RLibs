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
#' @importFrom stringr str_match str_split
#' @importFrom dplyr %>% first
#' @importFrom glue glue_collapse
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

#' @title either
#' @param ... A number of conditions, one of which should be met.
#' @importFrom rlang list2 call_args
#' @importFrom glue glue_collapse
#' @importFrom purrr %>% map some
#' @importFrom dplyr first
#' @export
either <- function(...) {
    args <- list2(...)
    args %>% some(identity)
}

#' @title vec_within
#'
#' @param x Vector to test
#' @param lower Lower limit
#' @param upper Upper limit
#' @param include_lower Wether to include lower limit
#' @param include_upper Wether to include upper limit
#' @importFrom vctrs vec_is vec_cast_common
#' @return \code{logical} \code{TRUE} if all elements are within the boundaries.
#' @export
vec_within <- function(x, lower, upper, include_lower = TRUE, include_upper = TRUE) {
    if (!vec_is(include_lower, logical(), 1L))
        return (FALSE)
    if (!vec_is(include_upper, logical(), 1L))
        return(FALSE)
    if (!vec_is(lower, size = 1L))
        return(FALSE)
    if (!vec_is(upper, size = 1L))
        return(FALSE)

    vec_cast_common(x, lower, upper) %->% c(x, lower, upper)

    left_check <- if_else_weak(include_lower, x >= lower, x > lower)
    right_check <- if_else_weak(include_upper, x <= upper, x < upper)

    return(all(left_check & right_check))
}

parse_assertion <- function(str) {
    parsed <- array(str_match(str, "^\\(*(?:(not)\\(+)?(is)[\\._](.+)\\(+(.+?)\\)+$"))
    if (is.na(parsed[1]))
        glue_fmt("{str} is false")
    else {
        parsed[4] <- parsed[4] %>% str_split("[_\\.]") %>% first %>% glue_collapse(sep = " ")
        if (vec_size(parsed[4]) == 1L &&
            (tolower(parsed[4]) == "missing" || tolower(parsed[4]) == "empty")) {
            needs_a <- ""
        } else
            needs_a <- "a "

        if (is.na(parsed[2]))
            glue_fmt("{parsed[5]} is not {needs_a}{parsed[4]}")
        else
            glue_fmt("{parsed[5]} should not be {needs_a}{parsed[4]}")
        }
}

assertthat::on_failure(passes) <- function(call, env) {
    callStr <- deparse(call$x)
    parse_assertion(callStr)
}

assertthat::on_failure(has_size) <- function(call, env) {
    len <- eval(call$len, env)
    callStr <- deparse(call$x)
    glue_fmt("{callStr} should have size of {len}")
}

assertthat::on_failure(either) <- function(call, env) {
    args <- rlang::call_args(call) %>%
        map(deparse) %>%
        map(parse_assertion) %>%
        map(~glue_fmt("\t{.x}")) %>%
        glue_collapse(sep = ",\n") -> asserts
    glue_fmt("Failed, because \n{asserts}.")
}

assertthat::on_failure(not) <- function(call, env) {
    str <- deparse(call)
    parse_assertion(str)
}

assertthat::on_failure(vec_within) <- function(call, env) {
    rlang::call_args(call) -> args
    left_br <- if_else_weak(args$include_lower %||% TRUE, "[", "(")
    right_br <- if_else_weak(args$include_upper %||% TRUE, "]", ")")

    glue_fmt("{deparse(args$x)} is not within {left_br}{deparse(args$lower)}, {deparse(args$upper)}{right_br} range")
}