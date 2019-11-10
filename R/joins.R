#   MIT License
#
#   Copyright(c) 2019 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
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


## Fixing `dplyr` type issues before it goes full `vctrs`
#' @title join
#' @rdname join
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.
#' @param copy Should tables be copied.
#' @param suffix Column names suffixes if names are the same.
#' @param ... Other params
#' @param keep If TRUE the by columns are kept in the nesting joins.
#' @param name Used in nested join.
#
#' @return Joined table using respective \code{dplyr::*_join()}
#' @export
#' @importFrom dplyr left_join
left_join_safe <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    data <- safe_join_coalesce(x, y, by)

    left_join(data$x, data$y, by = data$by, copy = copy, suffix = suffix, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr inner_join
inner_join_safe <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    data <- safe_join_coalesce(x, y, by)

    inner_join(data$x, data$y, by = data$by, copy = copy, suffix = suffix, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr right_join
right_join_safe <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    data <- safe_join_coalesce(x, y, by)

    right_join(data$x, data$y, by = data$by, copy = copy, suffix = suffix, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr full_join
full_join_safe <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    data <- safe_join_coalesce(x, y, by)

    full_join(data$x, data$y, by = data$by, copy = copy, suffix = suffix, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr nest_join
nest_join_safe <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
    data <- safe_join_coalesce(x, y, by)

    nest_join(data$x, data$y, data$by, copy = copy, keep = keep, name = name, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr semi_join
semi_join_safe <- function(x, y, by = NULL, copy = FALSE, ...) {
    data <- safe_join_coalesce(x, y, by)

    semi_join(data$x, data$y, data$by, copy = copy, ...)
}

#' @rdname join
#' @export
#' @importFrom dplyr anti_join
anti_join_safe <- function(x, y, by = NULL, copy = FALSE, ...) {
    data <- safe_join_coalesce(x, y, by)

    anti_join(data$x, data$y, data$by, copy = copy, ...)
}

#' @importFrom rlang quo_name enquo quo !! !!! set_names
#' @importFrom dplyr common_by mutate
#' @importFrom purrr map2
#' @importFrom vctrs vec_cast vec_ptype2
safe_join_coalesce <- function(x, y, by) {
    x_rep <- quo_name(enquo(x))
    y_rep <- quo_name(enquo(y))

    bys <- common_by(by, x, y)

    map2(bys$x, bys$y,
            function(left, right)
                vec_ptype2(
                    x[[left]],
                    y[[right]],
                    x_arg = paste(x_rep, left, sep = "$"),
                    y_arg = paste(y_rep, right, sep = "$"))) -> common_types

    mutator <- function(name, type) quo(vec_cast(!!sym(name), !!type))

    map2(set_names(bys$x), common_types, mutator) -> x_mod
    map2(set_names(bys$y), common_types, mutator) -> y_mod

    return(list(x = mutate(x, !!!x_mod), y = mutate(y, !!!y_mod), by = bys))
}