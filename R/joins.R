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


#' @title join_cnd
#' @rdname join_condition
#' @param left LHS table.
#' @param right RHS table
#' @param ... Joining conditions, where \code{.x} refers to RHS table and
#' \code{.y} referes to LHS table. E.g. \code{.x$hp > .y$mpg}.
#' Comma-separated conditions are \code{`&`}.
#' @param .selector If multiple matches, which RHS to select
#' @param .type Type of the performed join.
#' @param .suffix Suffixes that are added to the repeating columns.
#' @param .enforce_suffix If \code{TRUE}, enforces suffixes on all columns.
#'
#' @importFrom utils tail
#' @importFrom dplyr group_split
#' @importFrom purrr map_at
#' @importFrom vctrs vec_is_empty vec_cbind
#' @importFrom rlang abort quo_is_call quo_get_expr quo_set_expr
join_cnd <- function(left, right, ...,
    .type = "inner",
    .selector = "all",
    .suffix = c("__l", "__r"),
    .enforce_suffix = FALSE) {

    cond <- enquos(...)

    if (vec_is(.selector, character(), 1L)) {
        .selector <- tolower(.selector)
        if (.selector == "first")
            selector <- function(x) head(x, 1)
        else if (.selector == "last")
            selector <- function(x) tail(x, 1)
        else if (.selector == "all")
            selector <- function(x) x
        else
            abort("Error", "maxi2_invalid_argument")
        }
    else
        selector <- as_function(.selector)

    .type <- match.arg(.type, cc("left", "right", "inner", "full"))

    cond <- map(cond, function(cnd) {
        if (!quo_is_call(cnd))
            abort("Error", "maxi2_invalid_argument")

        expr <- quo_get_expr(cnd)
        expr <- expr(outer(!!expr[[2]], !!expr[[3]], !!expr[[1]]))
        quo_set_expr(cnd, expr)
    })


    cond %>%
        map(eval_tidy, list(.x = left, .y = right)) %>%
        map(which, arr.ind = TRUE) %>%
        map(as_tibble, .name_repair = "minimal") %>%
        reduce(inner_join, by = c("row", "col")) -> indices

    if (.type %==% "left") {
        indices %>%
            arrange(row) %>%
            group_split(row) %>%
            map(slice, selector(1:n())) %>%
            vec_rbind_uq -> indices

        vec_rbind(indices, tibble(row = setdiff(1:len(left), indices$row), col = NA_integer_)) -> indices

    }
    else if (.type %==% "right") {
        indices %>%
            arrange(col) %>%
            group_split(col) %>%
            map(slice, selector(1:n())) %>%
            vec_rbind_uq -> indices

        vec_rbind(indices, tibble(col = setdiff(1:len(right), indices$col), row = NA_integer_)) -> indices
    }
    else if (.type %==% "full") {
        vec_rbind(indices, tibble(row = setdiff(1:len(left), indices$row), col = NA_integer_)) -> indices
        vec_rbind(indices, tibble(col = setdiff(1:len(right), indices$col), row = NA_integer_)) -> indices
    }

    indices %>% arrange(row, col) -> indices

    left <- left[indices$row, ]
    right <- right[indices$col, ]

    if (.enforce_suffix) {
        left <- set_names(left, paste0(names(left), .suffix[1]))
        right <- set_names(right, paste0(names(right), .suffix[2]))
    }
    else {
        common_names <- which(vec_in(names(right), names(left)))
        if (!vec_is_empty(common_names)) {
            left <- set_names(left,
                flatten_chr(map_at(
                    names(left),
                    which(vec_in(names(left), names(right)[common_names])),
                    paste0, .suffix[1])))
            right <- set_names(right,
                flatten_chr(map_at(
                    names(right),
                    common_names,
                    paste0, .suffix[2])))
        }

    }

    vec_cbind(left, right)
}

#' @name inner_join_cnd
#' @rdname join_condition
#' @export
inner_join_cnd <- function(left, right, ...,
    .selector = "all",
    .suffix = c("__l", "__r"),
    .enforce_suffix = FALSE) {
    join_cnd(left, right, ..., .type = "inner", .selector = .selector, .suffix = .suffix, .enforce_suffix = .enforce_suffix)
}

#' @name left_join_cnd
#' @rdname join_condition
#' @export
left_join_cnd <- function(left, right, ...,
    .selector = "all",
    .suffix = c("__l", "__r"),
    .enforce_suffix = FALSE) {
    join_cnd(left, right, ..., .type = "left", .selector = .selector, .suffix = .suffix, .enforce_suffix = .enforce_suffix)
}

#' @name right_join_cnd
#' @rdname join_condition
#' @export
right_join_cnd <- function(left, right, ...,
    .selector = "all",
    .suffix = c("__l", "__r"),
    .enforce_suffix = FALSE) {
    join_cnd(left, right, ..., .type = "right", .selector = .selector, .suffix = .suffix, .enforce_suffix = .enforce_suffix)
}

#' @name full_join_cnd
#' @rdname join_condition
#' @export
full_join_cnd <- function(left, right, ...,
    .selector = "all",
    .suffix = c("__l", "__r"),
    .enforce_suffix = FALSE) {
    join_cnd(left, right, ..., .type = "full", .selector = .selector, .suffix = .suffix, .enforce_suffix = .enforce_suffix)
}

#tbl <- tibble(x = cc(1, 10, 12, 14, 15, 17, 20), y = x + 3, c = letters[seq_along(x)])

#join_cnd(mtcars[1:15,], tbl, .x$mpg >= .y$x, .x$mpg < .y$y) %>%
    #print(n = vec_size(.))