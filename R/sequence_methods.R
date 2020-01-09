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

#' @title List_of mappers
#' @rdname map_lo
#' @param .x,.y,.f,.l Parameters passed to respective \code{purrr} method
#' @param ... Additional parameters
#' @return A \code{vctrs::list_of} instead of a regular list.
#' @export
map_lo <- function(.x, .f, ...) {
    as_list_of(map(.x, .f, ...))
}

#' @rdname map_lo
#' @export
imap_lo <- function(.x, .f, ...) {
    as_list_of(imap(.x, .f, ...))
}

#' @rdname map_lo
#' @export
map2_lo <- function(.x, .y, .f, ...) {
    as_list_of(map2(.x, .y, .f, ...))
}

#' @rdname map_lo
#' @export
pmap_lo <- function(.l, .f, ...) {
    as_list_of(pmap(.l, .f, ...))
}


#' @title seq_int_len
#' @param length.out Size of the sequence.
#' @return Sequence of ints.
#' @export
seq_int_len <- function(length.out) {
    seq.int(length.out = length.out)
}

#' @title seq_int_along
#' @param along.with Sequence along which indexes are genereated.
#' @return Sequence of ints.
#' @export
seq_int_along <- function(along.with) {
    seq.int(along.with = along.with)
}

#' @title sec_len
#' @param x Object of some length (a list, a vector, etc)
#' @return Integer sequence from 1 to \code{length(x)}
#' @export
sec_len <- function(x)
    seq.int(length.out = length(x))

#' @title raise
#' @param x Power.
#' @param y Base.
#' @return \code{y ^ x}
#' @export
raise <- function(x, y) y ^ x

#' @title Within
#' @description
#' Returns a subset of x such as x is
#' within [min(range), max(range)].
#' @param x Input subset.
#' @param range Range within which elements of x should be.
#' @return Subset of source vector x.
#' @export
Within <- function(x, range) {
    min <- min(range, na.rm = TRUE)
    max <- max(range, na.rm = TRUE)

    return(x[x >= min & x <= max])
}

#' @title Order
#' @description Returns ordered collection.
#' @param x Input collection.
#' @return Ordered \code{x}
#' @export
Order <- function(x) {
    x[order(x)]
}


#' @title WithinL
#' @description Returns \code{TRUE}/\code{FALSE} vector indicating
#' which elements are within the range.
#' @param x Input collection.
#' @param low Lower boundary.
#' @param upp Upper boundary.
#' @return Logical vector.
#' @export
WithinL <- function(x, low, upp) {
    x >= low & x <= upp
}

#' @title Intersect
#' @param x First vector.
#' @param y Second vector.
#' @param tol The tolerance level.
#' @return Indices of first and second vector.
#' These elements are found to be equal within \code{tol}
#' @importFrom magrittr subtract is_weakly_less_than
#' @importFrom dplyr %>%
#' @export
Intersect <- function(x, y, tol = 0.1) {
    x %>% outer(y, subtract) %>%
        abs %>%
        is_weakly_less_than(tol) %>%
        which(arr.ind = TRUE) -> indices

    return(list(indices[, 1], indices[, 2]))
}

#' @title UniqueWhichTol
#' @param x Vector to check.
#' @param tol Tolerance level for comparisons.
#' @return Indices of unique elements within given tlerance.
#' @importFrom dplyr %>%
#' @importFrom magrittr subtract is_less_than extract
#' @importFrom purrr map map2 map_lgl
#' @export
UniqueWhichTol <- function(x, tol = .Machine$double.eps) {
    x %>%
        outer(x, subtract) %>%
        abs %>%
        is_less_than(tol) %>% {
            map(seq_int_len(length(x)),
                function(x) extract(., x,))
        } %>%
        map2(seq_int_len(length(x)), ~ c(.y, which(.x))) %>%
        map_lgl(~.x[2] == .x[1]) %>%
        which
}

#' @title UniqueTol
#' @param x Vector to check.
#' @param tol Tolerance level for comparisons.
#' @return Unique elements of the vector.
#' @importFrom dplyr %>%
#' @importFrom magrittr extract
#' @export
UniqueTol <- function(x, tol = .Machine$double.eps) {
    x %>%
        extract(UniqueWhichTol(., tol))
}

#' @title BetweenWhich
#' @param x Ordered vector.
#' @param x0 Value to find.
#' @importFrom purrr map_lgl map
#' @importFrom rlang is_empty
#' @importFrom dplyr %>%
#' @export
BetweenWhich <- function(x, x0) {
    1:(length(x) - 1) %>%
        map_lgl(~(x[.x] <= x0 & x0 < x[.x + 1]) |
                 (x[.x + 1] < x0 & x0 <= x[.x])) %>%
        which %>% {
            if (is_empty(.))
                NA
            else
                map(., ~ .x + 0:1)
            }
}

#' @title a_ch
#' @description A shortcut to \code{as.character}.
#' @param ... Arguments to convert.
#' @return Character representation of arguments.
#' @importFrom purrr map_chr
#' @export
a_ch <- function(...) map_chr(list(...), as.character)

#' @title Log10Floor
#' @param x Input numeric vector.
#' @return Closest power of 10 that is smaller than or equal to the number.
#' @importFrom dplyr %>%
#' @export
Log10Floor <- function(x) {
    x %>% log10 %>% floor %>% raise(10)
}

#' @title Log10Ceiling
#' @param x Input numeric vector.
#' @return Closest power of 10 that is greater than or equal to the number.
#' @importFrom dplyr %>%
#' @export
Log10Ceiling <- function(x) {
    x %>% log10 %>% ceiling %>% raise(10)
}

#' @title RoundIntervalTo
#' @param x An input oredered vector of size 2 (Interval lims).
#' @param rnd Rounding base.
#' @return A vector of size 2, both limits of which are powers of \code{rnd}
#' @export
RoundIntervalTo <- function(x, rnd) {
    rnd * c(floor(x[1] / rnd), ceiling(x[2] / rnd))
}