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

#' @title labels_rep
#' @rdname labels_rep
#' @param labels Input numeric vector.
#' @param norm_range Range within which the numers are
#' represented without exponential notation (fixed).
#' @param is_tex If \code{TRUE}, writes output as TeX compatible strings,
#' otherwiase, uses \code{expression}.
#' @param exp_dig_plc How many digits should be left in the exponential notation.
#' @param omit_ones If \code{TRUE}, all powers of 10 are written
#' without leading 1
#' @param every Samples labels. All other labels, except every \code{every} one
#' are empty strings. Works good when there is no space to put all the labels.
#' @param same_digit_count If \code{TRUE}, enforces same amount of
#' dec. digits across all items.
#'
#' @return Textual represntation of the numeric vector.
#' @importFrom dplyr %>%
#' @importFrom magrittr multiply_by divide_by
#' @importFrom purrr map_chr map2_chr
#' @importFrom glue glue
#' @export
#'
labels_rep <- function(labels,
    norm_range = c(-2, 2),
    is_tex = FALSE,
    exp_dig_plc = 0,
    omit_ones = TRUE,
    every = 1,
    same_digit_count = FALSE) {

    if (is_tex)
        exp_pattern <-
            paste0("${ifelse(.x == 1 & omit_ones, '',",
                " as.character(.x) %&% '\\\\times')} 10^{{{.y}}}$")
    else
        exp_pattern <-
            paste0("{ifelse(.x == 1 & omit_ones, '',",
                " as.character(.x) %&% '%*%')}10^{.y}")

    digits <- labels %>% abs %>% log10 %>% floor
    dec_digit <- digits %>% multiply_by(-1) %>% Clamp(0, + Inf)
    if (same_digit_count)
        dec_digit <- rep(max(dec_digit), length(dec_digit))
    if (!all(WithinL(digits, norm_range[1], norm_range[2]))) {
        remnants <- labels %>%
            divide_by(10 ^ digits) %>% round(digits = exp_dig_plc)
        result <- map2_chr(remnants, digits, ~ glue(exp_pattern))
    }
    else {
        result <- dec_digit %>%
            map_chr(~glue("%.{.x}f")) %>%
            map2_chr(labels, sprintf)

    }

    if (is_tex) {
        result[-seq(from = 1, to = length(result), by = every)] <- ""
        return(result)
    }
    else {
        result[-seq(from = 1, to = length(result), by = every)] <- "''"
        return(parse(text = result))
    }
}

#' @rdname labels_rep
#' @param ... Parameter list
#' @export
LabelsRep <- function(...) {
    labels_rep(...)
}