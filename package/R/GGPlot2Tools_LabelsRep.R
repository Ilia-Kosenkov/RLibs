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

#' @title LabelsRep
#'
#' @param labels Input numeric vector.
#' @param normRange Range within which the numers are
#' represented without exponential notation (fixed).
#' @param isTex If \code{TRUE}, writes output as TeX compatible strings,
#' otherwiase, uses \code{expression}.
#' @param expDigPlc How many digits should be left in the exponential notation.
#' @param omitOnes If \code{TRUE}, all powers of 10 are written
#' without leading 1
#' @param every Samples labels. All other labels, except every \code{every} one
#' are empty strings. Works good when there is no space to put all the labels.
#'
#' @return Textual represntation of the numeric vector.
#' @importFrom dplyr %>%
#' @importFrom magrittr multiply_by divide_by
#' @importFrom purrr map_chr map2_chr
#' @importFrom glue glue
#' @export
#'
LabelsRep <- function(labels,
    normRange = c(-2, 2),
    isTex = FALSE,
    expDigPlc = 0,
    omitOnes = TRUE,
    every = 1) {

    if (isTex)
        expPattern <-
            "${ifelse(.x == 1 & omitOnes, ''," %+%
                " as.character(.x) %+% '\\\\times')} 10^{{{.y}}}$"
    else
        expPattern <-
            "{ifelse(.x == 1 & omitOnes, ''," %+%
                " as.character(.x) %+% '%*%')}10^{.y}"

    digits <- labels %>% abs %>% log10 %>% floor
    decDigits <- digits %>% multiply_by(-1) %>% Clamp(0, + Inf)

    if (!all(WithinL(digits, normRange[1], normRange[2]))) {
        remnants <- labels %>%
            divide_by(10 ^ digits) %>% round(digits = expDigPlc)
        result <- map2_chr(remnants, digits, ~ glue(expPattern))
    }
    else {
        result <- decDigits %>%
            map_chr(~glue("%.{.x}f")) %>%
            map2_chr(labels, sprintf)

    }

    if (isTex) {
        result[-seq(from = 1, to = length(result), by = every)] <- ""
        return(result)
    }
    else {
        result[-seq(from = 1, to = length(result), by = every)] <- "''"
        return(parse(text = result))
    }
}