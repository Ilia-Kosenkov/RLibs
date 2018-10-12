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

#' @title GenerateBreaks
#' @description
#' Generates large and small breaks within given range.
#' @param range Range of breaks (typically a scale of an axis).
#' @param largeStep Step for large breaks (the ones with labels).
#' @param smallStep If specified, used as step for small breaks.
#' @param ticks If \code{smallStep} is not specified, then \code{ticks} are used
#' to create small breaks.
#' @param op Used in combination with \code{ticks} as:
#' per each large break produces
#' \code{op(ticks, break)} small breaks and then limits them to \code{range}
#' @return A list containing either $Large, $Small
#' collections of breaks, or both.
#' @importFrom stats median
#' @export
GenerateBreaks <- function(range, largeStep, smallStep, ticks, op = `*`) {
    result <- list()
    if (!missing(largeStep)) {
        temp <- largeStep *
            (ceiling(range[1] / largeStep):floor(range[2] / largeStep))
        result <- append(result, list(Large = temp))
    }
    if (!missing(smallStep)) {
        temp <- smallStep *
            (floor(range[1] / smallStep):ceiling(range[2] / smallStep))
        result <- append(result, list(Small = Within(temp, range)))
    }
    else if (!missing(ticks) && !missing(largeStep)) {
        sq <- c(min(result$Large) - largeStep,
                result$Large,
                max(result$Large) + largeStep)
        temp <- unlist(lapply(sq, function(x) op(ticks, x)))
        result <- append(result, list(Small = Within(temp, range)))
    }
    else
        reulst <- NULL

    if (!is.null(result$Large)) {
        rangeSh <- Expand(range, factor = -0.05)
        result$Large <- Within(result$Large, rangeSh)
    }

    if ("Large" %in% names(result)) {
        prod <- outer(result$Small, result$Large, function(x, y) abs(x - y)) <
            0.5 * median(diff(result$Small))
        inds <- prod %>%
            as.tibble %>%
            reduce(or) %>%
            which %>% multiply_by(-1)
        result$Small <- result$Small[inds]
    }

    return(result)
}