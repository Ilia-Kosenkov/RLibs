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

#' @title GenerateLog10Breaks
#' @param ylim Axis limit.
#' @param ticks If the limit spans over several orders,
#' \code{ticks} are used to generate large breaks.
#' @return log10 breaks.
#' @importFrom magrittr is_weakly_less_than extract2 multiply_by add
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @export
GenerateLog10Breaks <- function(ylim, ticks = c(0.2, 0.5, 1, 2, 5)) {

    if (ylim %>% log10 %>% diff %>% is_weakly_less_than(1)) {
        mult <- ylim %>% min %>% Log10Floor
        yInt <- ylim / mult

        lStep <- yInt %>% FancyStep(4)
        sStep <- 0.1 * lStep

        yRng <- yInt %>% RoundIntervalTo(lStep)

        breaks <- list(
            Large = seq(yRng[1], yRng[2], by = lStep),
            Small = seq(yRng[1], yRng[2], by = sStep))

        inds <- Intersect(breaks$Large, breaks$Small, tol = 0.5 * sStep) %>%
            extract2(2)

        breaks$Small <- breaks$Small[setdiff(1:length(breaks$Small), inds)]

        breaks <- breaks %>%
            map(Within, range = yInt) %>%
            map(multiply_by, mult)

    } else {

        breaks <- ylim %>% log10 %>%
        GenerateBreaks(largeStep = 1,
            ticks = log10(1:9), op = add)

        breaks$Large <- breaks$Large %>%
            map(~log10(ticks) + .x) %>%
            unlist %>%
            UniqueTol(tol = 5 * .Machine$double.eps) %>%
            Within(range = log10(ylim))

        inds <- Intersect(breaks$Large, breaks$Small, tol = 1e-4)[[2]]

        breaks$Small <- breaks$Small[setdiff(1:length(breaks$Small), inds)]

        breaks <- breaks %>%
            map(Within, range = log10(ylim)) %>%
            map(raise, 10)
    }

    return(breaks)
}