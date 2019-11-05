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

utils::globalVariables(c(".x"))
#' @title lsa_phase
#'
#' @param w (Angular) frequencies.
#' @param t Time.
#'
#' @return Phase shifts in form of a vector that is utilized by the
#' \code{LSAPeriodogram}.
#' @importFrom purrr map_dbl %>%
#' @export
lsa_phase <- function(w, t) {
    # Determines phase correction to Scargle periodogram
    # Args:
    #       w : Frequencies (2 pi nu)
    #       t : Time

    result <- map_dbl(w, ~0.5 / .x * atan2(sum(sin(2 * .x * t)), sum(cos(2 * .x * t))))

    inds <- which(abs(w) < 2 * .Machine$double.eps)

    result[inds] <- mean(t)

    return(result)
}

#' @rdname lsa_phase
#' @export
LSAPhase <- function(w, t) {
    lifecycle::deprecate_warn("0.6.2", "RLibs::LSAPhase()", "RLibs::lsa_phase()")
    lsa_phase(w, t)
}