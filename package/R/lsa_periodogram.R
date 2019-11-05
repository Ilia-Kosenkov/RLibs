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

#' @title lsa_periodogram
#'
#' @param w (Angular) frequencies.
#' @param t Moments of time at which \code{x} were observed.
#' @param x Observations
#' @param tau If \code{NA}, \code{LSAPhase} is used to calculate phases.
#' Can be used to caches values for a given set of \code{w} and \code{t}.
#'
#' @return Numeric vector of length equal to length of \code{w}.
#' @importFrom purrr map2_dbl
#' @export
lsa_periodogram <- function(w, t, x, tau = NA) {
    # Calculates scargle periodogram
    # Args:
    #   w   : Frequencies (2 pi nu)
    #   t   : Time
    #   x   : Measurements
    #   tau : Phase correction. If NA, it is calculated

    if (all(is.na(tau)))
        tau <- lsa_phase(w, t)

    map2_dbl(w, tau, function(loc_w, loc_t) {
        arg <- loc_w * (t - loc_t)
        cs <- cos(arg)
        cs_term <- (sum(x * cs) ^ 2) / sum(cs ^ 2)
        if (abs(loc_w) <= 2 * .Machine$double.eps)
            sn_term <- sum(x * (t - loc_t)) ^ 2 / sum((t - loc_t) ^ 2)
        else {
            sn <- sin(arg)
            sn_term <- sum(x * sn) ^ 2 / sum(sn ^ 2)
        }

        return(0.5 * (cs_term  + sn_term))
    })

}

#' @rdname lsa_periodogram
#' @export
LSAPeriodogram <- function(w, t, x, tau = NA) {
    lifecycle::deprecate_warn("0.6.2", "RLibs::LSAPeriodogram()", "RLibs::lsa_periodogram()")
    lsa_periodogram(w, t, x, tau)
}