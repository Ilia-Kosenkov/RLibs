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

utils::globalVariables(
    c("CS", "SN", "PSD", "W", "P", "Tau", "PSDN", "Amplitude",
      "CSPhase", "SNPhase", "TCSPhase", "TSNPhase", "FAP", "Wnd"))
#' @title lsa_periodogram_ex
#'
#' @param .data Input data tibble.
#' @param t Time column.
#' @param x Measurement column.
#' @param w (Angular) frequencies
#' @param tau Phase shifts. If \code{NA},
#' \code{RLibs::LSAPhase} is used to obtain values.
#' @return Table containg parameters of the LS periodogram.
#' @importFrom purrr %>% map2
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate select pull
#' @importFrom rlang enquo !!
#' @export
lsa_periodogram_ex <- function(.data, t, x, w, tau = NA) {

    t <- pull(.data, !!enquo(t))
    x <- pull(.data, !!enquo(x))
    x <- x - mean(x)

    if (all(is.na(tau)))
        tau <- lsa_phase(w, t)

    var <- var(x)
    n <- len(.data)

    map2(w, tau, function(loc_w, loc_t) {
        arg <- loc_w * (t - loc_t)
        cs <- cos(arg)
        cs_term <- (sum(x * cs) ^ 2) / sum(cs ^ 2)
        if (abs(loc_w) <= 2 * .Machine$double.eps)
            sn_term <- sum(x * (t - loc_t)) ^ 2 / sum((t - loc_t) ^ 2)
        else {
            sn <- sin(arg)
            sn_term <- sum(x * sn) ^ 2 / sum(sn ^ 2)
        }

        return(list(W = loc_w, Tau = loc_t, CS = cs_term, SN = sn_term))
    }) %>%
    bind_rows %>%
    mutate(
        PSD = 0.5 * (CS + SN), PSDN = PSD / var,
        CSPhase = -atan2(sqrt(SN), sqrt(CS)),
        SNPhase = atan2(sqrt(CS), sqrt(SN)),
        TCSPhase = -W * Tau + CSPhase,
        TSNPhase = -W * Tau + SNPhase,
        Amplitude = sqrt(4 / n * PSD),
        F = W / 2 / pi, P = 1 / `F`,
        FAP = 1 - (1 - exp(-PSDN)) ^ n) %>%
    select(W, `F`, P, Tau, CS, SN, PSD, PSDN, Amplitude, FAP,
        CSPhase, SNPhase, TCSPhase, TSNPhase) %>%
    mutate(Wnd = lsa_periodogram(w, t, rep(1.0, n), tau))

}

LSAPeriodogramEx <- function(.data, t, x, w, tau = NA) {
    lifecycle::deprecate_warn("0.6.2", "RLibs::LSAPeriodogramEx()", "RLibs::lsa_periodogram_ex()")
    lsa_periodogram_ex(.data, t, x, w, tau)
}