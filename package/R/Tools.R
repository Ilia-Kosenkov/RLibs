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

utils::globalVariables(vctrs::vec_c(".", ".x", ".y"))

#' @title fct_get
#'
#' @param x A factor to convert
#'
#' @return Factor values
#' @importFrom assertthat assert_that
#' @export
fct_get <- function(x) {
    assert_that(passes(is.factor(x)))
    levels(x)[x]
}

#' @export
`%vec_in%` <- vctrs::vec_in
#' @export
cc <- vctrs::vec_c

#' @export
len <- function(x) UseMethod("len")
len.default <- vctrs::vec_size
len.unit <- length
len.quosures <- length

#' @title contours_2d
#' @param x First variable.
#' @param y Second variable.
#' @param prob Vector of probabilities in (0, 1).
#' @param n Size of the \code{MASS::kde2d} estimator's grid.
#' @param lims Explicit lims passed to \code{kde2d}.
#' @param ... For compatibility
#' @return List of contours, one per each probability.
#' @importFrom grDevices contourLines
#' @importFrom purrr map map_dbl map2
#' @importFrom MASS kde2d
#' @importFrom dplyr tibble %>% bind_rows
#' @importFrom rlang is_integerish
#' @export
contours_2d <- function(x, y, prob,
    n = 30, lims = c(range(x), range(y))) {
    
    assert_that(passes(is_numeric(x)))
    assert_that(passes(is_numeric(y)))
    assert_that(passes(is_numeric(prob)), all(prob >= 0), all(prob <= 1))
    assert_that(passes(is_integerish(n)), passes(is_positive(n)))
    assert_that(passes(is_numeric(lims)))
    assert_that(vec_size(x) == vec_size(y), msg = "x and y should be of the same length")

    # Depends on MASS package
    #require(MASS)
    # Calculates 2D density
    dens <- kde2d(x, y, n = n, lims = lims)

    # Aranges values in ascending order
    z <- sort(dens$z)
    # Bin size in x and y directions
    dx <- diff(dens$x[1:2])
    dy <- diff(dens$y[1:2])

    # Returns all cumulative sums:
    # z1; z1+z2; ...; z1+z2+z3+...zN
    # Times 2D bin size
    cz <- cumsum(z) * dx * dy

    # For each prob gives Z
    #levels <- sapply(prob, function(x)
    #{
        #approx(CZ, Z, xout = 1 - x)$y
    #})

    levels <- map(prob, ~approx(cz, z, xout = 1 - .x)$y)


    # Generates contour lines at appropriate levels
    cntrs <- map2(levels, seq_len(length(levels)),
        function(l, prId) {
            contourLines(dens, levels = l) %>%
            map2(seq_len(length(.)),
                ~ tibble(x = .x$x, y = .x$y, prId = prId, lnId = .y)) %>%
            map(~AsSegments(.x, x, y)) %>%
            bind_rows
    }) %>%
    bind_rows

    return(cntrs)
}

#' @rdname contours_2d
#' @export
JointDistributionContours <- function(...) {
    lifecycle::deprecate_stop("0.6.1", "RLibs::JointDistributionContours()")
}

#' @title fancy_step
#' @param range The range within which the ticks are placed.
#' @param n Approximate number of desired ticks.
#' @param modifier Preferred tick placements/
#' @return Returns the size of the step.
#' @importFrom dplyr %>%
#' @importFrom magrittr multiply_by raise_to_power subtract equals
#' @export
fancy_step <- function(range,
    n = 6, modifier = c(1, 2.5, 5)) {

    modifier <- c(0.1 * modifier, modifier)

    # Gets the smallest base_10 step
    large_steps <- range %>%
        diff %>% abs %>% log10 %>% floor %>% raise(10)

    # Calculates the number of intervals within range
    # for each modifer and selects the ones
    # that produce the closest to n amount of breaks.
    # If there are multuple matches, selects the smallest step
    # or largest number of breaks
    mod_ind <- large_steps %>%
        multiply_by(modifier) %>%
        raise_to_power(-1) %>%
        multiply_by(range %>% diff %>% abs) %>%
        subtract(n) %>%
        abs %>%
        equals(min(.)) %>%
        which

    large_steps * modifier[mod_ind] %>% min
}

#' @rdname fancy_step
#' @export
FancyStep <- function(range,
    n = 6, modifier = c(1, 2.5, 5)) {
    lifecycle::deprecate_warn("0.6.1", "RLibs::FancyStep()", "RLibs::fancy_step()")
    fancy_step(range, n, modifier)
}

#' @title Clamp
#' @param ... Parameter.
#' @return Clamped numerics.
#' @importFrom vctrs %0%
#' @export
clamp <- function(...) {
    UseMethod("Clamp")
}

#' @rdname clamp
#' @export
Clamp <- function(...) {
    lifecycle::deprecate_warn("0.6.0", "RLibs::Clamp()", "RLibs::clamp()")
    clamp(...)
}

clamp.numeric <- function(...) {
    args <- rlang::list2(...)
    assertthat::assert_that(len(args) == 2L)
    names <- names(args)
    data_id <- which(names %==% ".data") %0% 1L
    range_id <- which(names %==% ".range") %0% 2L

    data <- args[[data_id]]
    range <- args[[range_id]]

    data[data < range[1]] <- range[1]
    data[data > range[2]] <- range[2]

    return(data)
}


#' @title expand_interval
#' @param x Input interval.
#' @param factor How large the expansion is.
#' 1 corresponds to 100\% increase in size.
#' @param direction In which way to expand.
#' @return Expanded interval.
#' @export
expand_interval <- function(x, factor = 1, direction = c(1, 1)) {
    # Expands provided interval x factor times,
    # preserving the center of the interval
    # Args :
    #   x      : input interval
    #   factor : expanding factor
    # Returns :
    #   Expanded interval

    center <- mean(x)

    halfSize <- (diff(x)) / 2

    return(center + (c(-1, 1) * (1 + factor * direction)) * halfSize)
}

#' @rdname expand_interval
#' @export
Expand <- function(x, factor = 1, direction = c(1, 1)) {
    lifecycle::deprecate_warn("0.6.0", "RLibs::Expand()", "RLibs::Expand()")
    expand_interval(x, factor, direction)
}

#' @title lin
#' @param x Where to interpolate.
#' @param x0 Arguments (size 2).
#' @param y0 Values (size 2).
#' @return Interpolated value between two provided.
#' @importFrom zeallot %->%
#' @export
lin <- function(x, x0, y0) {

    data <- vec_cast_common_flatten(x0, y0)
    vctrs::vec_recycle_common(!!!data, .size = 2L) %->% c(x0, y0)

    dx <- diff(x0)
    dy <- diff(y0)
    sz <- len(x)
    if (sz %==% 0L)
        return(x)
    else if (sz %==% 1L)
        (x - x0[1]) * dy / dx + y0[1]
    else {
        purrr::map(x, ~ (.x - x0[1]) * dy / dx + y0[1]) -> result
        vctrs::vec_cast(result, vctrs::vec_ptype_common(!!!result))
    }
}

#' @rdname lin
Lin <- function(x, x0, y0) {
    lifecycle::deprecate_soft("0.6.0", "RLibs::Lin()", "RLibs::lin()")
    lin(x, x0, y0)
}


#' @title pforeach
#' @param .data An implicit argument passed by a pipe operatpr \code{\%>\%}.
#' @param ... Additional parameters passed to \code{foreach} as is.
#' @return Output of \code{foreach} function that can be piped using
#' operators like \code{\%do\%} and \code{\%dopar\%}.
#' @importFrom foreach foreach
#' @export
pforeach <- function(.data, ...) {
   lifecycle::deprecate_stop("0.6.1", "RLibs::pforeach()")
}

utils::globalVariables("formula")

EvalTrans <- function(trans, data) {
    if (scales::is.trans(trans))
        trans$transform(data)
    else if (rlang::is_formula(trans))
        rlang::as_function(trans)(data)
    else if (rlang::is_function(trans))
        trans(data)
    else
        data
    }


#' @title if_else_weak
#'
#' @param condition Condition to test
#' @param true Returned if \code{condition} is \code{TRUE}
#' @param false Returned otherwise
#' @importFrom assertthat assert_that
#' @importFrom vctrs vec_size vec_recycle
#' @importFrom rlang is_missing is_logical
#' @importFrom magrittr not
#' @export
if_else_weak <- function(condition, true, false) {
    assert_that(passes(is_logical(condition)))
    if (vec_size(condition) == 1L) {
        if (condition) {
            assert_that(not(is_missing(true)))
            true
        }
        else {
            assert_that(not(is_missing(false)))
            false
        }
    }
    else {
        assert_that(not(is_missing(true)), not(is_missing(false)))
        true <- vec_recycle(true, vec_size(condition))
        false <- vec_recycle(false, vec_size(condition))


        false[condition] <- true[condition]
        false
    }
}

utils::globalVariables(vctrs::vec_c("lnId", "prId"))
#' @title contours_2d_df
#' @param df Data frame.
#' @param x First variable.
#' @param y Second variable.
#' @param prob Vector of probabilities in (0, 1).
#' @param n Size of the \code{MASS::kde2d} estimator's grid.
#' @param lims Explicit lims passed to \code{kde2d}.
#' @return List of contours, one per each probability.
#' @importFrom grDevices contourLines
#' @importFrom purrr map map_dbl map2
#' @importFrom MASS kde2d
#' @importFrom dplyr tibble %>% bind_rows
#' @importFrom forcats as_factor fct_cross
#' @export
contours_2d_df <- function(df, x, y, prob,
    n = 30L, lims = NULL) {

    assert_that(passes(is_numeric(prob)), all(prob >= 0), all(prob <= 1))
    assert_that(passes(is_integerish(n)), passes(is_positive(n)), has_size(n, 1L))
    assert_that(either(is_numeric(lims), is_null(lims)))

    if (rlang::is_null(lims))
        lims <- vec_c(range(dplyr::pull(df, {{ x }})), range(dplyr::pull(df, {{ y }})))

    # Depends on MASS package
    #require(MASS)
    # Calculates 2D density
    dens <- kde2d(dplyr::pull(df, {{ x }}), dplyr::pull(df, {{ y }}), n = n, lims = lims)

    # Aranges values in ascending order
    z <- sort(dens$z)
    # Bin size in x and y directions
    dx <- diff(dens$x[1:2])
    dy <- diff(dens$y[1:2])

    # Returns all cumulative sums:
    # z1; z1+z2; ...; z1+z2+z3+...zN
    # Times 2D bin size
    cz <- cumsum(z) * dx * dy

    # For each prob gives Z
    #levels <- sapply(prob, function(x)
    #{
    #approx(CZ, Z, xout = 1 - x)$y
    #})

    levels <- map(prob, ~ approx(cz, z, xout = 1 - .x)$y)


    # Generates contour lines at appropriate levels
    cntrs <- map2(levels, seq_len(length(levels)),
        function(l, prId) {
            contourLines(dens, levels = l) %>%
            map2(seq_len(length(.)),
                ~ tibble(x = .x$x, y = .x$y, prId = prId, lnId = .y)) %>%
            #map(~AsSegments(.x, x, y)) %>%
            bind_rows
        }) %>%
    bind_rows %>%
            mutate_at(vars(prId, lnId), as_factor) %>%
            mutate(glId = fct_cross(lnId, prId))

    return(cntrs)
}