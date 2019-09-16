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

#' @title JointDistributionContours
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
#' @export
contours_2d <- function(x, y, prob,
    n = 30, lims = c(range(x), range(y))) {
    
    assert_that(passes(is_numeric(x)))
    assert_that(passes(is_numeric(y)))
    assert_that(passes(is_numeric(prob)), prob >= 0, prob <= 1)
    assert_that(passes(is_integer(n)), passes(is_positive(n)))
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

JointDistributionContours <- deprecate_function(JointDistributionContours, contours_2d)

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
    largeSteps <- range %>%
        diff %>% abs %>% log10 %>% floor %>% raise(10)

    # Calculates the number of intervals within range
    # for each modifer and selects the ones
    # that produce the closest to n amount of breaks.
    # If there are multuple matches, selects the smallest step
    # or largest number of breaks
    modInd <- largeSteps %>%
        multiply_by(modifier) %>%
        raise_to_power(-1) %>%
        multiply_by(range %>% diff %>% abs) %>%
        subtract(n) %>%
        abs %>%
        equals(min(.)) %>%
        which

    largeSteps * modifier[modInd] %>% min
}

#' @rdname fancy_step
#' @export
FancyStep <- deprecate_function(FancyStep, fancy_step)

#' @title Clamp
#' @param ... Parameter.
#' @return Clamped numerics.
#' @export
Clamp <- function(...) {
    UseMethod("Clamp")
}

#' @title Clamp.numeric
#' @param ... Input parameters.
#' @return Clamps vector.
#' @export
Clamp.numeric <- function(...) {
    args <- list(...)
    x <- args[[1]]
    lower <- args[[2]]
    upper <- args[[3]]
    if (!missing(lower))
        x[x < lower] <- lower
    if (!missing(upper))
        x[x > upper] <- upper

    return(x)
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
Expand <- deprecate_function(Expand, expand_interval)

#' @title Lin
#' @param x0 Where to interpolate.
#' @param x Arguments (size 2).
#' @param y Values (size 2).
#' @return Interpolated value between two provided.
#' @importFrom purrr map_dbl
#' @importFrom magrittr %<>%
#' @export
Lin <- function(x0, x, y) {
    x %<>% unlist %>% as.numeric
    y %<>% unlist %>% as.numeric

    map_dbl(x0, ~ y[1] + diff(y) / diff(x) * (. - x[1]))
}

#' @title pforeach
#' @param .data An implicit argument passed by a pipe operatpr \code{\%>\%}.
#' @param ... Additional parameters passed to \code{foreach} as is.
#' @return Output of \code{foreach} function that can be piped using
#' operators like \code{\%do\%} and \code{\%dopar\%}.
#' @importFrom foreach foreach
#' @export
pforeach <- function(.data, ...) {
    # Evaluates first (implicit) argument (`dot`)
    # Value of `dot` is reassigned to `dot` to create
    # a `dot` name in the current environment.
    . <- eval(.data)
    # Extracts all additional arguments and evaluates it.
    # All references to `dot` are resolved because `dot`
    # is defiend above
    ._args <- list(...)

    # Constructs a named argument list, first
    # argument is `x` = `input collection`
    ._args <- append(list(x = .), ._args)

    # Executes foreach using do.call and named argument list (all evaluated)
    # foreach generates a descriptor of what to do.
    # Operators like %do% and %dopar% can then process data in accordance to
    # this decription.
    do.call(foreach, ._args)
}

utils::globalVariables(c("Dx", "Dy"))
#' @importFrom rlang enquo quo_squash !!
#' @importFrom dplyr %>% mutate pull
TangentAndNorm <- function(dt, xcol, ycol, t) {
    f <- 1
    xcol <- quo_squash(enquo(xcol))
    ycol <- quo_squash(enquo(ycol))
    t <- quo_squash(enquo(t))

    xData <- dt %>% pull(!!xcol)
    yData <- dt %>% pull(!!ycol)
    tData <- dt %>% pull(!!t)

    n <- nrow(dt)
    dx <- c((xData[2] - xData[1]) / (tData[2] - tData[1]),
      (xData[3:n] - xData[1:(n - 2)]) / (tData[3:n] - tData[1:(n - 2)]),
      (xData[n] - xData[n - 1]) / (tData[n] - tData[n - 1]))

    dy <- c((yData[2] - yData[1]) / (tData[2] - tData[1]),
      (yData[3:n] - yData[1:(n - 2)]) / (tData[3:n] - tData[1:(n - 2)]),
      (yData[n] - yData[n - 1]) / (tData[n] - tData[n - 1]))

    dt %>%
        mutate(Dx = dx / sqrt(dx ^ 2 + dy ^ 2),
               Dy = dy / sqrt(dx ^ 2 + dy ^ 2)) %>%
        mutate(slope = Dy / Dx) %>%
        mutate(x_tn = !!xcol - f * Dx,
               y_tn = !!ycol - f * Dy) %>%
        mutate(x_tn_end = !!xcol + f * Dx,
               y_tn_end = !!ycol + f * Dy) %>%
        mutate(x_nrm = !!xcol + f * Dy,
               y_nrm = !!ycol - f * Dx) %>%
        mutate(x_nrm_end = !!xcol - f * Dy,
               y_nrm_end = !!ycol + f * Dx)

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