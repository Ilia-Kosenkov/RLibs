#' @import vctrs, assertthat, rlang, purrr


#' are_equal_f
#'
#' @param x LHS
#' @param y RHS
#' @param eps Precision of floating-point comparison
#'
#' @return Logical vector
#' @export
#' @importFrom zeallot %->%
are_equal_f <- function(x, y, eps = 1) {
    eps <- vec_assert_numeric(eps, size = 1L)
    assertthat::assert_that(eps >= 0)

    vec_cast_common_flatten(x, y) %->% c(x, y)

    vec_assert_numeric(x, arg = name_of(x))
    vec_assert_numeric(y, arg = name_of(y))

    vctrs::vec_recycle_common(x, y) %->% c(x, y)

    comparator <- function(p, q) {
        if (rlang::is_na(p) || rlang::is_na(q))
            return(FALSE)

        if (is.infinite(p) || is.infinite(q))
            return(p == q)

        if (p == q)
            return(TRUE)

        p_abs <- abs(p)
        q_abs <- abs(q)
        diff <- abs(p - q)

        delta <- eps * .Machine$double.eps
        # According to IEEE-754 https://en.wikipedia.org/wiki/IEEE_754
        # -0 and 0 are equal, therefore p_abs and q_abs
        # cannot be 0 at the same time
        if (p_abs == 0 && q_abs == 0)
            rlang::abort("Should not happen", "RLibs_should_not_happen")
        else if (p_abs == 0)
            fact <- q_abs
        else if (q_abs == 0)
            fact <- p_abs
        else
            fact <- min(cc(p_abs, q_abs))

        return(diff < fact * delta)
    }

    purrr::map2_lgl(x, y, comparator)
}

#' @export
`%==%` <- function(e1, e2) {
    vctrs::vec_recycle_common(!!!vctrs::vec_cast_common(e1, e2)) %->% c(x, y)
    if (vctrs::vec_is(x, complex()))
        return(are_equal_f(Re(x), Re(y)) & are_equal_f(Im(x), Im(y)))
    if (vctrs::vec_is(x, double()))
        return(are_equal_f(x, y))
    return(x == y)
}

#' Floating-point equality
#'
#' @param e1 LHS
#' @param e2 RHS
#'
#' @return Logical vector
#' @export
`%!=%` <- function(e1, e2) {
    !(e1 %==% e2)
}

#' Floating-point inequality
#'
#' @param e1 LHS
#' @param e2 RHS
#'
#' @return Logical vector
#' @export
are_same_all <- function(x, eps = 1) {
    # `eps` is tested in `are_equal_f`
    x <- vec_assert_numeric(x, arg = name_of(x))

    if (vec_size(x) == 0L)
        return(FALSE)
    if (vec_size(x) == 1L)
        return(TRUE)

    all(are_equal_f(vctrs::vec_slice(x, 1L), vctrs::vec_slice(x, 2L:vctrs::vec_size(x)), eps = eps))
}

#' name_of
#'
#' @param x Argument
#'
#' @return Text representation of symbol
#' @export
name_of <- function(x) {
    y <- rlang::expr_name(rlang::enexpr(x))
}


vec_assert_numeric <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (!vctrs::vec_is(x, integer(), size = size) && !vctrs::vec_is(x, double(), size = size))
        vctrs::stop_incompatible_type(x, numeric(), x_arg = arg)

    invisible(vec_cast(x, double(), x_arg = rlang::as_label(x)))
}

vec_cast_integerish <- function(x, x_arg = "x") {
    if (vec_is(x, integer()))
        return(x)

    if (vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            return(vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = rlang::as_label(x))))

        vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
    }
}

vec_assert_integerish <- function(x, size = NULL, x_arg = "x") {
    if (vec_is(x, integer()))
        result <- x

    if (vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            result <- vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = rlang::as_label(x)))
        else
            vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
        }
    if (!rlang::is_null(size)) {
        vctrs::vec_assert(result, size = size)
    }

    invisible(result)
}

vec_cast_common_flatten <- function(x, y, .to = NULL) {
    if (rlang::is_null(.to))
        .to <- vctrs::vec_type_common(!!!x, !!!y)
    list(
         x = vctrs::vec_cast(x, .to),
         y = vctrs::vec_cast(y, .to))
}