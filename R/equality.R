#' @title equals
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

#' @rdname equals
#' @title Floating-point equality
#'
#' @param e1 LHS
#' @param e2 RHS
#'
#' @return Logical vector
#' @export
`%==%` <- function(e1, e2) {
    if (vec_is_empty(e1) || vec_is_empty(e2))
        return (vec_is_empty(e1) && vec_is_empty(e2))

    if (vctrs::vec_is(e1, complex())) {
        vctrs::vec_recycle_common(!!!vctrs::vec_cast_common(e1, e2)) %->% c(x, y)
        return(are_equal_f(Re(x), Re(y)) & are_equal_f(Im(x), Im(y)))
    }
    if (vctrs::vec_is(e1, double())) {
        vctrs::vec_recycle_common(!!!vctrs::vec_cast_common(e1, e2)) %->% c(x, y)
        return(are_equal_f(x, y))
    }
    return(vctrs::vec_equal(e1, e2))
}


#' @rdname equals
#' @title Floating-point inequality
#'
#' @return Logical vector
#' @export
`%!=%` <- function(e1, e2) {
    !(e1 %==% e2)
}

#' @rdname equals
#' @title Full equality
#' @description Returns \code{logical} of size 1, \code{TRUE} if all elements are equal, otherwise \code{FALSE},
#' @export

`%===%` <- function(e1, e2) {
    all(`%==%`(e1, e2))
}

#' @rdname equals
#'@title Full inequality
#' @description Returns \code{logical} of size 1, \code{TRUE} if any elements are unequal, otherwise \code{FALSE},
#' @export
`%!==%` <- function(e1, e2) {
    any(`%!=%`(e1, e2))
}

#' @title are_same_all
#' @param x Vector to test
#' @param eps Floating-point comparison tolerance
#'
#' @return \code{TRUE} if all elements are equal
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


#' @title unique_which_f
#'
#' @param x Vector to test.
#' @param eps Floating-point comparison tolerance
#'
#' @return Indices of items that are unique within the source vector.
#' @export
unique_which_f <- function(x, eps = 1L) {
    x <- vec_assert_numeric(x)

    prod <- outer(x, x, are_equal_f, eps = eps)

    which(map_int(seq_along(x), ~ sum(prod[1:.x, .x])) %==% 1L)
}

#' @title unique_f
#'
#' @param x Vector to test.
#' @param eps Floating-point comparison tolerance
#'
#' @return Vector of unique items tkaen from the source vector.
#' @export
unique_f <- function(x, eps = 1L) {
    x[unique_which_f(x, eps)]
}

#' @title distinct_which_f
#'
#' @param x First vector.
#' @param y Second vector.
#' @param eps Floating-point comparison tolerance
#'
#' @return List with two items, \code{x} and \code{y}, which contain indices of values that are not present in another collection
#' @export
distinct_which_f <- function(x, y, eps = 1L) {
    x <- vec_assert_numeric(x)
    y <- vec_assert_numeric(y)

    prod <- !outer(x, y, are_equal_f, eps = eps)
    list(x = which(apply(prod, 1, all)), y = which(apply(prod, 2, all)))
}

#' @title distinct_f
#'
#' @param x First vector.
#' @param y Second vector.
#' @param eps Floating-point comparison tolerance
#'
#' @return List with two items, \code{x} and \code{y}, which contain values that are not present in another collection
#' @export
distinct_f <- function(x, y, eps = 1L) {
    ids <- distinct_which_f(x, y, eps)
    list(x = x[ids$x], y = y[ids$y])
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


#' vec_assert_numeric
#' @rdname vec_ext
#' @param x Vector to test
#' @param size Desired size. Can be \code{NULL}.
#' @param arg Arg names, defaults to the name of \code{X}.
#'
#' @return Invisibly returns \code{x} cast to \code{double()}
#' @export
vec_assert_numeric <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (!vctrs::vec_is(x, integer(), size = size) && !vctrs::vec_is(x, double(), size = size))
        vctrs::stop_incompatible_type(x, numeric(), x_arg = arg)

    invisible(vec_cast(x, double(), x_arg = rlang::as_label(x)))
}

#' vec_cast_integerish
#' @rdname vec_ext
#' @param x Vector to test.
#' @param arg Arg name for error message.
#'
#' @return \code{x} cast to \code{integer()} if \code{x} is coercible to \code{integer()}
#' i.e. \code{5.0} is converted to \code{5L}, while \code{5.1} triggers an exception.
#' @export
vec_cast_integerish <- function(x, arg = rlang::as_label(x)) {
    if (vctrs::vec_is(x, integer()))
        return(x)

    if (vctrs::vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vctrs::vec_is_empty(inds))
            return(vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = arg)))

        vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
    }
}

#' vec_assert_integerish
#' @rdname vec_ext
#' @param size Desired size. Can be \code{NULL}.
#'
#' @return Invisibly returns \code{x} cast to \code{integer()}.
#' @export
vec_assert_integerish <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (vctrs::vec_is(x, integer()))
        result <- x

    if (vctrs::vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vctrs::vec_is_empty(inds))
            result <- vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = arg))
        else
            vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
        }
    if (!rlang::is_null(size)) {
        vctrs::vec_assert(result, size = size)
    }

    invisible(result)
}

#' vec_cast_common_flatten
#' @param x Frist argument.
#' @param y Second argument.
#' @param .to Optional target type. If \code{NULL}, common type is inferred.
#'
#' @return List with two elements (\code{x} and \code{y}) that are source collections
#' cast to the common type and flattened.
#' @export
vec_cast_common_flatten <- function(x, y, .to = NULL) {
    if (rlang::is_null(.to))
        .to <- vctrs::vec_ptype_common(!!!x, !!!y)
    list(
         x = vctrs::vec_cast(x, .to),
         y = vctrs::vec_cast(y, .to))
}

#' vec_is_integerish
#' @rdname vec_ext
#' @return Returns \code{TRUE} if the input can be losslessly coerced to \code{integer()}.
#' @export
vec_is_integerish <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (vctrs::vec_is(x, integer(), size))
        return(TRUE)

    if (vctrs::vec_is(x, double(), size)) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vctrs::vec_is_empty(inds))
            return(TRUE)
        else
            return(FALSE)
    }

    return(FALSE)
}