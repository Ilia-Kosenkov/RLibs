
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