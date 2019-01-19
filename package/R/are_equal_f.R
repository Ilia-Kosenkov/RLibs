#' are_equal_f
#' @description
#' Checks for floating-point equality of two collections.
#' @param x First floating point collection.
#' @param y Second collection. By default has all zeroes
#' to enable shortcut comparison to zero.
#' @param tol Optional tolerance, defaulted to machine precision.
#'
#' @return Logical vector.
#' @importFrom purrr map2_lgl
#' @export
are_equal_f <- function(x, y = rep(0.0, length(x)), tol = .Machine$double.eps) {
    if (!is_double(x) && !is_integer(x))
        stop("`x` should be either floating point or integer vector.")

    if (!is_double(y) && !is_integer(y))
        stop("`y` should be either floating point or integer vector.")

    if (!is_scalar_double(tol) && !is_scalar_integer(tol))
        stop("`tol` should be a small floating point number.")

    map2_lgl(x, y,
        function(q, u) {
            if (abs(u) < tol)
                abs(q) < tol
            else
                abs(q - u) < max(min(abs(q), abs(u)) * tol, tol)
        })
}