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

M.Drv = function(func, x0, eps = 1e-6) {
    if(is.list(func))
    if (!is.function(func) && !all(sapply(func, is.function)))
        stop("Parameter 'func' is not a function or vector of functions.")

    if (!is.numeric(x0))
        stop("Parameter 'x0' is not a number.")
    else if (length(x0) != 1)
        stop("Cannot vectorize over 'x0'.")

    x0 = as.numeric(x0)

    if (!is.numeric(eps))
        stop("Parameter 'eps' is not a number.")
    else if (length(eps) != 1)
        stop("Cannot vectorize over 'eps'.")
    else if (as.numeric(eps) <= .Machine$double.eps)
        stop(sprintf("Value of 'eps' is smaller than minimum platform epsilon %e.", .Machine$double.eps))

    eps = as.numeric(eps)

    worker = function(f) (f(x0 + eps) - f(x0 - eps)) / (2 * eps)
    result = if(is.function(func)) worker(func) else unlist(lapply(func, worker))

    return(result)
}


M.Orth2 = function(vec) {
    if (!all(is.numeric(vec)))
        stop("Parameter 'vec' is not a number.")
    else if (length(vec) != 2)
        stop("Parameter 'vec' should be a vector of length 2.")

    vec = as.numeric(vec)

    return(c(-vec[2], vec[1]))
}

#' @title Math.NormalizeVector
#' @description 
#' \code{M.Norm} normalizes vector, reducing its length in terms of Euclidian norm to 1.
#' @details 
#' Divides all source vector elements by a positive quantity selected in such a way that Euclidian norm of 
#' the resulting vector is 1. Requires a numerical structure coercible to 1D array.
#' @param vec Input vector (or similar structure, e.g. N x 1 matrix).
#' @return  A unit vector parallel to the source one.
#' @export
M.Norm = function(vec) {
    if (!all(is.numeric(vec)))
        stop("Parameter 'vec' is not a number.")
    else if (length(dim(vec)) > 1 && ((sum(dim(vec) == 1) != length(dim(vec)) - 1)))
        stop("Parameter 'vec' cannot be coerced to 1D vector.")

    vec = as.numeric(vec)

    return (vec / sqrt(sum(vec^2)))
}

M.IsReal = function(z) {
    # TODO: check z for proper type
    return(Re(z) == Mod(z))
}

M.IsImaginary = function(z) {
    return(Im(z) == Mod(z))
}
    
