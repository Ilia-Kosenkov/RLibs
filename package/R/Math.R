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