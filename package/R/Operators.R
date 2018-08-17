#' @title \code{is} interfix operator
#' @param object Object to test.
#' @param class Target type (supports \code{rlang} quosure).
#' @description An interfix version of \link{is} method.
#' @return \code{logical} \code{TRUE} if
#' \code{object} is of class \code{class}, \code{FALSE} otherwise.
#' @importFrom rlang quo_squash enquo
#' @export
`%is%` <- function(object, class) {
    nm <- quo_squash(enquo(class))
    if (is.null(nm))
        className <- "NULL"
    else
        className <- a_ch(nm)
    return(is(object, className))
}

#' @title Concat/add interfix operator.
#' @param x Left summand.
#' @param y Right summand.
#' @description Performs (possibly) a vectorized summation operation,
#'  which depends on the class of operators.
#'  Following methods are implemented:
#'  \code{character} + \code{character},
#'        1-to-1 vectorized, concatenation of strings
#' @return Result of the aapropriate summation/concatenation.
#' @importFrom purrr map2_chr
#' @export
`%+%` <- function(x, y) {
    if (x %is% character && y %is% character) {
        if (length(x) == length(y))
            return(map2_chr(x, y, ~ paste0(.x, .y)))
        else
            stop(paste("Unable to concat string collections:",
                "different collection lengths."))
        }
    else
        stop(sprintf(paste0("Unable to add objects of following types:",
            "\r\n[%s]\r\n[%s]\r\nNo rule defined.\r\n"),
            paste(class(x), collapse = ", "),
            paste(class(y), collapse = ", ")))
}