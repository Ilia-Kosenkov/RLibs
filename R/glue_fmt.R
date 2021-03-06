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

# ADOPTED FROM [glue] package; vignettes

sprintf_transformer <- function(text, envir) {
    str_match(text, "^(.*?)(?::(\\ *%-?.+))?$")[2:3] -> expr

    vals <- eval_tidy(parse_expr(expr[1]), env = envir)

    if (!is.na(expr[2]))
        return(sprintf(expr[2], vals))

    return(vals)
}

#' @title glue_fmt
#'
#' @param ... Passed to \code{glue::glue}.
#' @param .envir Evaluation environment.
#'
#' @return Format-aware interpoalted string.
#' @importFrom glue glue
#' @export
glue_fmt <- function(..., .envir = parent.frame()) {
    glue(..., .envir = .envir, .transformer = sprintf_transformer)
}

#' @title glue_fmt_chr
#'
#' @param ... Passed to \code{glue::glue}.
#' @param .envir Evaluation environment.
#'
#' @return Format-aware interpoalted string.
#' @importFrom glue glue
#' @export
glue_fmt_chr <- function(..., .envir = parent.frame()) {
    as.character(glue(..., .envir = .envir, .transformer = sprintf_transformer))
}

#' @rdname glue_fmt
#' @export
GlueFmt <- function(..., .envir = parent.frame()) {
    lifecycle::deprecate_warn("0.6.1", "RLibs::GlueFmt()", "RLibs::glue_fmt()")
    glue_fmt(..., .envir = .envir)
}