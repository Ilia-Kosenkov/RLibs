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

utils::globalVariables(c("Ext", "FileName"))

#' @title Tex2Pdf
#' @param ... Source files.
#' @param verbose \code{logical}. If \code{TRUE}, prints detailed output.
#' @param additionalParams Passed to 'pdflatex'.
#' @export
#' @importFrom tibble tibble
#' @importFrom rlang flatten_chr is_empty
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr %>% pull
#' @importFrom purrr map pwalk walk
#' @importFrom fs path_dir path_ext_remove path_ext file_exists
#' @importFrom fs path_norm path_ext_set path_file file_delete
#' @importFrom vctrs vec_c vec_size
#'
tex_2_pdf <- function(..., verbose = FALSE,
    additionalParams = "") {

    source <- flatten_chr(list(...))

    params <- glue_collapse(additionalParams, sep = " ")

    if (!verbose)
        params <- glue("{params} -quiet")

    fInfo <- tibble(
            Folder = path_dir(source),
            FileName = path_ext_remove(path_file(source)),
            Ext = path_ext(source),
            Tex = path_norm(source))

    invalidFiles <- fInfo %>%
        filter(tolower(Ext) != "tex" && (!file_exists(FileName))) %>%
        pull(FileName)

    if (!is_empty(invalidFiles)) {
        lim <- 4L
        error <- glue("[{head(invalidFiles, lim)}]") %>%
            glue_collapse(sep = ", ")
        extraMsg <- if_else_weak(vec_size(invalidFiles) > lim,
            glue(" and {length(invalidFiles) - lim} more"), "")
        stop(glue("Illegal files: {error}{extraMsg}."))
    }

    pwalk(fInfo, function(Folder, FileName, Ext, Tex) {
        if (verbose)
            message(glue("Running `pdflatex` for \"{FileName}.tex\"..."))

        pdflatexCmd <-
            glue("pdflatex -job-name={FileName}",
                " -output-directory=\"{Folder}\"",
                " {params} \"{Tex}\"")

        system(pdflatexCmd, show.output.on.console = verbose)

        rmFiles <- vec_c("aux", "log") %>%
            map(~glue("{path_ext_set(Tex, .x)}"))

        rmFiles %>%
            walk(function(f) {
                if (verbose)
                    message(glue("Removing {f}..."))
                file_delete(f)
            })

    })
    return(invisible(NULL))
}

#' @rdname tex_2_pdf
#' @export
Tex2Pdf <- deprecate_function(Tex2Pdf, tex_2_pdf)