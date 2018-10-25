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


#' @title Tex2Pdf
#' @param source Source files.
#' @param verbose \code{logical}. If \code{TRUE}, prints detailed output.
#' @param additionalParams Passed to 'pdflatex'.
#' @param seps Directory name separators.
#' @export
#' @aliases PlotAPI.Tex2Pdf
#' @importFrom stringr str_split str_replace
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr %>%
#' @importFrom purrr map map_if keep pwalk reduce
#' @importFrom utils tail
Tex2Pdf <- function(source, verbose = FALSE,
    additionalParams = "", seps = c("/", "\\\\")) {
    # Transforms TeX output of tikzDevice into .pdf
    # using 'pdflatex' command.
    # Output file is file with the same name as source,
    # except extention is changed to .pdf
    # Params :
    #   source           : path to .tex file
    #   verbose          : If TRUE, prints detailed output
    #   additionalParams : Passed to 'pdflatex' command
    #   seps             : Path separators

    if (all(nzchar(unlist(additionalParams))))
        params <- glue_collapse(additionalParams, sep = " ")
    else
        params <- ""

    if (!verbose)
        params <- glue("{params} -quiet")

    # Splits strings
    fInfo <- source %>%
        str_split(glue("[{glue_collapse(seps)}]")) %>%
        map(~keep(.x, nzchar)) %>%
        map_if(~length(.x) < 2, ~ c(getwd(), .x)) %>%
        map_if(~.x[1] == ".", ~c(getwd(), .x[-1]))

    fNames <- fInfo %>% map_chr(tail, 1)
    fNamesClean <- fNames %>% str_replace("\\.[a-zA-Z0-9]+?$", "")
    fDir <- fInfo %>%
        map(~glue_collapse(head(.x, -1), sep = .Platform$file.sep))

    list(fNames, fNamesClean, fDir) %>% pwalk(function(nm, nmc, dir) {
        pdflatexCmd <-
            glue("pdflatex -job-name={nmc}",
            " -output-directory=\"{dir}\"",
            " {params} \"{dir}{.Platform$file.sep}{nm}\"")

        rmCmd <- glue("rm {ifelse(verbose, \"-v\", \"\")}")

        rmCmds <- c("aux", "log") %>%
            reduce(~glue("{.x} \"{dir}{.Platform$file.sep}{nmc}.{.y}\""),
                   .init = rmCmd)

        system(pdflatexCmd)
        system(rmCmds)

    })

    return(invisible(NULL))
}

#' @export
#' @importFrom stringr str_sub str_locate_all
#' @importFrom tools file_path_as_absolute
PlotAPI.Tex2Pdf <- function(...) {
    message("[RLibs::PlotAPI.Tex2Pdf] is deprecated, use [RLibs::Tex2Pdf]")
    do.call("Tex2Pdf", ...)
}
