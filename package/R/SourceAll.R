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

utils::globalVariables(c("Path", "Length", "Temp"))

#' Sources files in the directory.
#' Useful for the initial project setup.
#'
#' @param path Path where to look for .R files.
#' @param except Regex pattern to ignore files.
#' @param quiet If \code{FALSE}, prints message per each file.
#' @param recursive If \code{TRUE}, goes recurisvely into sub directories.
#'
#' @return Nothing
#' @importFrom fs dir_ls
#' @importFrom purrr %>% discard pwalk map_int
#' @importFrom stringr str_detect str_split str_extract
#' @importFrom glue glue
#' @importFrom tibble enframe
#' @importFrom dplyr mutate arrange desc
#' @export
SourceAll <- function(path, except, quiet = FALSE, recursive = TRUE) {
    srcs <- dir_ls(
           path = path,
           regexp = "\\.R$",
           recursive = recursive)
    if (missing(except) || !nzchar(except))
        except <- "^$"

    srcs %>%
        discard(~str_detect(.x, except)) %>%
        enframe("Temp", "Path") %>%
        select(-Temp) %>%
        mutate(
            Length = map_int(str_split(Path, "[\\\\/]"), length))  %>%
        arrange(desc(Length), Path)  %>%
        mutate(Prints =
            str_extract(Path, "(?<=\\bSource[\\\\/]).*(?=\\.R\\b)")) %>%
        pwalk(function(Path, Length, Prints) {
            if (!quiet)
                message(glue("Sourcing {Prints}..."))
            source(Path)
        })
    invisible(NULL)
}