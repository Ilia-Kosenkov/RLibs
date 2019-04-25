#   MIT License
#
#   Copyright(c) 2017-2019 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
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

table_2_tex <- function(
    data,
    path,
    format = NULL,
    col_layout = "c",
    insert_math_body = TRUE,
    insert_math_header = TRUE,
    print_header = TRUE,
    add_hlines = TRUE,
    append = FALSE) {

    expand_argument <- function(arg, names, default = NULL) {
        if (rlang::is_null(arg)) {
            if (rlang::is_null(default))
                return(rlang::set_names(rep(list(default), length(names)), names))
            return(rlang::set_names(rlang::as_list(rep(default, length(names))), names))
        }
        if (rlang::is_scalar_atomic(arg)) {
            return(rlang::set_names(rlang::as_list(rep(arg, length(names))), names))
        }
        if (rlang::is_atomic(arg)
            && length(arg) == length(names)) {
            return(rlang::set_names(rlang::as_list(arg), names))
        }
        if (rlang::is_list(arg)
            && length(arg) == length(names)
            && purrr::every(arg, rlang::is_scalar_atomic)) {
            if (rlang::is_named(arg))
                return(arg[names])
            return(rlang::set_names(arg, names))
        }

        return (NULL)
    }

    get_format <- function(type) {
        switch(type,
               "integer" = "d",
               "double" = "g",
               "s")
    }

    indent <- function(str, n) {
        paste0(paste0(rep(" ", n), collapse = ""), str)
    }

    # Arguments' contracts
    assertthat::assert_that(is.data.frame(data))
    assertthat::is.string(path)
    data %>% walk(~assertthat::assert_that(rlang::is_atomic(.x)))
    assertthat::assert_that(rlang::is_scalar_logical(print_header))
    assertthat::assert_that(rlang::is_scalar_logical(add_hlines))
    assertthat::assert_that(rlang::is_scalar_logical(append))

    nms <- names(data)
    col_layout <- expand_argument(col_layout, nms, "c")
    format <- expand_argument(format, nms)
    insert_math_body <- expand_argument(insert_math_body, nms, FALSE)
    insert_math_header <- expand_argument(insert_math_header, nms, FALSE)
    col_types <- data %>% map(typeof)

    format <- purrr::map2(format, col_types, function(f, t) {
            if (rlang::is_null(f)) 
                return(paste0("%8", get_format(t)))
            return(f)
        }) %>%
        stringr::str_match("^\\%([0-9]+)?(?:\\.([0-9]+))?(\\w+)$") %>%
        tibble::as_tibble(.name_repair = identity) %>%
        rlang::set_names("Source", "Padding", "Precision", "Type") %>%
        dplyr::mutate(Name = nms,
               Padding = dplyr::if_else(is.na(Padding), "8", Padding),
               InsertMathBody = purrr::flatten_lgl(insert_math_body),
               InsertMathHeader = purrr::flatten_lgl(insert_math_header)) %T>% print



    line_format <- format %>%
        dplyr::select(Source, Name, InsertMathBody) %>%
        pmap(~glue::glue("{if(..3) '$' else ''}{{{..2}: {..1}}}{if(..3) '$' else ''}")) %>%
        glue::glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_format <- format %>%
        dplyr::select(Padding, Name, InsertMathHeader) %>%
        pmap(~glue::glue("{if(..3) '$' else ''}{{{..2}: %{..1}s}}{if(..3) '$' else ''}")) %>%
        glue::glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_string <- RLibs::GlueFmt(header_format, .envir = set_names(as_list(nms)))

    append_next <- append

    table_frmt <- paste0(col_layout, collapse = "")

    offset <- 0L
    readr::write_lines(indent(glue::glue("\\begin{{tabular}}{{{table_frmt}}}"), offset), path, append = append_next)
    append_next <- TRUE

    if (add_hlines) {
        offset <- offset + 4L
        readr::write_lines(indent("\\hline", offset), path, append = append_next)
    }
    

    offset <- offset + 4L
    if (print_header) 
        readr::write_lines(indent(header_string, offset), path, append = append_next)

    offset <- offset - 4L
    if (add_hlines && print_header) 
        readr::write_lines(indent("\\hline", offset), path, append = append_next)
    

    offset <- offset + 4L
    if (print_header)
        readr::write_lines(indent("", offset), path, append = append_next)

    data %>% purrr::pmap_chr(function(...) {
                RLibs::GlueFmt(indent(line_format, offset), .envir = list(...))
            }) %>%
        readr::write_lines(path, append = append_next)

    if (add_hlines) {
        offset <- offset - 4L
        readr::write_lines(indent("\\hline", offset), path, append = append_next)
    }

    offset <- offset - 4L
    readr::write_lines(indent("\\end{tabular}", offset), path, append = append_next)
}

 mtcars %>% table_2_tex("test.tex", add_hlines = T, print_header = F) %>% print