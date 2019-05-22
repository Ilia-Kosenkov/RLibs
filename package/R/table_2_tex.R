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

utils::globalVariables(c("Padding", "Precision", "BodyFormat", "InsertMathBody", 
    "HeaderFormat", "InsertMathHeader", "AddonFormat", "Name"))

#' @export
#' @importFrom rlang set_names as_list is_scalar_atomic is_list is_named are_na 
#' @importFrom rlang is_scalar_character is_scalar_logical as_character is_na
#' @importFrom stringr str_match
#' @importFrom assertthat assert_that is.string
#' @importFrom readr parse_integer
#' @importFrom purrr flatten_lgl pmap pmap_chr
table_2_tex <- function(
    data,
    path,
    format = NULL,
    col_layout = "c",
    insert_math_body = FALSE,
    insert_math_header = FALSE,
    print_header = TRUE,
    add_hlines = TRUE,
    before = NULL,
    after = NULL,
    custom_header = NULL,
    append = FALSE) {

    expand_argument <- function(arg, names, default = NULL) {
        if (is_null(arg)) {
            if (is_null(default))
                return(set_names(rep(list(default), length(names)), names))
            return(set_names(as_list(rep(default, length(names))), names))
        }
        if (is_scalar_atomic(arg)) {
            return(set_names(as_list(rep(arg, length(names))), names))
        }
        if (is_atomic(arg)
            && length(arg) == length(names)) {
            return(set_names(as_list(arg), names))
        }
        if (is_list(arg)
            && length(arg) == length(names)
            && every(arg, is_scalar_atomic)) {
            if (is_named(arg)) {
                if (all(names(arg) %in% names))
                    return(arg[names])
                return(NULL)
            }
            return(set_names(arg, names))
        }

        stop("Invalid argument")
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

    process_extra_headers <- function(input, format_tbl, header_format) {
        if (is_null(input))
            return(input)
        if (is_string(input)) {
            return(input)
        }

        if (is_character(input)
            && length(input) == nrow(format_tbl))
                return(GlueFmt(header_format, .envir = set_names(as_list(input), format_tbl$Name)))

        if (is_list(input)
            && length(input) == nrow(format_tbl)
            && every(input, is_scalar_character)) {

            if (is_named(input) && all(names(input) %in% format_tbl$Name))
                return(GlueFmt(header_format, .envir = input))
            else
                return(GlueFmt(header_format, .envir = set_names(input, format_tbl$Name)))
            }

        return(NULL)

    }

    assert_that(is.data.frame(data))
    is.string(path)
    data %>% walk(~assert_that(is_atomic(.x)))
    assert_that(is_scalar_logical(print_header))
    assert_that(is_scalar_logical(add_hlines))
    assert_that(is_scalar_logical(append))

    nms <- names(data)
    col_layout <- expand_argument(col_layout, nms, "c")
    format <- expand_argument(format, nms)
    insert_math_body <- expand_argument(insert_math_body, nms, FALSE)
    insert_math_header <- expand_argument(insert_math_header, nms, FALSE)
    custom_header <- expand_argument(custom_header, nms)
    custom_header <- map2(custom_header[nms], nms, ~.x %||% .y) 
    col_types <- data %>% map(typeof)

    format <- map2(format, col_types, function(f, t) {
        if (is_null(f))
            return(paste0("%8", get_format(t)))
        return(f)
    }) %>%
        str_match("^\\%([0-9]+)?(?:\\.([0-9]+))?(\\w+)$") %>%
        as_tibble(.name_repair = identity) %>%
        set_names("Source", "Padding", "Precision", "Type") %>%
        mutate(
            Name = nms,
            Padding = parse_integer(Padding),
            Padding = if_else(is.na(Padding), 8L, Padding),
            Precision = parse_integer(Precision),
            InsertMathBody = flatten_lgl(insert_math_body),
            InsertMathHeader = flatten_lgl(insert_math_header)) %>%
        mutate(
            PrecisionStr = if_else(is.na(Precision), "", as_character(glue(".{Precision}"))),
            BodyFormat =
               glue("%{if_else(InsertMathHeader & !InsertMathBody, Padding + 2L, Padding)}{PrecisionStr}{Type}"),
            HeaderFormat =
                glue("%{if_else(InsertMathBody & !InsertMathHeader, Padding + 2L, Padding)}s"),
            AddonFormat =
                glue("%{if_else(InsertMathBody | InsertMathHeader, Padding + 2L, Padding)}s"))




    line_format <- format %>%
        select(BodyFormat, Name, InsertMathBody) %>%
        pmap(~glue("{if(..3) '$' else ''}{{{..2}:{..1}}}{if(..3) '$' else ''}")) %>%
        glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_format <- format %>%
        select(HeaderFormat, Name, InsertMathHeader) %>%
        pmap(~glue("{if(..3) '$' else ''}{{{..2}:{..1}}}{if(..3) '$' else ''}")) %>%
        glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    plain_header_format <- format %>%
        select(AddonFormat, Name) %>%
        pmap(~glue("{{{..2}:{..1}}}")) %>%
        glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_string <- GlueFmt(header_format, .envir = custom_header)

    append_next <- append

    table_frmt <- paste0(col_layout, collapse = "")

    before <- process_extra_headers(before, format, plain_header_format)
    after <- process_extra_headers(after, format, plain_header_format)

    offset <- 0L
    offset_step <- 4L
    output <- indent(glue("\\begin{{tabular}}{{{table_frmt}}}"), offset)

    if (add_hlines) {
        offset <- offset + offset_step
        output %<>% c(indent("\\hline", offset))
    }


    offset <- offset + offset_step
    if (!is_null(before))
        output %<>% c(indent(before, offset))

    if (print_header)
        output %<>% c(indent(header_string, offset))

    if (!is_null(after))
        output %<>% c(indent(after, offset))

    offset <- offset - offset_step
    if (add_hlines && print_header)
        output %<>% c(indent("\\hline", offset))


    offset <- offset + offset_step

    if (print_header)
        output %<>% c(indent("", offset))

    output %<>% c(data %>% pmap_chr(function(...) {
        GlueFmt(indent(line_format, offset), .envir = list(...))
    }))


    if (add_hlines) {
        offset <- offset - offset_step
        output %<>% c(indent("\\hline", offset))
    }

    offset <- offset - offset_step
    output %<>% c(indent("\\end{tabular}"))

    write_lines(output, path, append = append)

    invisible(NULL)
}