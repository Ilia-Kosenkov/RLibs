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
    before = NULL,
    after = NULL,
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
            if (rlang::is_named(arg)) {
                if(all(names(arg) %in% names))
                    return(arg[names])
                return (NULL)
            }
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

    process_extra_headers <- function(input, format_tbl, header_format) {
        if (rlang::is_null(input))
            return(input)
        if (rlang::is_string(input)) {
            str_rep <- stringr::str_match(input, "^(.*?)(?:(?<!\\\\)%(.*))?$")[2:3]

            if (all(rlang::are_na(str_rep))) {
                warning("Inconsistent extra hedare string. Ignoring it in the output.")
                return(NULL)
            }
            if (rlang::is_na(str_rep[1]))
                return(paste0("%", str_rep[2]))

            eol_test <- stringr::str_match_all(str_rep[1], "(?:(\\\\)\\s*$|(\\\\)|^([^\\\\]*)$)") %>%
                magrittr::extract2(1) %>% {
                    list(ExtraEOL = any(!are_na(.[1:nrow(.), 3])), EOL = any(!are_na(.[1:nrow(.), 2])))
                }
            if (eol_test$ExtraEOL) {
                warning("Multuple new lines in the extra header. Ignoring it in the output")
                return(NULL)
            }

            if (!eol_test$EOL)
                str_rep[1] <- paste0(str_rep[1], "\\\\")

            result <- str_rep[1]
            if (!rlang::is_na(str_rep[2]))
                result <- paste0(result, " %", str_rep[2])

            return(result)
        }

        if (rlang::is_character(input)
            && length(input) == nrow(format_tbl))
                return(RLibs::GlueFmt(header_format, .envir = set_names(as_list(input), format_tbl$Name)))

        if (rlang::is_list(input)
            && length(input) == nrow(format_tbl)
            && purrr::every(input, rlang::is_scalar_character)) {

            if(rlang::is_named(input) && all(names(input) %in% format_tbl$Name))
                return(RLibs::GlueFmt(header_format, .envir = input))
            else
                return(RLibs::GlueFmt(header_format, .envir = set_names(input, format_tbl$Name)))
            }

        return(NULL)

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
        dplyr::mutate(
            Name = nms,
            Padding = readr::parse_integer(Padding),
            Padding = dplyr::if_else(is.na(Padding), 8L, Padding),
            Precision = readr::parse_integer(Precision),
            InsertMathBody = purrr::flatten_lgl(insert_math_body),
            InsertMathHeader = purrr::flatten_lgl(insert_math_header)) %>%
        dplyr::mutate(
            PrecisionStr = if_else(is.na(Precision), "", rlang::as_character(glue::glue(".{Precision}"))),
            BodyFormat =
               glue::glue("%{if_else(InsertMathHeader & !InsertMathBody, Padding + 2L, Padding)}{PrecisionStr}{Type}"),
            HeaderFormat =
                glue::glue("%{if_else(InsertMathBody & !InsertMathHeader, Padding + 2L, Padding)}s"),
            AddonFormat =
                glue::glue("%{if_else(InsertMathBody | InsertMathHeader, Padding + 2L, Padding)}s"))
        



    line_format <- format %>%
        dplyr::select(BodyFormat, Name, InsertMathBody) %>%
        pmap(~glue::glue("{if(..3) '$' else ''}{{{..2}: {..1}}}{if(..3) '$' else ''}")) %>%
        glue::glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_format <- format %>%
        dplyr::select(HeaderFormat, Name, InsertMathHeader) %>%
        pmap(~glue::glue("{if(..3) '$' else ''}{{{..2}: {..1}}}{if(..3) '$' else ''}")) %>%
        glue::glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    plain_header_format <- format %>%
        dplyr::select(AddonFormat, Name) %>%
        pmap(~glue::glue("{{{..2}: {..1}}}")) %>%
        glue::glue_collapse(sep = " & ") %>%
        paste0(" \\\\")

    header_string <- RLibs::GlueFmt(header_format, .envir = set_names(as_list(nms)))

    append_next <- append

    table_frmt <- paste0(col_layout, collapse = "")

    before <- process_extra_headers(before, format, plain_header_format)
    after <- process_extra_headers(after, format, plain_header_format)

    offset <- 0L
    offset_step <- 4L
    output <- indent(glue::glue("\\begin{{tabular}}{{{table_frmt}}}"), offset)

    if (add_hlines) {
        offset <- offset + offset_step
        output %<>% c(indent("\\hline", offset))
    }


    offset <- offset + offset_step
    if (!rlang::is_null(before))
        output %<>% c(indent(before, offset))

    if (print_header) 
        output %<>% c(indent(header_string, offset))

    if (!rlang::is_null(after))
        output %<>% c(indent(after, offset))

    offset <- offset - offset_step
    if (add_hlines && print_header) 
        output %<>% c(indent("\\hline", offset))
    

    offset <- offset + offset_step
    
    if (print_header)
        output %<>% c(indent("", offset))

    output %<>% c(data %>% purrr::pmap_chr(function(...) {
                RLibs::GlueFmt(indent(line_format, offset), .envir = list(...))
            }))
        

    if (add_hlines) {
        offset <- offset - offset_step
        output %<>% c(indent("\\hline", offset))
    }

    offset <- offset - offset_step
    output %<>% c(indent("\\end{tabular}"))

    readr::write_lines(output, path, append = append)

    invisible(NULL)
}

mtcars %>%
    table_2_tex("test.tex", add_hlines = T, print_header = T,
        before = as_list(names(.)),
        after = as.character(1:ncol(.))) %>%
    print