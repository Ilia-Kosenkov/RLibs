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


#' @title ReadList
#' @param file Path to a file
#' @description Reads specifically formatted sequences of integers from a file.
#' @return A list of indexes generated from file
#' @importFrom stringr str_split str_match str_detect str_replace_all
#' @importFrom purrr some keep map map_if
#' @importFrom magrittr extract extract2
#' @importFrom dplyr %>%
#' @export
ReadList <- function(file) {
    input <- scan(file, what = "", sep = "\n", quiet = TRUE)

    parse <- function(x) {
        str_split(x, ":") %>%
            extract2(1) %>% {
                if (length(.) == 1L)
                    as.numeric(.)
                else if (length(.) == 2L)
                    seq(as.numeric(.[1]), as.numeric(.[2]))
                else
                    stop("Failed to parse.")
                }
    }


    input %>%
        map(str_match,
            pattern = "([[[:alnum:]][[:punct:]]^:]*?)\\ *?:?\\ +?(.*)") %>%
        map(~list(extract(.x,, 2), extract(.x,, 3))) %>%
        map(function(x) {
            x[[2]] <- str_split(x[[2]], " ")[[1]]
            x[[2]] <- x[[2]] %>% extract(nzchar(.))
            if (x[[2]] %>% some(~str_detect(.x, "\"")))
                x[[2]] %<>%
                    str_replace_all("\"", "")
            else
                x[[2]] %<>% map(parse) %>% unlist

            return(x)
        }) %>%
        map(function(x) {
            if (str_detect(x[[1]], "\""))
                x[[1]] %<>% str_replace_all("\"", "")
            else
                x[[1]] %<>% parse

            return(x)
        }) %>%
        map_if(~length(.x) > 1, function(x) {
            negInds <- x[[2]] %>% keep(~.x < 0)
            x[[2]] %<>% keep(~.x >= 0) %<>% setdiff(abs(negInds))
            x
        }) -> parsed

    result <- parsed %>%
        map(function(x) if (length(x) > 1) extract2(x, 2) else NA) %>%
        setNames(parsed %>% map_chr(~extract(.x, 1) %>% as.character))

    return(result)
}

utils::globalVariables(c("Col", "Format", "Header", "IsFactor", "Str", "Type"))

#' @title write_fixed
#' @description Prints table to the target file in a fixed-format manner.
#' @param frame \link{data.frame} or \link{tibble} to print.
#' @param path Path to the output file.
#' @param frmt \link{sprintf}-compatible format.
#' Either one value applied to all columns,
#' or a \code{character} vector of \code{ncol(frame)} elements.
#' @param append If \code{TRUE}, appends data to the existing file.
#' @importFrom rlang is_missing is_atomic
#' @importFrom tibble tibble is_tibble
#' @importFrom purrr map_chr map_lgl map2_chr map2_df negate
#' @importFrom dplyr %>% mutate filter pull mutate_at vars funs one_of
#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_extract str_c
#' @importFrom readr write_lines
#' @importFrom assertthat assert_that is.string
#' @importFrom vctrs vec_ptype_full
#' @export
write_fixed <- function(frame, path, frmt, append = FALSE) {

    assert_that(is_tibble(frame) || is.data.frame(frame))
    assert_that(is.string(path))

    if (some(frame, negate(is_atomic)))
        stop("Only tables with atomic types are supported.")

    selector <- function(x)
        switch(x,
            "double" = "%8.2f",
            "integer" = "%8d",
            "character" = ,
            "logical" = ,
            "complex" = ,
            "raw" = "%8s")

    colTypes <- tibble(
            Col = names(frame),
            Type = map_chr(frame, vec_ptype_full),
            IsFactor = map_lgl(frame, is.factor)) %>%
        mutate(Type = if_else(IsFactor, "character", Type))

    if (!is_missing(frmt) && !is_empty(frmt) && is_character(frmt))
        colTypes %<>%
            mutate(Format = frmt)
    else
        colTypes %<>%
            mutate(Format = map_chr(Type, selector))

    fctr <- colTypes %>% filter(IsFactor) %>% pull(Col)

    colTypes %<>% mutate(
            Header = str_extract(Format, "(?<=%)[0-9]+(?=\\.?[[:alnum:]]+)"),
            Header = glue("%{Header}s"))


    headFrmt <- colTypes %>%
        mutate(Str = map2_chr(Col, Header, ~ sprintf(.y, .x))) %>%
        pull(Str) %>%
        glue_collapse

    headFrmt %>%
        append(
            frame %>% mutate_at(vars(one_of(fctr)), fct_get) %>%
                map2_df(colTypes$Format, ~ sprintf(.y, .x)) %>%
                pmap(str_c))%>%
        write_lines(path, append = append)
}

Tools.DataFrame.Print <- function(frame, file, frmt = "%8.2f",
                                 printHeaders = TRUE, append = FALSE) {
    temp <- ""
    Nc <- ncol(frame)
    Nr <- nrow(frame)

    tryCatch({
        sink(file, append = append)

        if (printHeaders) {
            sizes <- as.integer(
                sapply(regmatches(frmt, regexec("%([0-9]*)", frmt)),
                "[[", 2))
            if (any(is.na(sizes)))
                stop("Explicit column width is required in [frmt].")
            if (length(sizes) != ncol(frame))
                sizes <- rep(sizes[1], ncol(frame))

            hdrFrmt <- sapply(sizes, function(sz) sprintf("%%%ss", sz))
            header <- paste(
                sapply(seq_len(length(hdrFrmt)),
                    function(i) sprintf(hdrFrmt[i], names(frame)[i])),
                collapse = "")
            writeLines(header)
        }

        if (length(frmt) != ncol(frame))
            bodyFrmt <- rep(frmt[1], ncol(frame))
        else
            bodyFrmt <- frmt
        for (j in 1:Nr) {

            data <- as.list(frame[j, ])
            body <- paste(sapply(seq_len(length(bodyFrmt)),
                            function(i) sprintf(bodyFrmt[i], data[[i]])),
                          collapse = "")
            writeLines(body)
        }
    },
    finally = sink()
  )
}

#' @export
Tools.DataFrame.DF2Latex <- function(...)
    stop("`Tools.DataFrame.DF2Latex` is deprecated. " %+%
         "Use `Tools.DataFrame.DF2Latex2` instead.")

#' @export
Tools.DataFrame.DF2Latex2 <- function(frame, file,
                                    frmt = "%6.2f", printHeaders = TRUE,
                                    insMathHead = TRUE, insMathBody = FALSE,
                                    insMathBefore = FALSE, insMathAfter = FALSE,
                                    cols = "c", NA.symb = NA_character_,
                                    beforeHead = NA, afterHead = NA) {
    if (insMathBody)
        mB <- "$"
    else
        mB <- ""
    if (insMathHead)
        mH <- "$"
    else
        mH <- ""
    if (insMathBefore)
        mBe <- "$"
    else
        mBe <- ""
    if (insMathAfter)
        mAf <- "$"
    else
        mAf <- ""

    sink(file)
    temp <- ""
    Nc <- ncol(frame)
    Nr <- nrow(frame)
    tryCatch({
        if (printHeaders) {
            temp <- sprintf("\\begin{tabular}{")

            if (length(cols) == 1)
                for (i in 1:Nc)
                    temp <- sprintf("%s%s", temp, cols[[1]])
            else
                for (i in 1:Nc)
                    temp <- sprintf("%s%s", temp, cols[[i]])

            temp <- sprintf("%s}\n", temp)
            temp <- sprintf("%s\t\\hline\n\t", temp)

            if (!all(is.na(beforeHead))) {
                for (j in 1:nrow(beforeHead)) {
                    for (i in 1:ncol(beforeHead)) {
                        if (length(frmt) > 1)
                            frmt_t <- frmt[[i]]
                        else
                            frmt_t <- frmt[[1]]

                        expr <- regexpr("[0-9]+", frmt_t, perl = TRUE)

                        format <- paste("%s ", mBe, "%",
                            regmatches(frmt_t, expr), "s", mBe,
                            ifelse(i == ncol(beforeHead), "\\\\\n\t", " & "),
                            sep = "")
                        temp <- sprintf(format, temp, beforeHead[j, i])
                    }
                }
            }

            for (i in 1:(Nc)) {

                if (length(frmt) > 1)
                    frmt_t <- frmt[[i]]
                else
                    frmt_t <- frmt[[1]]

                expr <- regexpr("[0-9]+", frmt_t, perl = TRUE)

                format <- paste("%s ", mH, "%",
                    regmatches(frmt_t, expr), "s", mH,
                    ifelse(i == ncol(frame), "\\\\\n\t", " & "),
                    sep = "")
                temp <- sprintf(format, temp, names(frame)[i])
            }


            if (!all(is.na(afterHead))) {
                for (j in 1:nrow(afterHead)) {
                    for (i in 1:ncol(afterHead)) {
                        if (length(frmt) > 1)
                            frmt_t <- frmt[[i]]
                        else
                            frmt_t <- frmt[[1]]

                        expr <- regexpr("[0-9]+", frmt_t, perl = TRUE)

                        format <- paste("%s ", mAf, "%",
                            regmatches(frmt_t, expr), "s", mAf,
                            ifelse(i == ncol(afterHead), "\\\\\n\t", " & "),
                            sep = "")
                        temp <- sprintf(format, temp, afterHead[j, i])
                    }
                }
            }
        }

        writeLines(paste(temp, ifelse(nchar(temp) > 0, "", "\t"), 
            "\\hline", sep = ""))

        for (i in 1:Nr) {
            temp <- "\t"

            for (j in 1:(Nc - 1)) {
                if (length(frmt) > 1)
                    frmt_t <- frmt[[j]]
                else
                    frmt_t <- frmt[[1]]
                if (is.na(frame[i, j])) {
                    expr <- regexpr("[0-9]+", frmt_t, perl = TRUE)

                    format <- paste("%s ", mB, "%", regmatches(frmt_t, expr),
                        "s", mB, " & ", sep = "")
                }
                else
                    format <- paste("%s ", mB, frmt_t, mB, " & ", sep = "")

                temp <- sprintf(format, temp, ifelse(is.na(frame[i, j]),
                    NA.symb, frame[i, j]))
            }

            if (length(frmt) > 1)
                frmt_t <- frmt[[Nc]]
            else
                frmt_t <- frmt[[1]]
            if (is.na(frame[i, Nc])) {
                expr <- regexpr("[0-9]+", frmt_t, perl = TRUE)
                format <- paste("%s ", mB, "%", regmatches(frmt_t, expr),
                    "s", mB, " \\\\ ", sep = "")
            }
            else
                format <- paste("%s ", mB, frmt_t, mB, " \\\\ ", sep = "")


            temp <- sprintf(format, temp, ifelse(is.na(frame[i, Nc]),
                NA.symb, frame[i, Nc]))

            writeLines(temp)
        }

        writeLines("\t\\hline")
        writeLines("\\end{tabular}")
    },
    finally = sink())
}

#' @title write_smart
#' @description Writes output in one of the avilable formats
#' based on the file extension.
#' @param data Input table.
#' @param path Path to save.
#' @param ... Additional paramteres passed to either of
#' \code{feather::write_feather}, \code{readr::write_rds} or 
#' \code{RLibs::WriteFixed}.
#' @return Nothing
#' @importFrom assertthat assert_that is.string
#' @importFrom tibble is_tibble
#' @importFrom fs path_ext
#' @importFrom readr write_rds
#' @importFrom feather write_feather
#' @export
write_smart <- function(data, path, ...) {
    assert_that(is_tibble(data) || is.data.frame(data), msg = "data should be either tibble or a data.frame")
    assert_that(is.string(path))
    
    ext <- tolower(path_ext(path))

    switch(ext,
           "feather" = ,
           "feath" = ,
           "fth" = write_feather(data, path),
           "rds" = write_rds(data, path, ...),
           write_fixed(data, path, ...))
}

#' @title read_smart
#' @description Reads output in one of the avilable formats
#' based on the file extension.
#' @param path Path to read from.
#' @param ... Additional paramteres passed to either of
#' \code{feather::read_feather}, \code{readr::read_rds} or 
#' \code{readr::read_table2}.
#' @return Nothing
#' @importFrom assertthat assert_that is.string is.readable
#' @importFrom fs path_ext
#' @importFrom readr read_rds read_table2
#' @importFrom feather read_feather
#' @export
read_smart <- function(path, ...) {
    assert_that(is.string(path), is.readable(path))

    ext <- tolower(path_ext(path))

    switch(ext,
        "feather" = ,
        "feath" = ,
        "fth" = read_feather(path),
        "rds" = read_rds(path),
        read_table2(path, ...))
}


#' @rdname WriteFixed
#' @export
WriteFixed <- deprecate_function(WriteFixed, write_fixed)