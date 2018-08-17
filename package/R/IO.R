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

#' @title WriteFixed
#' @param frame \link{data.frame} or \link{tibble} to print.
#' @param path Path to the output file.
#' @param frmt \link{sprintf}-compatible format.
#' Either one value applied to all columns,
#' or a \code{character} vector of \code{ncol(frame)} elements.
#' @description Prints table to the target file in a fixed-format manner.
#' @export
WriteFixed <- function(frame, path, frmt = "%8.2f") {
    Tools.DataFrame.Print(frame, file = path, frmt = frmt)
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
