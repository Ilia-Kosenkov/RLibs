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

utils::globalVariables(c("Fst", "Snd", "Thd", "Sgn", "Comb"))

#' @importFrom stringr str_match
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate mutate_at bind_rows if_else select vars
#' @importFrom purrr map reduce
#' @importFrom magrittr extract
#' @importFrom stats setNames
Str2Deg <- function(x, sep, denoms){
    pattern <-
        sprintf("([+-]?)([0-9]{1,2})%1$s([0-9]{1,2})%1$s([0-9\\.]+)", sep)
    matches <- x %>%
        str_match(pattern)


    tbl <- tibble(Str = character(0),
                  Sgn = character(0),
                  Fst = character(0),
                  Snd = character(0),
                  Thd = character(0))
    result <- 1:nrow(matches) %>% map(~matches %>%
                extract(.x,) %>%
                setNames(c("Str", "Sgn", "Fst", "Snd", "Thd"))) %>%
        reduce(bind_rows, .init = tbl) %>%
        select(-1) %>%
        mutate_at(vars(-Sgn), as.numeric) %>%
        mutate(Sgn = if_else(nzchar(Sgn), Sgn, "+"))
    result <- result %>%
        mutate(Comb = (Fst / denoms[1] + Snd / denoms[2] + Thd / denoms[3])) %>%
        mutate(Comb = if_else(Sgn == "-", - Comb, Comb))
    return(result)
}

#' @importFrom magrittr is_weakly_greater_than
Deg2Str <- function(x, sep, denoms, plus = "+", dig = 3) {
    sgn <- sign(x)
    sgnSymb <- sgn %>%
        is_weakly_greater_than(0) %>%
        sapply(ifelse, plus, "-")
    x <- abs(x)
    fst <- floor(x * denoms[1])
    snd <- floor((x - fst / denoms[1]) * denoms[2])
    thd <- (x - fst / denoms[1] - snd / denoms[2]) * denoms[3]
    frmt <- sprintf("%%2$1s%%3$02d%%1$s%%4$02d%%1$s%%5$02d.%%6$0%d.0f", dig)
    result <- sprintf(frmt, sep, sgnSymb,
                      fst, snd, floor(thd),
                      10 ^ dig * (thd - floor(thd)))
    return(result)
}

#' @title Dec2Degrees
#' @description Converts \code{DEC}lination to degrees.
#' @param x Vector of values.
#' @param sep Separator between degrees, minutes and seconds.
#' @param simple If \code{TRUE}, return vector of converted values.
#' If \code{FALSE}, returns a \code{tibble} with more detatiled infomration.
#' @return Either a vector of length \code{n} o a tibble \code{n x 5}.
#' @importFrom dplyr %>% rename pull
#' @export
Dec2Degrees <- function(x, sep = "\ ", simple = TRUE) {
    Str2Deg(x, sep, c(1.0, 60.0, 3600.0)) %>%
        rename(Deg = Fst, Min = Snd, Sec = Thd) %>% {
            if (simple)
                pull(., Comb)
            else
                .
        }
}

#' @title Ra2Degrees
#' @description Converts \code{R}ight \code{A}scention to degrees.
#' @param x Vector of values.
#' @param sep Separator between hours, minutes and seconds.
#' @param simple If \code{TRUE}, return vector of converted values.
#' If \code{FALSE}, returns a \code{tibble} with more detatiled infomration.
#' @return Either a vector of length \code{n} o a tibble \code{n x 5}.
#' @importFrom dplyr %>% rename pull
#' @export
Ra2Degrees <- function(x, sep = "\ ", simple = TRUE) {
    Str2Deg(x, sep, c(1.0, 60.0, 3600.0) / 15.0) %>%
        rename(Hr = Fst, Min = Snd, Sec = Thd) %>% {
            if (simple)
                pull(., Comb)
            else
                .
        }
}

#' @title Degrees2Deg
#' @description Converts numerical degrees to string representation
#' of \code{DEC}lination.
#' @param x Vector of values.
#' @param sep Separator between degrees, minutes and seconds.
#' @param dig Number of decimal digits in seconds.
#' @return A string vector.
#' @export
Degrees2Dec <- function(x, sep = "\ ", dig = 3) {
    Deg2Str(x, sep, denoms = c(1.0, 60.0, 3600.0), "+", dig)
}

#' @title Degrees2Ra
#' @description Converts numerical degrees to string representation
#' of \code{R}ight \code{A}scention.
#' @param x Vector of values.
#' @param sep Separator between hours, minutes and seconds.
#' @param dig Number of decimal digits in seconds.
#' @return A string vector.
#' @export
Degrees2Ra <- function(x, sep = "\ ", dig = 3) {
    Deg2Str(x, sep, c(1.0, 60.0, 3600.0) / 15.0, "", dig)
}
