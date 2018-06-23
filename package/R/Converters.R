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
    1:nrow(matches) %>% map(~matches %>%
                extract(.x,) %>%
                setNames(c("Str", "Sgn", "Fst", "Snd", "Thd"))) %>%
        reduce(bind_rows, .init = tbl) %>%
        select(-1) %>%
        mutate_at(vars(-Sgn), as.numeric) %>%
        mutate(Sgn = if_else(nzchar(Sgn), Sgn, "+")) %>%
        mutate(Comb = (Fst / denoms[1] + Snd / denoms[2] + Thd / denoms[3]) *
               if_else(Sgn == "-", -1, +1)) -> result
    return(result)
}

Deg2Str <- function(x, sep, denoms, plus = "+", dig = 3) {
    sgn <- sign(x)
    sgnSymb <- sgn %>% equals(1) %>% sapply(ifelse, plus, "-")
    x <- abs(x)
    fst <- floor(x * denoms[1])
    snd <- floor((x - fst / denoms[1]) * denoms[2])
    thd <- (x - fst / denoms[1] - snd / denoms[2]) * denoms[3]
    frmt <- sprintf("%%2$1s%%3$02d%%1$s%%4$02d%%1$s%%5$02.%df", dig)
    result <- sprintf(frmt, sep, sgnSymb, fst, snd, thd)
    return(result)
}

Dec2Degrees <- function(x, sep = "\ ", simple = TRUE) {
    Str2Deg(x, sep, c(1.0, 60.0, 3600.0)) %>%
        rename(Deg = Fst, Min = Snd, Sec = Thd) %>% {
            if (simple)
                pull(., Comb)
            else
                .
        }
}

Ra2Degrees <- function(x, sep = "\ ", simple = TRUE) {
    Str2Deg(x, sep, c(1.0, 60.0, 3600.0) / 15.0) %>%
        rename(Hr = Fst, Min = Snd, Sec = Thd) %>% {
            if (simple)
                pull(., Comb)
            else
                .
        }
}

Degrees2Dec <- function(x, sep = "\ ", dig = 3) {
    Deg2Str(x, sep, denoms = c(1.0, 60.0, 3600.0), "+", dig)
}

Degrees2Ra <- function(x, sep = "\ ", dig = 3) {
    Deg2Str(x, sep, c(1.0, 60.0, 3600.0) / 15.0, "", dig)
}

Degrees2Ra(c(-37.511, 45.52)) %>% Ra2Degrees %T>% print