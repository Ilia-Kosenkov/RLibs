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
        setNames(nm = c("Sgn", "Deg", "Min", "Sec")) %>%
        mutate_at(vars(-Sgn), as.numeric) %>%
        mutate(Sgn = if_else(nzchar(Sgn), Sgn, "+")) %>%
        mutate(Comb = (Deg / denoms[1] + Min / denoms[2] + Sec / denoms[3]) *
               if_else(Sgn == "-", -1, +1)) -> result
    return(result)
}

Dec2Degrees <- function(x, sep = "\ ") {
   Str2Deg(x, sep, c(1.0, 60.0, 3600.0))
}

Hour2Degrees <- function(x, sep = "\ ") {
    Str2Deg(x, sep, c(1.0, 60.0, 3600.0)/ 15.0)
}
#Dec2Degrees(c("+07 11 07.296", "-10 22 50.344", "10 30 55")) %T>% print
#Hour2Degrees(c("12 00 00", "00 30 00")) %T>% { print(pull(., Comb)) }