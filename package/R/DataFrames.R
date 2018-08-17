#' @title FitlerRange
#' @param .data Input table.
#' @param .var Column to filter.
#' @param .range Limits on the column.
#' @return FIltered table.
#' @importFrom rlang enquo quo_squash !!
#' @importFrom dplyr %>% filter
#' @export
FilterRange <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        filter(!!expr >= .range[1] & !!expr <= .range[2])
}

#' @title Clamp.data.frame
#' @param .data Input \code{data.frame} or \code{tibble}.
#' @param .var Variable to clamp.
#' @param .range Clamp range.
#' @return \code{.data} whith clamped within \code{.range} column \code{.var}.
#' @importFrom dplyr mutate %>% if_else
#' @importFrom rlang enquo quo_squash !! :=
#' @export
Clamp.data.frame <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        mutate(!!expr := if_else(!!expr > .range[2], .range[2], !!expr)) %>%
        mutate(!!expr := if_else(!!expr < .range[1], .range[1], !!expr))
}