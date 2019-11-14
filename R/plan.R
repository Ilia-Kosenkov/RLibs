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

#' @importFrom future nbrOfWorkers future value
#' @importFrom vctrs vec_c
get_child_workers <- function() {
    x <- future::nbrOfWorkers()

    if (x != 1L)
        wrks <- vec_c(x, value(future({ get_child_workers() })))
    else
        wrks <- x
    wrks
}


#' @title get_topology
#' @return Returns the topology of the \code{future} cluster
#' @importFrom vctrs vec_size
#' @importFrom utils head
#' @export
get_topology <- function() {
    wrks <- get_child_workers()
    if (vec_size(wrks) != 1)
        wrks <- head(wrks, -1)
    wrks
}

#' @title report_cluster_status
#' @description Informs a user of the current cluster status.
#' @return Nothing
#' @importFrom vctrs vec_size
#' @export
report_cluster_status <- function() {
    topology <- get_topology()
    if (vec_size(topology) == 1L && topology[1] == 1L)
        message(glue_fmt("Cluster: single process"))
    else
        message(glue_fmt("Cluster: [{glue_collapse(topology, sep = \", \")}]"))
}


#' @title plan_cluster
#' @param ... Number of workers per each dimension
#' @param .default_seq Default tweak method for sequential execution
#' @param .default_multi Default tweak method for parallel execution
#' @return Nothing
#' @importFrom future tweak sequential cluster plan
#' @importFrom rlang as_function list2 is_empty is_function is_formula is_empty list2
#' @importFrom assertthat assert_that
#' @importFrom vctrs vec_cast vec_size
#' @importFrom purrr map_int some %>%
#' @export
plan_cluster <- function(...,
    .default_seq = ~tweak(sequential),
    .default_multi = ~tweak(cluster, workers = .)) {

    assert_that(is_formula(.default_seq) || is_function(.default_seq))
    assert_that(is_formula(.default_multi) || is_function(.default_multi))

    seq_tweak <- rlang::as_function(.default_seq)
    multi_tweak <- rlang::as_function(.default_multi)

    args <- list2(...) %>%
        map_int(vec_cast, integer())

    if (is_empty(args))
        args <- 1L

    if (is_empty(args) || some(args, ~ .x == 0L))
        stop("Cannot create empty cluster")
    if (some(args, ~ .x == 1L) && vec_size(args) != 1L)
        stop("Cannot have 1-proc-sized clusters")


    topology <- get_topology()

    if (vec_size(args) != vec_size(topology) || !all(args == topology)) {
        args %>% map(function(sz) {
            if (sz == 1L)
                return(seq_tweak())
            else
                return(multi_tweak(sz))
            }) %>% plan
    }

    topology <- get_topology()

    report_cluster_status()
}