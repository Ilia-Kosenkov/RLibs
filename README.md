
# `RLibs`

`RLibs` is a library of various production tools I use in data
processing. It is under construction, a lot of functions are/will be
deprecated, some of them will be moved to other packages (like anything
useful and related to plotting with `ggplot2` will got to
[`sciplotr`](https://github.com/Ilia-Kosenkov/sciplotr)).

# Features

## Equality & Comparison

Default `R` `==` operator performs strict comparison, which does not
work very well for floating-point problems. What `R` considers unequal,
technically can be equal within machine’s precision. The standard
example is

``` r
0.1 + 0.2 == 0.3
```

    ## [1] FALSE

There is `RLibs::are_equal_f`, which performs more or less correct
floating-point comparison (with some given tolerance).

``` r
library(RLibs, quietly = TRUE, warn.conflicts = FALSE)
are_equal_f(0.1 + 0.2, 0.3)
```

    ## [1] TRUE

Atop of this function there are several more built for comfortable use:

``` r
(0.1 + 0.2) %==% 0.3
```

    ## [1] TRUE

``` r
(0.1 + 0.2) %!=% 0.3
```

    ## [1] FALSE

Operators invoke floating-point method only if type of one operand is
floating-point. Type/size stability is enforced by the
[`vctrs`](https://github.com/r-lib/vctrs) package.

## Cluster planning

A set of tools to create clusters to work with `future` and `furrr`
packages.

``` r
# Checks cluster status
get_topology()
```

    ## [1] 1

``` r
# Create 2 workers, each spawning 2 workers (so 4 + 2 in total, max 4 working simultaneously)
plan_cluster(2, 2)
```

    ## Cluster: [2, 2]

``` r
unlist(furrr::future_map(1:2, ~list(Sys.getpid(), furrr::future_map(1:2, ~Sys.getpid()))))
```

    ## [1] 26272 15908 19512 24232 25120 26904

``` r
# Switch back to sequential execution
plan_cluster(1)
```

    ## Cluster: single process

``` r
unlist(furrr::future_map(1:2, ~list(Sys.getpid(), furrr::future_map(1:2, ~Sys.getpid()))))
```

    ## [1] 20028 20028 20028 20028 20028 20028

## Tricky joins

`dplyr` can do various joins, like `inner_join`, `left_join`. Here is a
way to do conditional joins (not really optimized):

``` r
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
tbl <- data.frame(Type = c("10-20", "20-30"), L = c(10, 20), U = c(20, 30))
# Subsetting mtcars to reduce output
left_join_cnd(mtcars[c(1:7, 18:20, 28:32),], tbl, .x$mpg >= .y$L, .x$mpg < .y$U) %>% select(Type, L, mpg, U, everything())
```

    ##     Type  L  mpg  U cyl  disp  hp drat    wt  qsec vs am gear carb
    ## 1  20-30 20 21.0 30   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## 2  20-30 20 21.0 30   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## 3  20-30 20 22.8 30   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## 4  20-30 20 21.4 30   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## 5  10-20 10 18.7 20   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## 6  10-20 10 18.1 20   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## 7  10-20 10 14.3 20   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## 8   <NA> NA 32.4 NA   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## 9   <NA> NA 30.4 NA   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## 10  <NA> NA 33.9 NA   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## 11  <NA> NA 30.4 NA   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## 12 10-20 10 15.8 20   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## 13 10-20 10 19.7 20   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## 14 10-20 10 15.0 20   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## 15 20-30 20 21.4 30   4 121.0 109 4.11 2.780 18.60  1  1    4    2

Here `...` accepts comma-separated conditions, similar to
`dplyr::filter`, where `.x` refers to lhs table and `.y` refers to rhs
table.

There are also `*_join_safe`, which perform exactly the same as
`dplyr::*_join` joins, but beforehand key columns are converted to
common types and a meaningful error message is displayed if conversion
fails. No more casting factors to strings if the levels are different.
`vctrs` to the rescue\!

## Utility methods

  - `%vec_in%` invokes `vctrs::vec_in()`,
  - `%within%` is non-inclusive check if vector is in range, `%withini%`
    include boundaries,
  - `fct_get` gets values of factor (as in `levels(factor)[factor]`),
  - `len` is an `S3` that invokes `vctrs::vec_size` almost always,
    except for a few cases, where it calls `length`, e.g. for
    `grid::unit`.
  - `cc` is short for `vctrs::vec_c`,
  - `vec_rbind_uq` is an unquoted pipe-friendly version of `vec_rbind`,
    which accepts list as parameter, not `...`. It can be used to bind
    list of data frames / tibbles, like `tbl_list %>% map(mutate, A = 2
    * B) %>% vec_rbind_uq`. It is currently preferred over
    `dplyr::bind_rows`, which does horrible things to the types.
  - `lin` does linear inter/extrapolation. It ensures type/size
    stability

## IO

  - `write_fixed` accepts a table, a path and format specifier (e.g. a
    vector of sprintf-like specifiers, one per each column), and outputs
    table in the plain text format. It is a great way to produce
    human-readable tables that can be read back by e.g. `read.table` or
    `readr::read_table / read_table2`.
  - `read/write_smart` accepts a table, a path, and other arguments. It
    calls different methods based on the extension of the path provided.
    Currently supported types are `feather`, `fth` for `feather` type of
    data, `rds` for `R`’s `rds`, `csv` for comma-separated file and
    everything else is processed by `write_fixed/read_table` as plain
    text.
