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

context("Piped-foreach tests")

test_that("Test sequential pipe", {
    res <- 1:10 %>% pforeach %do% { x } %>% unlist

    expect_equal(res, 1:10)
})

test_that("Test sequential pipe with extra named argument", {

    a <- rnorm(100)
    b <- rnorm(100)

    res <- a %>% pforeach(y = b) %do% {x * y} %>% unlist

    expect_equal(res, a * b)
})

test_that("Test sequential pipe with unnamened shorter argument", {

    a <- rnorm(100)
    n <- 10
    res <- a %>% pforeach(1:n) %do% { x } %>% unlist

    expect_equal(res, a[1:n])
})

test_that("Test sequential pipe with unnamened shorter argument", {

    a <- rnorm(100)

    res <- a %>% pforeach(.combine = c) %do% { x }

    expect_true(res %is% numeric && res == a)
})

test_that("Test sequential pipe with complex binding", {
    p <- 1:100
    q <- 100:1 - 1

    p %>% pforeach(y = q, .combine = bind_rows) %do% {
        c("p" = x, "q" = y)
    } -> res

    expect_true(res %is% data.frame &&
          ncol(res) == 2 &&
          all(c("p", "q") %in% names(res)) &&
          sum(res$p + res$q) == sum(p + q))
})
