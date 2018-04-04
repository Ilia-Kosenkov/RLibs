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

context("GGplot2 helper functions")

test_that("Lookup.gtable provides output", {
    result = Lookup(grob, "panel", "axis-l")$GrobDesc
    expect_true((c("panel") %in% result$name) &&
                (c("axis-l") %in% result$name))
})

test_that("Get grob", {

    expect_true(nzchar(GetGrob(grob, "title")$name))

})

test_that("NullGrob can be determined", {
    title = GetGrob(grob, "title")
    back = GetGrob(grob, "background")
          
    expect_true(IsGrobNull(title))
    expect_false(IsGrobNull(back))
    expect_true(all(c(TRUE, FALSE) == IsGrobNull(title, back)))
})

test_that("Inner margins are obtained", {
    expect_true(length(GetMargins(grob, "inner")) == 4)
})

test_that("Outer margins are obtained", {
    expect_true(length(GetMargins(grob, "outer")) == 4)
})

test_that("Both margins are obtained", {
    expect_true(length(r <- GetMargins(grob)) == 2)
    expect_true(length(r[[1]]) == 4)
    expect_true(length(r[[2]]) == 4)

})

test_that("GetaMargins throws error", {
    expect_error(GetMargins(grob, 123), "type")
    expect_error(GetMargins(NULL), "grob")
})

