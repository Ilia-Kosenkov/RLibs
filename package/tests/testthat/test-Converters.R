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

context("RA & DEC conversions")

test_that("Anchor RA values are correctly converted to strings", {
    vals <- c(0, 15, 22.5, 22.625)
    vals <- c(vals, - vals)
    exp <- c(" 00 00 00.000",
             " 01 00 00.000",
             " 01 30 00.000",
             " 01 30 30.000",
             " 00 00 00.000",
             "-01 00 00.000",
             "-01 30 00.000",
             "-01 30 30.000")
    res <- Degrees2Ra(vals)
    expect_equal(res, exp)
})

test_that("Anchor DEC values are correctly converted to strings", {
    vals <- c(0, 1, 1.5, 1.51)
    vals <- c(vals, - vals)
    exp <- c("+00 00 00.000",
             "+01 00 00.000",
             "+01 30 00.000",
             "+01 30 36.000",
             "+00 00 00.000",
             "-01 00 00.000",
             "-01 30 00.000",
             "-01 30 36.000")
    res <- Degrees2Dec(vals)
    expect_equal(res, exp)
})

test_that("Anchor RA values are correctly converted from strings", {
    vals <- c(" 00 00 00.000",
              " 01 00 00.000",
              " 01 30 00.000",
              " 01 30 30.000",
              " 00 00 00.000",
              "-01 00 00.000",
              "-01 30 00.000",
              "-01 30 30.000")
    exp <- c(0, 15, 22.5, 22.625)
    exp <- c(exp, -exp)
    res <- Ra2Degrees(vals)
    expect_equal(exp, res)
})

test_that("Anchor DEC values are correctly converted from strings", {
    exp <- c(0, 1, 1.5, 1.51)
    exp <- c(exp, -exp)
    vals <- c("+00 00 00.000",
             "+01 00 00.000",
             "+01 30 00.000",
             "+01 30 36.000",
             "+00 00 00.000",
             "-01 00 00.000",
             "-01 30 00.000",
             "-01 30 36.000")

    res <- Dec2Degrees(vals)
    expect_equal(res, exp)
})

test_that("Back and forth conversion preserves value", {
    angls <- dunif(1000, 0, 360)
    raStrs <- Degrees2Ra(angls)
    raAngls <- Ra2Degrees(raStrs)

    decStrs <- Degrees2Dec(angls)
    decAngls <- Dec2Degrees(decStrs)

    expect_equal(round(raAngls, 5), round(angls, 5))
    expect_equal(round(decAngls, 6), round(angls, 6))
})

