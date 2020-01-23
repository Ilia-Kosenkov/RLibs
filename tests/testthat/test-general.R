#   MIT License
#
#   Copyright(c) 2017-2020 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
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

context("General tests")

test_that("`glue_fmt` vectorizes corerctly", {

    x <- 123L
    comp <- paste(letters, LETTERS, x, sep = ">-<")
    glue_result <- glue_fmt_chr("{letters:%1s}>-<{LETTERS:%1s}>-<{x:%3d}")

    expect_equal(glue_result, comp)
})


test_that("`%>>%` `%<<%` compose correctly", {
    x <- rnorm(100L)
    comp <- map_dbl(x, ~ .x ^ 2 + 5)

    fwd <- (~.x ^ 2) %>>% (~.x + 5)
    bwd <- (~.x + 5) %<<% (~.x ^ 2)

    fwd_result <- map_dbl(x, fwd)
    bwd_result <- map_dbl(x, bwd)

    expect_equal(fwd_result, comp)
    expect_equal(bwd_result, comp)
    expect_equal(fwd_result, bwd_result)

})