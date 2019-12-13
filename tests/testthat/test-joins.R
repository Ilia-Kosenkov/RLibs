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

context("Join tests")

test_that("`inner_join_cnd` is identical to `inner_join`", {
    set.seed(1)
    tbl_1 <- tibble(X = 1:1000, Y = as_factor(sample(letters[1:5], 1000, TRUE)))
    tbl_2 <- tibble(X = 1:1000, Y = as_factor(sample(letters[3:10], 1000, TRUE)))

    join_1 <- inner_join(tbl_1, tbl_2, by = "Y")
    join_2 <- transmute(inner_join_cnd(tbl_1, tbl_2, .x$Y %==% .y$Y),
                        X.x = X__l, Y = Y__l, X.y = X__r)

    # Current {dplyr} joins are bugged
    ## https://github.com/tidyverse/dplyr/issues/4649
    #expect_true(all(vctrs::vec_equal(join_1, join_2)))
})