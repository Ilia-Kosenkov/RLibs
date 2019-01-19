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

context("Math input parameter tests")

test_that("M.Drv throws on illegal 'func' argument", {
    expect_error(M.Drv(func = 0, x0 = 0), "func")
    expect_error(M.Drv(func = c(0, "abc"), x0 = 0), "func")
    expect_error(M.Drv(func = c(ls, NA, NULL), x0 = 0), "func")
    expect_error(M.Drv(func = list(0, "abc"), x0 = 0), "func")
})

test_that("M.Drv throws on illegal 'x0' argument", {
    expect_error(M.Drv(ls, x0 = "abc"), "x0")
    expect_error(M.Drv(ls, x0 = NA), "x0")
    expect_error(M.Drv(ls, x0 = c(1,2)), "vectorize")

})

test_that("M.Drv throw on illegal 'eps' argument", {
    expect_error(M.Drv(ls, 0, eps = list(1)), "eps.*not.*number")
    expect_error(M.Drv(ls, 0, eps = 1:5), "vectorize")
    expect_error(M.Drv(ls, 0, eps = .Machine$double.eps * (1 - 1e-5)), "platform epsilon")
})

test_that("M.Orth2 throws on illegal 'vec' argument", {
    expect_error(M.Orth2(NA), "not a number")
    expect_error(M.Orth2(list(1, 2, 2, 4, 5)), "not a number")
    expect_error(M.Orth2(matrix(c(1, 2, 3))), "length 2")
    expect_error(M.Orth2(1:3), "length 2")
})

test_that("M.Norm throws on illegal 'vec' argument", {
    expect_error(M.Norm(NA), "not a number")
    expect_error(M.Norm(matrix(1, 2, 3)), "1D vector")
})


context("Math result tests")

test_that("M.Drv simple one-function derivative of x^2 at 2", {
    test_f = function(x) x ^ 2
    eps = 1e-5
    tolerance = 1e-7
    for (epsFactor in 10^(-(0:6)))
        expect_equal(M.Drv(func = test_f, x0 = 2, eps = eps * epsFactor), 2 * 2,
            tolerance = tolerance,
            info = sprintf("eps = %e, tolerance = %e", eps * epsFactor, tolerance))
})

test_that("M.Drv complex analytical derivative of log(cos(x^2)) at different values of x0", {
    test_f = function(x) log(cos(x ^ 2))
    test_d = function(x) - 2 * x * tan(x ^ 2)
    tolerance = 1e-7
    delta = 1e-2
    x_arr = seq(-sqrt(pi / 2) + delta, sqrt(pi / 2) - delta, length.out = 100)

    for (x in x_arr)
        expect_equal(M.Drv(func = test_f, x0 = x), test_d(x), tolerance = tolerance,
             info = sprintf("x0 = %e with tolerance = %e", x, tolerance))

})

test_that("M.Drv multiple polinomials' derivatives calcualted at x0 = 1", {
    inds = -4:6
    test_fs = sapply(inds, function(i) return(function(x) x ^ i))
    tolerance = 1e-7
    expect_equal(M.Drv(test_fs, 1), inds, tolerance = tolerance)       

})

test_that("M.Orth2 returns normal to a given vector; tests [normal x vector == 0]", {
    N = 100
    x = matrix(data = rnorm(2 * N), ncol = 2, byrow = FALSE)

    for (i in 1:N) {
        y = as.numeric(x[i,])
        z = M.Orth2(y)
        expect_equal(as.numeric(y %*% z), 0,
            info = sprintf("\tvec = [%f, %f]\n\tnorm = [%f, %f]\n", y[1], y[2], z[1], z[2]))
    }
})

test_that("M.Norm returnes vector, which length is 1", {
    N = 100
    for (i in 1:10) {
        x = runif(N, -10, 10)
        expect_equal(sum(M.Norm(x)^2), 1)
    }
})

test_that("Transitivity and symmetry of `are_equal_f", {
    x <- rnorm(50)
    sample_1 <- sample(x, 1e3, TRUE)
    sample_2 <- sample(x, 1e3, TRUE)

    expect_true(all(are_equal_f(sample_1, sample_2) ==
        are_equal_f(sample_2, sample_1)))

    expect_true(all(are_equal_f(sample_1, sample_1)))
    expect_true(all(are_equal_f(sample_2, sample_2)))

})