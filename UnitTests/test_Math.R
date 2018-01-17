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

    test_that("M.Drv throw on illegal 'eps' argument", {
          expect_error(M.Drv(ls, 0, eps = list(1)), "eps.*not.*number")
          expect_error(M.Drv(ls, 0, eps = 1:5), "vectorize")
          expect_error(M.Drv(ls, 0, eps = .Machine$double.eps * (1 - 1e-5)), "platform epsilon")
    })
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