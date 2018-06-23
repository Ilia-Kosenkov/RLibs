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

context("Bayesian samplers")

test_that("BSTools.Run2", {
    con <- textConnection(modelStr)
    N <- 1e5
    M <- 3
    sink(ifelse(.Platform$OS.type == "windows", "nul", "/dev/null"))
    result <- BSTools.Run2(con, data = NULL, samples = c("a"),
          nBurn = N, nChain = M, sampleEach = 10, updateCount = 3)
    sink()
    close(con)
    expect_true(length(result) == M)
    expect_true(all(sapply(result, nrow) == N))
    expect_equal(
        mean(sapply(result, function(item) item %>% pull(a) %>% mean)),
        0, tolerance = 1e-2)
})

test_that("BSTools.Run2: reproducibility", {
            N <- 1e5
            M <- 3
            inits <- BSTools.RNGs(1)
            sink(ifelse(.Platform$OS.type == "windows", "nul", "/dev/null"))
            con <- textConnection(modelStr)
            result1 <- BSTools.Run2(con, data = NULL, initials = inits[[1]],
                     samples = c("a"),
                     nBurn = N, nChain = M, sampleEach = 10, updateCount = 3)
            close(con)
            con <- textConnection(modelStr)

            result2 <- BSTools.Run2(con, data = NULL, initials = inits[[1]],
                     samples = c("a"),
                     nBurn = N, nChain = M, sampleEach = 10, updateCount = 3)
            close(con)
            sink()

            combResult1 <- bind_rows(result1)
            combResult2 <- bind_rows(result2)
            expect_equivalent(combResult1 %>% pull(a), combResult2 %>% pull(a))
          })