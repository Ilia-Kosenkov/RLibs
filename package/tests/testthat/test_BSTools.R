context("Bayesian samplers")

#test_that("BSTools.Run1", {
          #modelStr <- paste(" model ", "{ ", " a ~ dnorm(0, 1) ", " }", sep = " \r\n")
          #con <- textConnection(modelStr)
          #N <- 1e5
          #M <- 3
          #sink(ifelse(.Platform$OS.type == "windows", "nul", "/dev/null"))
          #BSTools.Run1(con, data = NULL, samples = c("a"), N, sample_each = 1,
                       #M = M)
          #sink()
          #close(con)
          #expect_true(length(BSTools.Result) == M)
          #expect_true(all(sapply(BSTools.Result, length) == N))
          #expect_equal(mean(sapply(BSTools.Result, mean)), 0,
                       #tolerance = 1e-2)
          #})

test_that("BSTools.Run2", {
    modelStr <- paste(" model ", "{ ", " a ~ dnorm(0, 1) ", " }", sep = " \r\n")
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

test_that("BSTools.Run2: reproducability", {
            modelStr <- paste(" model ", "{ ", " a ~ dnorm(0, 1) ", " }", sep = " \r\n")
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