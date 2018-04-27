if (interactive()) {
    # If is in interactive session, run debug code

    test_BSTools.DebugPlot <- function() {
        require(dplyr)
        require(foreach)
        require(ggplot2)
        require(RColorBrewer)
        require(gridExtra)
        require(tibble)



        N <- 10000
        data <- list(
            tibble(x = runif(N), y = rnorm(N), z = rnorm(N, 1, 100),
                test = rbeta(N, 1, 2)),
            tibble(x = runif(N), y = rnorm(N), z = rnorm(N, 1, 100),
                test = rbeta(N, 1, 2)),
            tibble(x = runif(N), y = rnorm(N), z = rnorm(N, 1, 100),
                test = rbeta(N, 1, 2)),
            tibble(x = runif(N), y = rnorm(N), z = rnorm(N, 1, 100),
                test = rbeta(N, 1, 2)))

        BSTools.DebugPlot(data, 100L, 10000L, nPltRow = 3L)
    }

    test_BSTools.DebugPlot()

} else {
    # If session is not interactive, build package
    require(devtools)
    require(roxygen2)

    roxygen2::roxygenize("./package")
    #devtools::document("./package")
    system("cmd /k \"R.exe CMD build ./package && R.exe CMD check *gz && exit\"")
}