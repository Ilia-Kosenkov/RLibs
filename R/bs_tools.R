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



#' @importFrom stats approx density quantile sd update
#' @importFrom utils setTxtProgressBar txtProgressBar install.packages

BSTools.Plots.Densities.PlotsPerRow <- 4
BSTools.Result <- NULL

BSTools.ToVector <- function(...) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.ToVector()")
    #N <- length(data)
    #result <- rep(0.0, N)

    #for (i in 1:N) {
        #result[i] <- data[i]
    #}

    #return (result)
}

BSTools.Density <- function(...) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.Density()")

    #dens <- density(data)

    #result <- data.frame(dens$x, dens$y)
    #names(result) <- c("Quantity", "Density")

    #return (result)
}

BSTools.Analyze <- function(...) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.Analyze()")

    #N <- length(result)

    #for (i in 1:N)  {
        #temp.df <- as.data.frame(result[[i]])
        #assign(sprintf("%s%0i", "RLibs::BSTools.Result.Run", i),
               #temp.df, envir = .GlobalEnv)

        #temp.stats <- data.frame(V1 = numeric(4))

        #for (j in 1:ncol(temp.df)) {
            #temp.stats[1, j] <- mean (temp.df[[j]])
            #temp.stats[2, j] <- sd (temp.df[[j]])
            #qnt <- quantile(temp.df[[j]], qntls)
            #for (k in 1:length(qnt))
                #temp.stats[2 + k, j] <- qnt[[k]]
        #}

        #names(temp.stats) <- attributes(result[[i]])$dimnames[[2]]

        #assign(sprintf("%s%0i%s", "RLibs::BSTools.Result.Run", i, ".Stats"),
               #temp.stats, envir = .GlobalEnv)
    #}

}


BSTools.Run <- function(...) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.Run()")

  #mdl <- jags.model(model, initials, n.chains = M, n.adapt = N)

  #for (i in 1:n_updates)  {
    #writeLines(sprintf("\r\nUpdating (%i)...", i))
    #adapt(mdl, N)
  #}

  #writeLines(sprintf("\r\nSampling..."))
  #result <- coda.samples(mdl, samples, N, sample_each)

  #n <- length(samples)


  #assign("RLibs::BSTools.Result", result, envir = .GlobalEnv)

}


BSTools.Run1 <- function(...) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.Run1()")

    ##message("\r\nStarting simulation...\r\n")
    #if (!all(is.na(initials)))
        #mdl <- jags.model(
            #file = model,
            #data = data,
            #inits = initials,
            #n.chains = M,
            #n.adapt = N)
    #else
        #mdl <- jags.model(
            #file = model,
            #data = data,
            #n.chains = M,
            #n.adapt = N)

        #for (i in seq_len(n_updates)) {
            ##message(sprintf("\r\nUpdating (%i)...", i))
            #adapt(mdl, N)
        #}

    ##message(sprintf("\r\nSampling..."))
    #result <- coda.samples(mdl, samples, N, sample_each)

    #n <- length(samples)


    #assign("RLibs::BSTools.Result", result, envir = .GlobalEnv)

}

#' @export
#' @importFrom tibble as.tibble
BSTools.Run2 <- function(model, data, samples, initials = NA,
        nChain, nBurn, updateCount = 2, nUpdate = nBurn,
        nSample = nUpdate, sampleEach = 10) {
    lifecycle::deprecate_warn("0.6.0", "RLibs::BSTools.Run2()",
        details = "Function is deprecated in order to remove dependence on {rjags}")

    jags.model <- rlang::exec("::", "rjags", "jags.model")
    update <- rlang::exec(":::", "rjags", "update.jags")
    coda.samples <- rlang::exec("::", "rjags", "coda.samples")

    cat("\r\nStarting simulation...\r\n")
    if (!all(is.na(initials)))
        mdl <- jags.model(
            file = model,
            data = data,
            inits = initials,
            n.chains = nChain,
            n.adapt = nBurn)
    else
        mdl <- jags.model(
            file = model,
            data = data,
            n.chains = nChain,
            n.adapt = nBurn)

    for (i in seq_len(updateCount)) {
        cat(sprintf("\r\nUpdating (%i)...\r\n", i))
        update(mdl, nUpdate, progress.bar = "text")
    }

    cat(sprintf("\r\nSampling..."))
    result <- coda.samples(mdl, samples, nSample * sampleEach, sampleEach)
    result <- lapply(result, as.tibble)
    return(result)
}


#' @importFrom MASS kde2d
#' @importFrom graphics contour
#' @importFrom graphics par plot.new mtext
BSTools.Densities <- function(plot = TRUE, rerun = TRUE) {
    lifecycle::deprecate_stop("0.6.0", "RLibs::BSTools.Densities()")

    #require(MASS)

    #N_runs = length(BSTools.Result)
    
    #pb = txtProgressBar(min = 0, max = 100, initial = 0, char = ">", style = 3, width = 60)
 
    #if(plot)
    #{
        #factor = 2
    #}
    #else
    #{
        #factor = 1
    #}
       
    #for (i in 1:N_runs)
    #{
       
        #frameName = sprintf("%s%0i", "RLibs::BSTools.Result.Run", i)
        #frame = eval(as.name(frameName))
        
        #N_var = ncol(frame)
        
        #varNames = names(frame)
     
        #for (j1 in 1:N_var)
            #for(j2 in 1:N_var)
            #{
                #if (j1 != j2)
                #{
                    #tempFrameName = sprintf("%s%0i.%s.%s", "RLibs::BSTools.Result.Run", i,  varNames[[j1]], varNames[[j2]])
                    #densFrameName = sprintf("%s%0i.%s.%s.D", "RLibs::BSTools.Result.Run", i,  varNames[[j1]], varNames[[j2]])
                    
                    #if(rerun)
                    #{
                        #dens = kde2d(frame[[j1]], frame[[j2]])
                        
                        #tempFrame = data.frame(X = dens$x, Y = dens$y)
                        #tempDens = (dens$z)
                        
                        #assign(tempFrameName, tempFrame, envir = .GlobalEnv)
                        #assign(densFrameName, tempDens, envir = .GlobalEnv)
                    #}
                    #else
                    #{
                        #tempFrame = eval(as.name(tempFrameName))
                        #tempDens = eval(as.name(densFrameName))
                    #}
                    
                #}
                
                #else
                    
                #{
                    #tempFrameName = sprintf("%s%0i.%s", "RLibs::BSTools.Result.Run", i, varNames[[j1]])
                    
                    #if (rerun)
                    #{
                        #dens = density(frame[[j1]])
                        
                        #tempFrame = data.frame(X = dens$x, D = dens$y)
                        
                        #assign(tempFrameName, tempFrame, envir = .GlobalEnv)
                    #}
                    #else
                    #{
                        #tempFrame = eval(as.name(tempFrameName))
                    #}
                    
                #}
                
                #setTxtProgressBar(pb, value = (100 * ((i-1) * N_var^2 + (j1-1) * N_var + j2) / (factor * N_var^2 * N_runs)))
              
            #}
       
    #}
    
    
    #if(plot)
    #{
        #counter = 0
        #for (g in 1:N_runs)
        #{
        
            #N_plts = BSTools.Plots.Densities.PlotsPerRow
            
            #sz = min(N_var, N_plts)
            
            #oldpar = par(oma = c(0,0,2,0), no.readonly = TRUE)
           
            #if (N_var <= N_plts)
            #{
                #plot.new()
                
                #par(mfrow = c(sz, sz))
                
                #for (j1 in 1:N_var)
                    #for(j2 in 1:N_var)
                    #{
                        #par(mfg = c(j2, j1))
                        #if (j1 != j2)
                        #{
                            #tempFrameName = sprintf("%s%0i.%s.%s", "RLibs::BSTools.Result.Run", g,  varNames[[j1]], varNames[[j2]])
                            #densFrameName = sprintf("%s%0i.%s.%s.D", "RLibs::BSTools.Result.Run", g,  varNames[[j1]], varNames[[j2]])
                            
                            #tempFrame = eval(as.name(tempFrameName))
                            #tempDens = eval(as.name(densFrameName))
                            
                            #contour(tempFrame$X, tempFrame$Y, tempDens, xlab = varNames[[j1]], ylab= varNames[[j2]], main = "")
                        #}
                        #else
                        #{
                            #tempFrameName = sprintf("%s%0i.%s", "RLibs::BSTools.Result.Run", g, varNames[[j1]])
                            
                            #tempFrame = eval(as.name(tempFrameName))
                            
                            #plot(tempFrame$X, tempFrame$D, xlab = varNames[[j1]], ylab = sprintf("Density of %s",varNames[[j1]]), main = "", type = 'l')
                        #}
                       
                         #counter = counter + 1
                        
                        #setTxtProgressBar(pb, value = (100 * (counter + N_var^2 * N_runs) / (factor * N_var^2 * N_runs)))
                    #}
                
                #mtext (text = sprintf("Densities of Run #%i", g), outer = TRUE)
            #}
            #else
            #{
                
                #N_sheets = ceiling(N_var/N_plts)
                
                #for(i1 in 1:N_sheets)
                #{
                    
                    #for(i2 in 1:N_sheets)
                    #{
                        #plot.new()
                        
                        #if(i1 == i2)
                        #{
                            
                            #par(mfcol = c(N_plts, N_plts))
                            
                        #}
                        #else if(i1 > i2)
                        #{
                            #par(mfrow = c(N_plts, N_plts))
                        #}
                        #else
                        #{
                            #par(mfcol = c(N_plts, N_plts))
                        #}
                        
                        #for (j1 in 1:N_plts)
                            #for(j2 in 1:N_plts)
                            #{
                                #k1 = j1 + (i1-1)*N_plts
                                #k2 = j2 + (i2-1)*N_plts
                                #par(mfg = c(j2, j1))
                                
                                #if ((k1 <= N_var) && (k2 <= N_var))
                                #{
                                    #if (k1 != k2)
                                    #{
                                        #tempFrameName = sprintf("%s%0i.%s.%s", "RLibs::BSTools.Result.Run", g,  varNames[[k1]], varNames[[k2]])
                                        #densFrameName = sprintf("%s%0i.%s.%s.D", "RLibs::BSTools.Result.Run", g,  varNames[[k1]], varNames[[k2]])
                                        
                                        #tempFrame = eval(as.name(tempFrameName))
                                        #tempDens = eval(as.name(densFrameName))
                                        
                                        #contour(tempFrame$X, tempFrame$Y, tempDens, xlab = varNames[[k1]], ylab= varNames[[k2]], main = "")
                                    #}
                                    #else
                                    #{
                                        #tempFrameName = sprintf("%s%0i.%s", "RLibs::BSTools.Result.Run", g, varNames[[k1]])
                                        
                                        #tempFrame = eval(as.name(tempFrameName))
                                        
                                        #plot(tempFrame$X, tempFrame$D, xlab = varNames[[k1]], ylab = sprintf("Density of %s",varNames[[k1]]) , main = "",  type = 'l')
                                    #}
                                    #counter = counter + 1
                                
                                #}
                                #else
                                #{
                                    #plot(1, type="n", axes=F, xlab="", ylab="")
                                #}
                                
                                
                                #setTxtProgressBar(pb, value = (100 * (counter +  N_var^2 * N_runs) / (factor * N_var^2 * N_runs)))
                               
                            #} 
                        
                        #mtext (text = sprintf("Densities of Run #%i", g), outer = TRUE)
                        
                    #}
                    
                #}
                
                
            #}
            
            #par(oldpar)
        #}
    #}
}

#' @export
BSTools.RNGs <- function(n) {
    lifecycle::deprecate_warn(
        "0.6.0", "RLibs::BSTools.RNGs()",
         details = "Function is deprecated in order to remove dependence on {rjags}")
    load.module <- rlang::exec("::", "rjags", "load.module")
    parallel.seeds <- rlang::exec("::", "rjags", "parallel.seeds")
    load.module("lecuyer")

    return(parallel.seeds("lecuyer::RngStream", n))
}

utils::globalVariables(c(".", "Vars"))
#' @export
#' @importFrom dplyr %>% mutate select summarise_all bind_rows everything funs
#' @importFrom tibble as.tibble
#' @importFrom stats pnorm
BSTools.Analyze1 <- function(input) {
    lifecycle::deprecate_warn("0.6.0", "RLibs::BSTools.Analyze1()")
    lapply(input,
          function(x)
              x %>%
              as.tibble %>%
              summarise_all(
              funs(Mean = mean, SD = sd,
                Q_02  = quantile(., 1 - pnorm(2)),
                Q_05  = quantile(., 0.05),
                Q_16  = quantile(., 1 - pnorm(1)),
                Q_84  = quantile(., pnorm(1)),
                Q_95  = quantile(., 0.95),
                Q_98  = quantile(., pnorm(2)),
              ))) %>%
        bind_rows %>%
        mutate(Vars = input %>% names) %>%
        select(Vars, everything())
}

utils::globalVariables(c("item", "group", ".Group", "x", "y"))
#' @title BSTools.DebugPlot
#' @description
#' Plots results of JAGS simulation obtained from \code{BSTools.Run2}.
#' Result is similar to calling \code{plot} on the direct result of
#' \code{coda.samples}
#' @param data A list of tibbles - one per each simulated chain.
#' @param traceLen Amount of data used in traceplot. If \code{traceLen} is
#' less than the size of input tibble, input is equally sampled to produce
#' a total of \code{traceLen} points on the plot. Can be useful to accelerate
#' computations.
#' @param densLen Amount of data used to construct density (distribution) plots.
#' Value smaller than the size of \code{data} downsamples input to accelerate
#' computations of densities and plotting.
#' @param nPltRow Number of plot rows per page. A good value is between 1 and 4.
#' @export
#' @importFrom dplyr %>% mutate slice pull rename bind_cols bind_rows
#' @importFrom foreach foreach %do%
#' @importFrom ggplot2 ggplot aes_string xlab ylab geom_line scale_color_manual
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.cur
#' @importFrom rlang !! :=
BSTools.DebugPlot <- function(data,
                              traceLen = 1000L, densLen = 10000L,
                              nPltRow = 3L) {

    lifecycle::deprecate_warn("0.6.0", "RLibs::BSTools.DebugPlot()")
    names <- names(data[[1]])

    pltData <- foreach(item = data,
        group = seq_int_len(length(data))) %do% {
        item %>%
            mutate(.Group = group) %>%
            mutate(ID = 1:nrow(.)) %>%
            slice(seq.int(1L, n(), length.out = traceLen))
    } %>%
        bind_rows %>%
        mutate(.Group = as.factor(.Group))

    densData <- foreach(item = data,
        group = seq_int_len(length(data))) %do% {
        foreach(name = names) %do% {
            item %>%
                slice(seq.int(1, n(), length.out = densLen)) %>%
                pull(name) %>%
                density %>%
                "["(c("x", "y")) %>%
                as.tibble %>%
                rename(!!name := x, !!paste0("dens_", name) := y)
        } %>%
            bind_cols %>%
            mutate(.Group = group)
    } %>%
        bind_rows %>%
        mutate(.Group = as.factor(.Group))


    plts <- list()
    for(name in names) {
        trace <- (pltData %>%
            ggplot(aes_string(x = "ID", y = name,
                group = ".Group", col = ".Group")) +
            DefaultTheme() +
            geom_line() +
            #scale_color_manual(values = c("#000000", brewer.pal(9, "Set1")),
                #guide = FALSE) +
            xlab("Observation") +
            ylab(name)) +
            scale_color_hue(guide = FALSE)

        dens <- (densData %>%
            ggplot(aes_string(x = name, y = paste0("dens_", name),
                group = ".Group", col = ".Group")) +
            DefaultTheme() +
            geom_line() +
            #scale_color_manual(values = c("#000000", brewer.pal(9, "Set1")),
                #guide = FALSE) +
            xlab(name) +
            ylab(paste("Density of", name))) +
            scale_color_hue(guide = FALSE)

        plts <- append(plts, list(trace))
        plts <- append(plts, list(dens))
    }

    grobs <- GGPlot2GrobEx(plts)

    isFirstPageRequired <- names(dev.cur()) != "pdf"

    if (length(names) == 1) {
        grid.arrange(grobs = grobs, widths = c(1, 1),
            newpage = isFirstPageRequired)
    } else {

        n <- ceiling(length(names) / nPltRow)

        for (i in seq_int_len(n)) {
            pltInds <- (i - 1) * 2 * nPltRow + 1:(2 * nPltRow)
            pltInds <- Within(pltInds, c(1, 2 * length(names)))
            grid.arrange(grobs = grobs[pltInds],
                widths = c(1, 1), heights = rep(1, nPltRow),
                newpage = ifelse(i == 1, isFirstPageRequired, TRUE))
        }
    }
}