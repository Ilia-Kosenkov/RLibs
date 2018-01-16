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
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#   
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#       FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#       OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.



#' @importFrom stats approx density quantile sd update
#' @importFrom utils setTxtProgressBar txtProgressBar install.packages

BSTools.Plots.Densities.PlotsPerRow = 4
BSTools.Result = NULL

BSTools.ToVector = function(data)
{
    N = length(data)
    result = rep(0.0, N)
    
    for (i in 1:N)
    {
        result[i] = data[i]
    }
    
    return (result)
}

BSTools.Density = function(data)
{
    dens = density(data) 
    
    result = data.frame(dens$x, dens$y)
    names(result) = c("Quantity", "Density")
  
    
    return (result)
}

BSTools.Analyze = function(result = BSTools.Result, qntls = c(0.05, 0.95))
{
    N = length(result)
    
    for (i in 1:N)
    {
        temp.df = as.data.frame(result[[i]])
        assign(sprintf("%s%0i", "BSTools.Result.Run", i), temp.df, envir = .GlobalEnv)
        
        temp.stats = data.frame(V1 = numeric(4))
        
        for (j in 1:ncol(temp.df))
        {
            temp.stats[1,j] = mean (temp.df[[j]])
            temp.stats[2,j] = sd (temp.df[[j]])
            qnt = quantile(temp.df[[j]], qntls)
            for (k in 1:length(qnt))
                temp.stats[2+k, j] = qnt[[k]]
        }
        
        names(temp.stats) = attributes(result[[i]])$dimnames[[2]]
        
        assign(sprintf("%s%0i%s", "BSTools.Result.Run", i, ".Stats"), temp.stats, envir = .GlobalEnv)
    }

    
}

#' @importFrom rjags jags.model coda.samples adapt

BSTools.Run = function(model, initials, samples, N, M = 3, sample_each = 10, n_updates = 1)
{
  
  mdl = jags.model(model, initials, n.chains = M, n.adapt = N)
 
  for (i in 1:n_updates)
  {
    writeLines(sprintf("\r\nUpdating (%i)...", i))
    adapt(mdl, N)
  }
  
  writeLines(sprintf("\r\nSampling..."))
  result = coda.samples(mdl, samples, N, sample_each)
  
  n = length(samples)
  
  
  assign("BSTools.Result", result, envir = .GlobalEnv)
    
}


BSTools.Run1 = function(model, data, samples, N, initials = NA, M = 3, sample_each = 10, n_updates = 1)
{
    message("\r\nStarting simulation...\r\n")
    if(!all(is.na(initials)))
        mdl = jags.model(
            file = model,
            data = data,
            inits = initials,
            n.chains = M,
            n.adapt = N)
    else
        mdl = jags.model(
            file = model,
            data = data,
            n.chains = M,
            n.adapt = N)

    if(n_updates > 0 )
        for (i in 1:n_updates)
        {
            message(sprintf("\r\nUpdating (%i)...", i))
            adapt(mdl, N)
        }

    message(sprintf("\r\nSampling..."))
    result = coda.samples(mdl, samples, N, sample_each)

    n = length(samples)


    assign("BSTools.Result", result, envir = .GlobalEnv)

}

#' @importFrom MASS kde2d
#' @importFrom graphics contour
BSTools.Densities = function(plot = TRUE, rerun = TRUE)
{
    #require(MASS)

    N_runs = length(BSTools.Result)
    
    pb = txtProgressBar(min = 0, max = 100, initial = 0, char = ">", style = 3, width = 60)
 
    if(plot)
    {
        factor = 2
    }
    else
    {
        factor = 1
    }
       
    for (i in 1:N_runs)
    {
       
        frameName = sprintf("%s%0i", "BSTools.Result.Run", i)
        frame = eval(as.name(frameName))
        
        N_var = ncol(frame)
        
        varNames = names(frame)
     
        for (j1 in 1:N_var)
            for(j2 in 1:N_var)
            {
                if (j1 != j2)
                {
                    tempFrameName = sprintf("%s%0i.%s.%s", "BSTools.Result.Run", i,  varNames[[j1]], varNames[[j2]])
                    densFrameName = sprintf("%s%0i.%s.%s.D", "BSTools.Result.Run", i,  varNames[[j1]], varNames[[j2]])
                    
                    if(rerun)
                    {
                        dens = kde2d(frame[[j1]], frame[[j2]])
                        
                        tempFrame = data.frame(X = dens$x, Y = dens$y)
                        tempDens = (dens$z)
                        
                        assign(tempFrameName, tempFrame, envir = .GlobalEnv)
                        assign(densFrameName, tempDens, envir = .GlobalEnv)
                    }
                    else
                    {
                        tempFrame = eval(as.name(tempFrameName))
                        tempDens = eval(as.name(densFrameName))
                    }
                    
                }
                
                else
                    
                {
                    tempFrameName = sprintf("%s%0i.%s", "BSTools.Result.Run", i, varNames[[j1]])
                    
                    if (rerun)
                    {
                        dens = density(frame[[j1]])
                        
                        tempFrame = data.frame(X = dens$x, D = dens$y)
                        
                        assign(tempFrameName, tempFrame, envir = .GlobalEnv)
                    }
                    else
                    {
                        tempFrame = eval(as.name(tempFrameName))
                    }
                    
                }
                
                setTxtProgressBar(pb, value = (100 * ((i-1) * N_var^2 + (j1-1) * N_var + j2) / (factor * N_var^2 * N_runs)))
              
            }
       
    }
    
    
    if(plot)
    {
        counter = 0
        for (g in 1:N_runs)
        {
        
            N_plts = BSTools.Plots.Densities.PlotsPerRow
            
            sz = min(N_var, N_plts)
            
            oldpar = par(oma = c(0,0,2,0), no.readonly = TRUE)
           
            if (N_var <= N_plts)
            {
                plot.new()
                
                par(mfrow = c(sz, sz))
                
                for (j1 in 1:N_var)
                    for(j2 in 1:N_var)
                    {
                        par(mfg = c(j2, j1))
                        if (j1 != j2)
                        {
                            tempFrameName = sprintf("%s%0i.%s.%s", "BSTools.Result.Run", g,  varNames[[j1]], varNames[[j2]])
                            densFrameName = sprintf("%s%0i.%s.%s.D", "BSTools.Result.Run", g,  varNames[[j1]], varNames[[j2]])
                            
                            tempFrame = eval(as.name(tempFrameName))
                            tempDens = eval(as.name(densFrameName))
                            
                            contour(tempFrame$X, tempFrame$Y, tempDens, xlab = varNames[[j1]], ylab= varNames[[j2]], main = "")
                        }
                        else
                        {
                            tempFrameName = sprintf("%s%0i.%s", "BSTools.Result.Run", g, varNames[[j1]])
                            
                            tempFrame = eval(as.name(tempFrameName))
                            
                            plot(tempFrame$X, tempFrame$D, xlab = varNames[[j1]], ylab = sprintf("Density of %s",varNames[[j1]]), main = "", type = 'l')
                        }
                       
                         counter = counter + 1
                        
                        setTxtProgressBar(pb, value = (100 * (counter + N_var^2 * N_runs) / (factor * N_var^2 * N_runs)))
                    }
                
                mtext (text = sprintf("Densities of Run #%i", g), outer = TRUE)
            }
            else
            {
                
                N_sheets = ceiling(N_var/N_plts)
                
                for(i1 in 1:N_sheets)
                {
                    
                    for(i2 in 1:N_sheets)
                    {
                        plot.new()
                        
                        if(i1 == i2)
                        {
                            
                            par(mfcol = c(N_plts, N_plts))
                            
                        }
                        else if(i1 > i2)
                        {
                            par(mfrow = c(N_plts, N_plts))
                        }
                        else
                        {
                            par(mfcol = c(N_plts, N_plts))
                        }
                        
                        for (j1 in 1:N_plts)
                            for(j2 in 1:N_plts)
                            {
                                k1 = j1 + (i1-1)*N_plts
                                k2 = j2 + (i2-1)*N_plts
                                par(mfg = c(j2, j1))
                                
                                if ((k1 <= N_var) && (k2 <= N_var))
                                {
                                    if (k1 != k2)
                                    {
                                        tempFrameName = sprintf("%s%0i.%s.%s", "BSTools.Result.Run", g,  varNames[[k1]], varNames[[k2]])
                                        densFrameName = sprintf("%s%0i.%s.%s.D", "BSTools.Result.Run", g,  varNames[[k1]], varNames[[k2]])
                                        
                                        tempFrame = eval(as.name(tempFrameName))
                                        tempDens = eval(as.name(densFrameName))
                                        
                                        contour(tempFrame$X, tempFrame$Y, tempDens, xlab = varNames[[k1]], ylab= varNames[[k2]], main = "")
                                    }
                                    else
                                    {
                                        tempFrameName = sprintf("%s%0i.%s", "BSTools.Result.Run", g, varNames[[k1]])
                                        
                                        tempFrame = eval(as.name(tempFrameName))
                                        
                                        plot(tempFrame$X, tempFrame$D, xlab = varNames[[k1]], ylab = sprintf("Density of %s",varNames[[k1]]) , main = "",  type = 'l')
                                    }
                                    counter = counter + 1
                                
                                }
                                else
                                {
                                    plot(1, type="n", axes=F, xlab="", ylab="")
                                }
                                
                                
                                setTxtProgressBar(pb, value = (100 * (counter +  N_var^2 * N_runs) / (factor * N_var^2 * N_runs)))
                               
                            } 
                        
                        mtext (text = sprintf("Densities of Run #%i", g), outer = TRUE)
                        
                    }
                    
                }
                
                
            }
            
            par(oldpar)
        }
    }
}

#' @importFrom rjags load.module parallel.seeds
BSTools.RNGs = function(n) {
    #require(rjags)
    load.module("lecuyer")

    return(parallel.seeds("lecuyer::RngStream", n))
}