BSTools.Densities <-
function(plot = TRUE, rerun = TRUE)
{
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
