BSTools.Analyze <-
function(result = BSTools.Result, qntls = c(0.05, 0.95))
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
