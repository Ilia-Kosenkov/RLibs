Tools.AverageAndScatter <-
function(x, dx, limits = c(0.16, 0.84), nRuns = 1000, nUpd = 5, nSample = 10000) 
{
    require(rjags)
    
    n = min(length(x), length(dx))
    x = x[1:n]
    dx = dx[1:n]
    jagsModel = "
    model
    {
        mean ~ dunif(lower, upper)
        
        for(i in 1:length(obsX))
        {
            obsX[i] ~ dnorm(mean, 1.0/ (obsdX[i]^2))
        }
    }
    "

    meanEst = mean(x)
    size = max(x + dx) - min(x-dx)
    lower = meanEst - size
    upper = meanEst + size

    initials = list("lower" = lower, "upper" = upper, "obsX" = x, "obsdX" = dx)

    logConnection = textConnection(NULL, open = "w")

    modelText = textConnection(jagsModel)
    
    sink(logConnection)

    model = jags.model(modelText, initials, n.chains = 1, n.adapt = nRuns)
    for (i in 1:ifelse(nUpd < 1, 1, nUpd))
    {
        update(model, nRuns, quiet = TRUE)
    }

    result = coda.samples(model, "mean", nSample, thin = 10)[[1]]

    mean = mean(result)
    sigmas = quantile(result, limits)
    close.connection(modelText)
    sink()
    
    return (list("Mean" = mean, "Limits" = sigmas))
}
