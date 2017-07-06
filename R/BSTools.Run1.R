BSTools.Run1 <-
function(model, data, samples, N, initials = NA, M = 3, sample_each = 10, n_updates = 1)
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
            update(mdl, N)
        }

    message(sprintf("\r\nSampling..."))
    result = coda.samples(mdl, samples, N, sample_each)

    n = length(samples)


    assign("BSTools.Result", result, envir = .GlobalEnv)

}
