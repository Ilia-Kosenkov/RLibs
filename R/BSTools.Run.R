BSTools.Run <-
function(model, initials, samples, N, M = 3, sample_each = 10, n_updates = 1)
{
  
  mdl = jags.model(model, initials, n.chains = M, n.adapt = N)
 
  for (i in 1:n_updates)
  {
    writeLines(sprintf("\r\nUpdating (%i)...", i))
    update(mdl, N)
  }
  
  writeLines(sprintf("\r\nSampling..."))
  result = coda.samples(mdl, samples, N, sample_each)
  
  n = length(samples)
  
  
  assign("BSTools.Result", result, envir = .GlobalEnv)
    
}
