

Scargle.Tau = function(w, t)
{
    worker = function(locW, t) return(1.0/(2*locW) * atan(sum(sin(2 * locW * t)) / sum(cos(2 *  locW * t))))
    
    return(sapply(w, worker, t))
}

Scargle.Periodogram = function(w, t, x, tau = NA)
{
    if (all(is.na(tau)))
        tau = Scargle.Tau(w, t)

    args = lapply(1:length(w), function(i) return(c("w" = w[i], "tau" = tau[i])))

    

    worker = function(locArgs, t, x) {
        locW = locArgs["w"]
        locTau = locArgs["tau"]
        return(  + 0.5 * ((sum(x * cos(locW * (t - locTau))))^2 / sum((cos(locW * (t - locTau))) ^ 2) +
            (sum(x * sin(locW * (t - locTau))))^2 / sum((sin(locW * (t - locTau))) ^ 2)))
    }

     return(unlist(lapply(args, worker, t, x)))

}


