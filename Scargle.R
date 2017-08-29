

Scargle.Tau = function(w, t)
{
    # Determines phase correction to Scargle periodogram
    # Args:
    #       w : Frequencies (2 pi nu)
    #       t : Time
    worker = function(locW, t) {
        if (locW < .Machine$double.xmin * 1e1)
            return(sum(t) / length(t))
        else
            return(1.0 / (2 * locW) * atan(sum(sin(2 * locW * t)) / sum(cos(2 * locW * t))))
    }
    
    return(sapply(w, worker, t))
}

Scargle.Periodogram = function(w, t, x, tau = NA)
{
    # Calculates acrgle periodogram
    # Args:
    #   w   : Frequencies (2 pi nu)
    #   t   : Time
    #   x   : Measurements
    #   tau : Phase correction. If NA, it is calculated

    if (all(is.na(tau)))
        tau = Scargle.Tau(w, t)

    args = lapply(1:length(w), function(i) return(c("w" = w[i], "tau" = tau[i])))

    

    worker = function(locArgs, t, x) {
        locW = locArgs["w"]
        locTau = locArgs["tau"]
        cosns = (sum(x * cos(locW * (t - locTau)))) ^ 2 / sum((cos(locW * (t - locTau))) ^ 2)
        if (locW < .Machine$double.xmin * 1e1)
            sins = (sum(x * (t - locTau))) ^ 2 / sum((t - locTau) ^ 2)
        else
            sins = (sum(x * sin(locW * (t - locTau)))) ^ 2 / sum((sin(locW * (t - locTau))) ^ 2)
        return(0.5 * (cosns + sins))
    }

     return(unlist(lapply(args, worker, t, x)))

}


