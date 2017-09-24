

Scargle.Tau = function(w, t)
{
    # Determines phase correction to Scargle periodogram
    # Args:
    #       w : Frequencies (2 pi nu)
    #       t : Time
    worker = function(locW, t) {
        
        if (F)
            return(sum(t) / length(t))
        else
            return(0.5/locW * atan2(sum(sin(locW * t)),  sum(cos(locW * t))))
    }
    
    return(sapply(w, worker, t))
}

Scargle.Periodogram = function(w, t, x, tau = NA)
{
    # Calculates scargle periodogram
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
        arg = locW * (t - locTau)
        cs = cos(arg)
        # cosns = (sum(x * cos(locW * (t - locTau)))) ^ 2 / sum((cos(locW * (t - locTau))) ^ 2)
        cosns = (sum(x * cs)) ^ 2 / sum(cs ^ 2)
        if (abs(locW) < .Machine$double.xmin * 1e1)
            sins = (sum(arg)) ^ 2 / sum((t - locTau) ^ 2)
        else {
            sn = sin(arg) 
            sins = (sum(x * sn)) ^ 2 / sum(sn ^ 2)
        }
        return(0.5 * (cosns + sins))
    }

     return(sapply(args, worker, t, x))

}


