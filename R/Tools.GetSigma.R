Tools.GetSigma <-
function(x)
{
    return(mean(x$Mean - x$Limits[[1]], x$Limits[[2]] - x$Mean))
}
