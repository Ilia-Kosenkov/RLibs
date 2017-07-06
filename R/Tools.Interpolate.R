Tools.Interpolate <-
function(x, args, vals)
{
  for (i in 1:(length(args)-1))
  {
    if ((x >= args[i]) & (x <= args[i+1]))
    {
      return(vals[i] + (vals[i+1] - vals[i]) * (x-args[i])/(args[i+1] - args[i]))
    }
  }
  
  return (NA)
}
