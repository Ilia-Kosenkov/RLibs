BSTools.Density <-
function(data)
{
    dens = density(data) 
    
    result = data.frame(dens$x, dens$y)
    names(result) = c("Quantity", "Density")
  
    
    return (result)
}
