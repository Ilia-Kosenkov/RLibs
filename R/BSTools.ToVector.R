BSTools.ToVector <-
function(data)
{
    N = length(data)
    result = rep(0.0, N)
    
    for (i in 1:N)
    {
        result[i] = data[i]
    }
    
    return (result)
}
