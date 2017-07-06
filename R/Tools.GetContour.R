Tools.GetContour <-
function(x, y, prob)
{
    require(MASS)
    dens = kde2d(x, y)

    Z = sort(dens$z)
    dx = diff(dens$x[1:2])
    dy = diff(dens$y[1:2])

    CZ = cumsum(Z) * dx * dy

    levels = sapply(prob, function(x)
    {
        approx(CZ, Z, xout = 1 - x)$y
    })

    return(contourLines(dens, levels = levels))
}
