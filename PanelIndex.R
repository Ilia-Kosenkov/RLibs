panelIndex = 1
panels = c('a', 'b', 'c', 'd', 'e', 'f', 'g')

PanelIndex = function()
{
    panel = panels[[panelIndex]]
    pi = panelIndex + 1
    assign("panelIndex", ifelse(pi > length(panels), 1, pi), envir = .GlobalEnv)

    legend("topleft", legend = panel, bty = 'n', cex = ifelse(exists("pi.cex"), pi.cex,par()$cex))
}

SetPanelIndex = function(index) 
{
    panelIndex = ifelse(index > length(panels), 1, index)
}

GetRightAxisPos = function() 
{
    L = par()$pin[[1]]
    l = par()$mai[[4]]
    sz = par()$usr[1:2]

    x1 = (sz[2] - sz[1]) * (1 + 5*l / (16 * L)) + sz[1]
    x2 = (sz[2] - sz[1]) * (1 + 3 * l / (4 * L)) + sz[1]
    return(c(x1, x2))
}