library("ggplot2")

GGPlot2.DefaultTheme = function() 
{
    return(
          theme(
                panel.border = element_rect(fill = NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank()
                )
           )
}

GGPlot2.Plot = function(...) 
{
    return(ggplot(...) + GGPlot2.DefaultTheme())
}