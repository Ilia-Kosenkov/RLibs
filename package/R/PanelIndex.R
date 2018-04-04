#   MIT License
#
#   Copyright(c) 2017-2018 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

panelIndex = 1
panels = c('a', 'b', 'c', 'd', 'e', 'f', 'g')

PanelIndex = function(advance = TRUE, cex = 1.0)
{
    panel = panels[[panelIndex]]
    if (advance) {
        pi = panelIndex + 1
        assign("panelIndex", ifelse(pi > length(panels), 1, pi), envir = .GlobalEnv)
    }
    legend("topleft", legend = panel, bty = 'n', cex = cex)
}

SetPanelIndex = function(index) 
{
    assign("panelIndex", ifelse(index > length(panels), 1, index), envir = .GlobalEnv)
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