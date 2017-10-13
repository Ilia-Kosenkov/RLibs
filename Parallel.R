#   MIT License
#   
#   Copyright(c) 2017 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
#   
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#   
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#       FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#       OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.

Parallel.Packages = c("foreach", "parallel", "doSNOW")

Parallel.Available = function()
    return (all(Parallel.Packages %in% installed.packages()))


#Parallel.Lapply = function(x, fun, ..., export = c()) {
   
    #if (!Parallel.BackendExists())
        #stop("No parallel backend detected.")

    #else {
        #globals = ls(envir = .GlobalEnv)
        #export = c(export, globals[grepl("Parallel", globals)])
        
        #clusterExport(.Cluster, export)
        #return(foreach(
            #locX = x,
        ##.verbose = TRUE,
        #.final = function(lst) { names(lst) = x; return(lst) },
        #.multicombine = TRUE,
        #.export = export)
        #%dopar% fun(locX, ...))


    #}

#}
.symbs = c(97:122, 65:90, 48:57)
setOldClass("cluster")
setOldClass("SOCKcluster")
Cluster = setRefClass("Cluster",
    fields = list(
        ClusterDesc = "SOCKcluster",        
        IsRegistered = "logical",
        IsDisposed = "logical",
        ID = "character"))
Cluster$lock("ClusterDesc", "ID")

Cluster$methods("initialize" = function(nProcs) {
    if (!is.numeric(nProcs) || nProcs < 0 || nProcs > detectCores())
        stop("Illegal number of processes requested.")
    ClusterDesc <<- makeCluster(nProcs, "SOCK")
    IsRegistered <<- FALSE
    IsDisposed <<- FALSE
    ID <<- paste("@", rawToChar(as.raw(sample(.symbs, 8))), sep = "")
    message(sprintf("Cluster %s (%d nodes) was created.", .self$ID, .self$getClusterSize()))
})

Cluster$methods("getClusterDesc" = function() .self$ClusterDesc)
Cluster$methods("getIsRegistered" = function() .self$IsRegistered)
Cluster$methods("getClusterSize" = function() length(.self$ClusterDesc))
Cluster$methods("getID" = function() .self$ID)
Cluster$methods("getIsDisposed" = function() .self$IsDisposed)

Cluster$methods("Register" = function(backend = "SNOW")
{
    initCall = paste("registerDo", backend, sep = "")
    if (exists(initCall))
        eval(as.name(initCall))(ClusterDesc)

    IsRegistered <<- TRUE
    message(sprintf("Cluster %s (%d nodes) was registered.", .self$ID, .self$getClusterSize()))
})

Cluster$methods("Dispose" = function()
{
    stopCluster(ClusterDesc)
    IsRegistered <<- FALSE
    IsDisposed <<- TRUE
    message(sprintf("Cluster %s (%d nodes) was disposed.", .self$ID, .self$getClusterSize()))
})

Cluster$methods("finalize" = function()
    { if (!.self$IsDisposed)
        .self$Dispose()
})