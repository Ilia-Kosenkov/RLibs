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

# Packages required for execution
.Parallel.Packages = c("foreach", "parallel", "doSNOW")
# Symbols (latin upper/lower-case letters & numbers 
# used to generate cluster UIDs
.Parallel.symbs = c(97:122, 65:90, 48:57)

# Checks if packages are installed
Parallel.Available = function()
    return (all(.Parallel.Packages %in% installed.packages()))

# Required, otherwise cannot declare fields of type [SOCKcluster]
setOldClass("cluster")
setOldClass("SOCKcluster")

# Cluster class declaration
Cluster = setRefClass("Cluster",
    fields = list(
        ClusterDesc = "SOCKcluster",        
        IsRegistered = "logical",
        IsDisposed = "logical",
        ID = "character"))
# ID is a unique identifier and should not be modified ater initialization.
# ID is immutable.
Cluster$lock("ClusterDesc", "ID")

# Initialization
Cluster$methods("initialize" = function(nProcs) {
    # Constructor.
    # Args:
    #   nProcs :  Number of processes (R sessions) to register.
    #             It is recommended to have nProcs < than amount of cores (hyperthreads).
    #             A good choice is detectCores() - 2.

    # Limits nProcs to [1, detecCores()] interval
    if (!is.numeric(nProcs) || nProcs < 0 || nProcs > detectCores())
        stop("Illegal number of processes requested.")

    # Creates cluster
    ClusterDesc <<- makeCluster(nProcs, "SOCK")
    # Not yet registered
    IsRegistered <<- FALSE
    # Not yet disposed
    IsDisposed <<- FALSE
    # Unique identifier. Dictionary is 62 symbols, 8 symbols are pulled, so probability of
    # identical IDs is about (1/62)^8 ~ 4.6e-15, providing [sample] has uniform distribution.
    ID <<- paste("@", rawToChar(as.raw(sample(.symbs, 8))), sep = "")
    # Log message
    message(sprintf("Cluster %s (%d nodes) was created.", .self$ID, .self$getClusterSize()))
})

# Accessors:
# Though it is not encouraged to use accessors in R, it allows to better structure code and introduce 
# value checks and other useful tricks.
# Here only getters are declared, thus direct access of fields is strongly discouraged.
Cluster$methods("getClusterDesc" = function() .self$ClusterDesc)
Cluster$methods("getIsRegistered" = function() .self$IsRegistered)
Cluster$methods("getClusterSize" = function() length(.self$ClusterDesc))
Cluster$methods("getID" = function() .self$ID)
Cluster$methods("getIsDisposed" = function() .self$IsDisposed)


Cluster$methods("Register" = function(backend = "SNOW")
{
    # Registers current claster
    # Args:
    #   backedn: Type of backend. !!! Tested only with SNOW

    # Tries to register backend assuming register method has a pattern of registerDoXXX
    # where XXX is backend
    initCall = paste("registerDo", backend, sep = "")

    # If such init method is present, initializes
    if (exists(initCall))
        eval(as.name(initCall))(ClusterDesc)
    else
        stop(sprintf("Assumed init function for backend %1$s (registerDo%1$s) is not found.", backend))
    # Marks cluster as initialzied
    IsRegistered <<- TRUE
    # Log message
    message(sprintf("Cluster %s (%d nodes) was registered.", .self$ID, .self$getClusterSize()))
})

Cluster$methods("Dispose" = function()
{
    # Explicit destructor. Frees resources/

    # Stops cluster
    stopCluster(ClusterDesc)
    # Changes flags
    IsRegistered <<- FALSE
    IsDisposed <<- TRUE

    # Log message
    message(sprintf("Cluster %s (%d nodes) was disposed.", .self$ID, .self$getClusterSize()))
})

Cluster$methods("finalize" = function()
{
    # Implicit destructor.
    # Called when [Cluster] object is garbage-collected.
    # If it was already disposed, does nothing, otherwise, calls Dispose()
    if (!.self$IsDisposed)
        .self$Dispose()
})