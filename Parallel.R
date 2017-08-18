Parallel.Packages = function() return(c("foreach", "parallel", "doSNOW"))

Parallel.Available = function()
    return (all(Parallel.Packages() %in% installed.packages()))

Parallel.TryInit = function(workers =
    ifelse(("parallel" %in% sessionInfo()[[5]]) && exists("detectCores"), detectCores() - 2, 2))
{
    if (exists(".Cluster")) {
        if ("cluster" %in% class(.Cluster)) {
            message(sprintf("Detected cluster with %.0f nodes.", length(.Cluster)))
            return(TRUE)
        }
        else {
            message("Found variable '.Cluster' of different type. Cannot initialize.")
            return(FALSE)
        }
    }

    if (Parallel.Available() & (is.na(workers) | workers > 1)) {

        sapply(Parallel.Packages(), library, character.only = TRUE)
        .Cluster = makeCluster(workers, outfile = "./multithread.log")
        assign(".Cluster", .Cluster, envir = .GlobalEnv)
        registerDoSNOW(.Cluster)

        message(sprintf("Registered %d nodes for parallel execution with doSNOW", length(.Cluster)))

        return(TRUE)
    }
    else if (workers <= 1)
        message(sprintf("Not enough workers. (Supplied value is '%g')", workers))
    else {

        s = "Parallel backend packages are not installed.\r\nMissing packages:"
        index = 1
        for (pckg in setdiff(Parallel.Packages(), installed.packages())) {
            s = sprintf("%s\r\n%2.0f. %s", s, index, pckg)
            index = index + 1
        }

        s = sprintf("%s\r\n", s)

        message(s)
    }

    return(FALSE)

}

Parallel.TryDispose = function() {
    if (!exists(".Cluster")) {
        message("Cluster is not initialized.")
        return(FALSE)
    }
    else if (!"cluster" %in% class(.Cluster)) {
        message("Backend '.Cluster' variable is of different type. Aborting.")
        return (FALSE)
    }
        

    if (!("parallel" %in% sessionInfo()[[5]]) || !exists("stopCluster")) {
        if ("parallel" %in% installed.packages()) {
            message("Loading package 'parallel'")
            library("parallel")
        }
        
        else {
            message("Package 'parallel' is unavailable. Aborting.")
            return (FALSE)
        }
    }

    stopCluster(.Cluster)

    message("Cluster disposed.")

    rm(".Cluster", envir = .GlobalEnv)

    return (TRUE)
}

Parallel.BackendExists = function()
    return(exists(".Cluster") && ("cluster" %in% class(.Cluster)))

Parallel.Lapply = function(x, fun, ..., export = c()) {
   
    if (!Parallel.BackendExists())
        stop("No parallel backend detected.")

    else {
        globals = ls(envir = .GlobalEnv)
        export = c(export, globals[grepl("Parallel", globals)])
        
        clusterExport(.Cluster, export)
        return(foreach(
            locX = x,
        #.verbose = TRUE,
        .final = function(lst) { names(lst) = x; return(lst) },
        .multicombine = TRUE,
        .export = export)
        %dopar% fun(locX, ...))


    }

}