
RDevice = setRefClass("RDevice",
                      fields = list(
                        Index = "integer",
                        Name = "character"))


RDevice$methods("initialize" = function(...) {
    args = list(...)

    knownDevices = dev.list()

    if (length(args) > 0) {
        deviceCtor = args[[1]]
        if (!is.character(deviceCtor) && !is.integer(deviceCtor))
            stop("First argument should be either string name of a device init function (e.g. \"png\"), or index of existing device to hook to.")

        if (is.character(deviceCtor)) {
            if (length(args) > 1)
                do.call(deviceCtor, args = args[2:length(args)])
            else
                do.call(deviceCtor, args = list())
            }
    }
    else
        dev.new()

    newDevices = dev.list()

    if (length(newDevices) >= length(knownDevices)) {
        newDevInd = setdiff(newDevices, knownDevices)

        if (length(newDevInd) > 0 ) {
            Index <<- newDevInd[length(newDevInd)]
            Name <<- names(newDevices)[newDevices == .self$Index]
        }
        else if (length(args) > 0 &&
            is.integer(args[[1]]) &&
            as.integer(args[[1]]) %in% newDevices) {
            Index <<- args[[1]]
            Name <<- names(newDevices)[newDevices == .self$Index]
            .self$SetActive()
        }
        else
            stop("Created device is unavailable.")
    }
    else
        stop ("No new device was created.")
    })

RDevice$methods("IsAlive" = function() {
    knownDevices = dev.list()
    if (Name %in% names(knownDevices) && Index %in% knownDevices)
        return(TRUE)
    else
        return(FALSE)
    })

RDevice$methods("finalize" = function() {
    if (.self$IsAlive()) {
        invisible(dev.off(Index))
        message(sprintf("Device %d [%s] is disposed.", Index, Name))
    }
    else
        warning(sprintf("Device %d [%s] is no longer available and therefore cannot be disposed.", Index, Name))

    Index <<- as.integer(NA)
    Name <<- ""
})

RDevice$methods("Close" = function() { .self$finalize()})

RDevice$methods("IsActive" = function() {
    if (.self$IsAlive() && dev.cur() == Index)
        return(TRUE)
    else
        return(FALSE)
})

RDevice$methods("SetActive" = function() {
    if (.self$IsAlive()) {
        if(.self$IsActive())
            dev.set(Index)
    }
    else
        stop(sprintf("Device %d [%s] is no longer available and cannot be set active.", Index, Name))
})

RDevice$methods("GetProperties" = function() {
    if (.self$IsAlive()) {
        if (!.self$IsActive()) {
            activeDev = dev.cur()
            .self$SetActive()
            result = append(dev.capabilities(), value =
                list("size_px" = dev.size("px"), "size_in" = dev.size('in'), "size_cm" = dev.size("cm"),
                        "isInteractive" = dev.interactive()))
            .dev.set(activeDev)
        }
        else
            result = append(dev.capabilities(), value =
                list("size_px" = dev.size("px"), "size_in" = dev.size('in'), "size_cm" = dev.size("cm"),
                        "isInteractive" = dev.interactive()))

        return (result)
    }
    else
        return (NULL)
    })

RDevice$methods("GetType" = function() {
    if (.self$IsAlive()) {
        check = function(str) all(regexpr(str, Name) > 0)

        if (check("tikz"))
            return("tikz")
        else if (check("postscript"))
            return("ps")
        else if (check("pdf"))
            return("pdf")
        else if (check("ide"))
            return("ide")
        else if (check("png"))
            return("png")
        else if (check("jpeg"))
            return("jpeg")
    }
    else
        return (NULL)

})