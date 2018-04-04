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

#' @export RDevice
#' @exportClass RDevice
RDevice = setRefClass("RDevice",
                      fields = list(
                        Index = "integer",
                        Name = "character"
                        ))


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

RDevice$methods("finalize" = function(closing = FALSE) {
    if (.self$IsAlive()) {
        invisible(dev.off(Index))
        message(sprintf("Device %d [%s] is disposed.", Index, Name))
    }
    else if(closing)
        warning(sprintf("Device %d [%s] is no longer available and therefore cannot be disposed.", Index, Name))
    Index <<- as.integer(NA)
    Name <<- ""
})

RDevice$methods("Close" = function() { .self$finalize(TRUE)})

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