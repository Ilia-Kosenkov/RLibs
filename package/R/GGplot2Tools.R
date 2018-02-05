Lookup = function(object, ...) UseMethod("Lookup")

Lookup.gtable = function(grob, ...) {
    what = unlist(list(...))
    inds = sapply(what, function(i) which(grob$layout$name == i))
    return(list(GrobDesc = grob$layout[inds, ], Index = inds))
}

GetGrob = function(grob, ...) {
    inds = Lookup(grob, ...)$Index
    if (all(is.na(inds)))
        return(NULL)
    if (length(inds) == 1)
        return(grob$grobs[[inds]])
    return(grob$grobs[inds])
}

IsGrobNull = function(...) {
    args = list(...)
    return(sapply(args, function(x) "zeroGrob" %in% class(x), simplify = TRUE))
}

GetMargins = function(grob, type = c("inner", "outer")) {

    if (is.null(grob) || !("gDesc" %in% class(grob)))
        stop("`grob` is invalid.")
    worker = function(txtX, txtY) {
        ax.lr = Lookup(grob, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb = Lookup(grob, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr = ax.lr$GrobDesc$l
        inds.tb = ax.tb$GrobDesc$t
        return(list(t = grob$heights[inds.tb[1]], r = grob$widths[inds.lr[2]],
            b = grob$heights[inds.tb[2]], l = grob$widths[inds.lr[1]]))
    }

    if (!all(type %in% c("inner", "outer")))
        stop("Wrong `type`.")
    if (all(type == "inner"))
        return(worker("axis", "axis"))
    if (all(type == "outer"))
        return(worker("ylab", "xlab"))
    if (all(type %in% c("inner", "outer")))
        return(list(Inner = worker("axis", "axis"), Outer = worker("ylab", "xlab")))

}