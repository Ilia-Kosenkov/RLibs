lookup = function(object, ...) UseMethod("lookup")

lookup.gtable = function(grob, ...) {
    what = unlist(list(...))
    inds = sapply(what, function(i) which(grob$layout$name == i))
    return(grob$layout[inds, ])
}