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

SetMargins = function(grob, type, margins) {
    worker = function(txtX, txtY) {
        ax.lr = Lookup(grob, paste(txtX, c("l", "r"), sep = "-"))
        ax.tb = Lookup(grob, paste(txtY, c("t", "b"), sep = "-"))
        inds.lr = ax.lr$GrobDesc$l
        inds.tb = ax.tb$GrobDesc$t
        return(list(t = inds.tb[1], r = inds.lr[2],
            b = inds.tb[2], l = inds.lr[1]))

    }

    if (all(type == "inner"))
        inds = worker("axis", "axis")
    else if (all(type == "outer"))
        inds = worker("ylab", "xlab")
    else
        stop("Wrong `type`.")

    if(!is.null(margins$t))
        grob$heights[inds$t] = margins$t
    if (!is.null(margins$b))
        grob$heights[inds$b] = margins$b
    if (!is.null(margins$l))
        grob$widths[inds$l] = margins$l
    if (!is.null(margins$r))
        grob$widths[inds$r] = margins$r

    return(grob)
}

DefaultTheme = function() {
    return(theme_bw() +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                theme(axis.ticks.length = unit(-3.5, "pt")) +
                theme(axis.text.x =
                    element_text(size = 10, margin = margin(t = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y =
                    element_text(size = 10, margin = margin(r = unit(10, "pt")), colour = "#000000")) +
                theme(axis.text.y.right =
                    element_text(size = 10, margin = margin(l = unit(10, "pt")), colour = "#000000")))
}

#Ticks <- function(range, n, breaks, labels, size) {
    #if (!missing(breaks) &&
        #!missing(labels) &&
         #is.numeric(breaks)) {
        #this <- list(
             #"breaks" = breaks,
             #"labels" = as.character(labels),
             #"n" = length(breaks),
             #"size" = if (missing(size)) NULL else size)
    #}
    #else if (!missing(range) &&
             #!missing(n)) {
        #breaks <- pretty(range, n)
        #this <- list(
            #"breaks" = breaks,
            #"labels" = as.character(breaks),
            #"n" = length(breaks),
            #"size" = if (missing(size)) NULL else size)
    #}
    #else
        #stop("Illegal initilization of `Ticks` instance.")

    #class(this) <- append(class(this), "Ticks")

    #return(this)
#}

#is.Ticks <- function(obj) {
    #return(all(c("Ticks", "list") %in% class(obj)))
#}

scale_x_custom <- function(type = "continuous", breaks, except = c(), ...) {

    params <- list(...)
    inds <- which(!breaks %in% except)
    params$breaks <- breaks[inds]
    params$labels <- rep("", length(inds))

    do.call(paste0("scale_x_", type), args = params)
}

scale_y_custom <- function(type = "continuous", breaks, ...) {

    params <- list(...)
    params$breaks <- breaks
    params$labels <- rep("", length(breaks))

    do.call(paste0("scale_y_", type), args = params)
}



GGCustomLargeTicks <- function(
    side,
    breaks,
    labels,
    min,
    max
   ) {

   if (side == 1) {
        append(
            foreach(br = seq(along = breaks),
                    lb = seq(along = labels)) %do% {
                        annotation_custom(
                            grob = textGrob(label = lb,
                                hjust = 0.5, vjust = 1.3),
                            xmin = br, xmax = br,
                            ymin = -Inf,
                            ymax = -Inf)
            },
            annotate(geom = "segment",
                            x = breaks, xend = breaks,
                            y = min, yend = max))
    }


}

#test <- function() {
    #a <- (tibble(x = 1:10, y = 1:10) %>%
        #ggplot(aes(x, y)) +
        #DefaultTheme() +
        #geom_line() +
        #scale_x_custom(breaks = pretty(c(1, 10), 100), except = 1:10, limit = c(-2, 10)) +
        #geom_point(color = "black") +
        #annotate(geom = "segment",
            #x = seq(2.5, 10, length.out = 6), xend = seq(2.5, 10, length.out = 6),
            #y = -Inf, yend = rep(10, 6)))

    #for (ann in GGCustomLargeTicks(side = 1, breaks = 1:10, labels = 10:1, min = -Inf, max = 1.0))
        #a <- a + ann

    #a <- a %>% ggplot_build() %>% ggplot_gtable()
    #a$layout$clip[a$layout$name == "panel"] <- "off"
    #grid.newpage()
    #grid.draw(a)
#}

#test()