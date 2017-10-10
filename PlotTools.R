AssignDefaultConstants = function()
{
    # Assigns global plotting parameters

    # Default amount of pretty() ticks per x - axis
    .Plot.X.Axis.N.Ticks <<- 8
    # Default amount of pretty() ticks per y-axis
    .Plot.Y.Axis.N.Ticks <<- 6
    # Default offset (in respect to par()$usr) of tick labels, inch, y-axis
    .Plot.Y.Axis.Tick.Offset.Inch <<- 0
    # Default offset (in respect to par()$usr) of axis label, inch, y-axis
    .Plot.Y.Axis.Lab.Offset.Inch <<- 0.8 * par()$mai[[2]]
    # Default offset (in respect to par()$usr) of tick labels, inch, x-axis
    .Plot.X.Axis.Tick.Offset.Inch <<- 0
    # Default offset (in respect to par()$usr) of axis label, inch, x-axis
    .Plot.X.Axis.Lab.Offset.Inch <<- 0.5 * par()$mai[[1]]

   
}

AssignDefaultConstants()

# A class that describes axis
AxisDesc = setRefClass("AxisDesc",
    fields = list(
        Label = "character",        # Label of the axis
        NTicks = "integer",         # Number of ticks
        Breaks = "numeric",         # Custom ticks
        TickLabels = "character",   # Custom ticks' labels
        TransformFunc = "function", # 
        Index = "integer",
        LimX = "numeric",
        LimY = "numeric",
        LabelSize = "numeric",
        NamesSize = "numeric",
        DecimalDigits = "integer",
        ForceScientific = "logical",
        IsTeX = "logical"
    ))


ExpRep = function(x) {
    # Parses floating point number into its decimal base and power to be used in string representation
    # Args:
    #   x: Input float numbers (vector possible)
    #
    # Returns : list with two fields; 
    #           "Base" stores the normalized representation of the number (-10, -1] U {0} U [1, 10);
    #           "Power" stores the power, such as "Base" * 10^"Power" gives x.
    #           

    sign = sign(x)
    x = abs(x)

    power = floor(log10(x))
    base = 10 ^ (log10(x) - power)

    result = list("Base" = sign * base, "Power" = power)


    result$Power[is.nan(result$Base)] = 0
    result$Base[is.nan(result$Base)] = 0

    return(result)

}

BasicLabelsDrawer = function(
    x,
    forceScientific = FALSE,
    forceNormal = FALSE,
    powerLimit = 3,
    forceDigits = NA,
    tex = TRUE)
{
    # Transformation method that returns expression - type representations of labels
    # in form of X0.00 x 10^+-X0
    # Args:
    #   x               : A vector of floats to be converted to string-to-expression representations
    #   forceScientific : Optional flag that forces all numbers to be converted to scientific representation. 
    #                     Otherwise, Numbers between 0.0001 and 9999 are left as they are, without power multiplier
    #   forceNormal     : Forces decimal representation (equivalent of %f). Has the highest priority
    #   powerLimit      : Limits the power for which non-scientific representation is used
    #   forceDigits     : Manually limits the amout of decimal digits displayed after the decimal point.
    #                     Works only for %f - like representation
    #
    # Returns: An expression vector that can be used to plot this numbers


    # Retrieves Base x 10^Power representrations of input x numbers
    str = ExpRep(x)
    res = c()

    # Processes each input value
    for (i in 1:length(str$Base))
        # NAs are handled explicitly
            if (is.na(str$Base[i]))
            {
                res = c(res, parse(text = 'NA'))
            }
            else
                {
                # If exp representation is forced (flag) or if Power is outside of certain limits,
                # use full scientific representation Base x 10^Power
                if (!forceNormal & (forceScientific | (str$Power[i] < -powerLimit || str$Power[i] > powerLimit))) {
                    size = ifelse(is.na(forceDigits), 2, forceDigits)
                    if (tex) {
                        template = sprintf("%%.%df$\\\\times$10$^{%%.0f}$", size)
                        res = c(res, sprintf(template, str$Base[i], str$Power[i]))
                    }
                    else {
                        template = sprintf("'%%.%df'%%%%*%%%%10^'%%.0f'", size)
                        res = c(res, parse(text = sprintf(template, str$Base[i], str$Power[i])))
                    }
                }
                # otherwise, print plain number in form X0.00
                else
                 {
                    if (abs(str$Power[[i]] >= 0))
                    {
                        if (str$Power[[i]] > 2)
                            size = ''
                        else
                            size = sprintf("%d", 2 - str$Power[[i]])
                    }
                    else
                    {
                        size = sprintf('%d', 1 + abs(str$Power[[i]]))
                    }

                    if (!is.na(forceDigits)) 
                        size = forceDigits
                     
                    format = sprintf("%%.%sf", size)
                    if (tex)
                        res = c(res, sprintf(format, str$Base[i] * 10 ^ str$Power[i]))
                    else
                        res = c(res, parse(text = sprintf(format, str$Base[i] * 10 ^ str$Power[i])))
                }
            }
    return(res)
}

Pretty = function(x, n)
{

    base = pretty(x, n)
    m = length(base)
    if (m - n > 3)
        if (m %% 2 == 0)
            return(0.5 * (base[1:(m / 2) * 2] + base[1:(m / 2) * 2 - 1]))
        else
            return(base[1:((m + 1) / 2) * 2 - 1])
    else return (base)

}

AxisPlotter = function(
    params,
    ind,
    xlim,
    ylim,
    labs.size = 0.85,
    names.size = 1,
    decimalDigits = NA,
    forceScientific = FALSE,
    tex = FALSE)
{

    # An auxilary function to add axes
    # Args :
    #   params     : An *.axis.* argument passed to function
    #   ind        : An axis index. Ranges from 1 to 4, 1 correspond to bottom x-axis, clockwise.
    #   xlim       : Limits of x-type axes
    #   ylim       : Limits of y-type axes
    #   labs.size  : Relative size of tick labels
    #   names.size : Relative size of axis labels
    #

    # Orientation of axis, 1 - x, 0 - y
    orient = ind %% 2

    # A mapping from axis index to par()$usr limits
    par_ind = c(3, 1, 4, 2)

    # Picks up limit that should be used in braks computation
    lim = xlim * orient + (1 - orient) * ylim
    # If axis parameter is not empty (NAs)
    if (!all(is.na(params)))
    {
        # "Breaks" & "Labels" case
        if (all(c("Breaks", "Labels") %in% names(params))) {
            # Picks breaks inside limits and obtain string-to-expression representation
            breaks = params$Breaks
            selInds = breaks >= lim[1] & breaks <= lim[2]
            breaks = breaks[selInds]
            labels = params$Labels[selInds]       
        }
        # Otherwise, uses pretty() to get breaks and then labels
        else
            {
            # If "N" is supplied
            if ("N" %in% names(params))
                N = params$N
            else
                N = .Plot.X.Axis.N.Ticks * orient + (1 - orient) * .Plot.Y.Axis.N.Ticks

            breaks = Pretty(lim, N)

            breaks = breaks[breaks >= lim[1] & breaks <= lim[2]]

            # If transformation function is present
            if ("F" %in% names(params))
                labels = params$F(breaks)
            else
                labels = breaks

            labels = BasicLabelsDrawer(labels, forceScientific = forceScientific, forceDigits = decimalDigits, tex = tex)

        }


        # Adds axis with ticks, but no label and tick labels. Ticks inside
        Axis(side = ind, at = breaks, labels = NA, tcl = 0.3)

        # If axis is y-oriented
        if (orient == 0)
        {
            ind_2 = (ind) / 2

            # Size of plot in inch
            L = par()$pin[[1]]
            # In axis units
            sz = par()$usr[1:2]

            # Proportionality factor
            f = diff(sz) / L

            # Uses global settings to determine positions of axis tick labels and axis labels
            posTick = sz[[ind_2]] + sign(ind_2 - 1.5) * f * .Plot.Y.Axis.Tick.Offset.Inch
            posLab = sz[[ind_2]] + sign(ind_2 - 1.5) * f * .Plot.Y.Axis.Lab.Offset.Inch

               #print(c(posLab, sz[[ind_2]], f, .Plot.Y.Axis.Lab.Offset.Inch))

            # Puts tick labels
            text(y = breaks, x = posTick, labels = labels, xpd = TRUE, pos = ind, cex = labs.size)
            # Puts axis label
            text(y = mean(ylim), x = posLab,
                    labels = parse(text = ifelse("Lab" %in% names(params), params$Lab, '')),
                    xpd = TRUE, adj = c(0.5, 0.5), cex = names.size, srt = -90 + 180 * (ind / 2))
        }
        else
            {

            ind_2 = (ind + 1) / 2

            # Size of plot in inch
            L = par()$pin[[2]]
            # In axis units
            sz = par()$usr[3:4]

            # Proportionality factor
            f = diff(sz) / L

            # Uses global settings to determine positions of axis tick labels and axis labels
            posTick = sz[[ind_2]] + sign(ind_2 - 1.5) * f * .Plot.X.Axis.Tick.Offset.Inch
            posLab = sz[[ind_2]] + sign(ind_2 - 1.5) * f * .Plot.X.Axis.Lab.Offset.Inch

            # Puts tick labels
            text(x = breaks, y = posTick, labels = labels, xpd = TRUE, pos = ind, cex = labs.size)
            # Puts axis label
            text(x = mean(xlim), y = posLab,
                    labels = parse(text = ifelse("Lab" %in% names(params), params$Lab, '')),
                    xpd = TRUE, adj = c(0.5, 1 - (ind - 1) / 2), cex = names.size)
        }
    }
}

PlotSelection = function(
    frame,
    x.cols,
    y.cols,
    selected = NA,
    cols = "#000000",
    pchs = 19,
    ltys = 1,
    pch.size = 1,
    lty.size = 1,
    names.size = 1,
    labs.size = 0.85,
    bty = 'o',
    errorBar.size = 1,
    xlim = NA,
    ylim = NA,
    xlog = FALSE,
    ylog = FALSE,
    x.axis.1 = list("Lab" = "'x'"),
    y.axis.1 = list("Lab" = "'y'"),
    x.axis.2 = NA,
    y.axis.2 = NA,
    selected.by.ind = FALSE,
    selectionCol = NA,
    tex = FALSE
    )
{
    # Low-level method that plots data set, including selected and non-selected regions.
    # Args:  
    #   frame         : A data frame to be plotted
    #   x.cols        : A vector of data frame column names used to pick x-axis values. 
    #   y.cols        : A vector of data frame column names used to pick y-axis values. 
    #   selected      : A vector or list of vectors of data frame values of x.cols[[1]] column
    #                   that indicaete selected data
    #   cols          : N colors for each selection (or 1 to use across all)
    #   pchs          : N symbols for each selection (or 1 to use across all)
    #   ltys          : N line types for each selection (or 1 to use across all)
    #   pch.size      : N sizes for each selection (or 1 to use across all)
    #   lty.size      : N line widths passed to lines(lwd = [lty.size])
    #   names.size    : A relative size that affects axis labels. Passed to text (cex = [naems.size])
    #   labs.size     : A relative size that affects axis tick labels. Passed to text (cex = [naems.size])
    #   bty           : Same as par$bty
    #   errorBar.size : N error bar line widths for each selection (or 1 to use across all)
    #   xlim          : Specified by the caller x-axis limits. Can be used to tune plotting area
    #   ylim          : Specified by the caller y-axis limits. Can be used to tune plotting area 
    #   xlog          : Flag indicating if x axis scale should be log10
    #   ylog          : Flag indicating if y axis scale should be log10
    #   x.axis.1      : Accepts a specific 'list' parameter that determines behaviour of bottom x-axis
    #   x.axis.2      : Accepts a specific 'list' parameter that determines behaviour of top x-axis
    #   y.axis.1      : Accepts a specific 'list' parameter that determines behaviour of left y-axis
    #   y.axis.2      : Accepts a specific 'list' parameter that determines behaviour of right y-axis
    #
    #   *.axis.* parameter : 
    #       "Lab"     : Determines the axis label (string-to-expression)
    #       "Breaks"  : Specifies ticks to be plotted with the axis. Should be used with "Labels"
    #       "Labels"  : Supplies float values for each break in "Breaks". Should be used with "Breaks"
    #       "N"       : *OR* Can be specified to determine the amount of desired ticks, generated by pretty(n = [N])
    #       "F"       : A float-to-float function used to scale labels for given breaks. Can be passed with "N", does not work with
    #                   "Breaks" & "Labels"
    #       
    #
    # 



    # Scaling functions for x and y axis. Transform plot to log scale if needed
    if (xlog)
        FX = function(x) log10(x)
    else
        FX = function(x) x

    if (ylog)
        FY = function(y) log10(y)
    else
        FY = function(y) y

    if (is.na(selectionCol))
        selectionCol = x.cols[[1]]


    # A number of different selections
    N_init = NA

    # If selected is vector, then there are only selected and non-selected data
    if (class(selected) == "numeric")
        N_init = 2
    # If list, then the number of groups equals to length of the list
    else if (class(selected) == "list")
        N_init = length(selected)

    # If there are more than 1 selections 
    if(!is.na(N_init))
    {
        # Each paramter that should provide value for each group is checked.
        # If there is not enough elements in vector, fills it with the first value.
        # Can be used to specify only one value to use across N selections.
        # E.g., if selected is list of 10 groups, to plot all groups with the same symbol (pch = 19),
        # one can either pass pchs = rep(19,10), or pchs = 19. Last scenario is processed here.

        cols = InitDefault(cols, N_init)
        pchs = InitDefault(pchs, N_init)
        ltys = InitDefault(ltys, N_init)
        pch.size = InitDefault(pch.size, N_init)
        lty.size = InitDefault(lty.size, N_init)
        errorBar.size = InitDefault(errorBar.size, N_init)
    }


    # Determines xlim if it is not provided
    if (any(is.na(xlim)))
    {
        # If x has only one column - values
        if (length(x.cols) == 1)
            xlim = c(
                min(frame[[x.cols[[1]]]], na.rm = TRUE),
                max(frame[[x.cols[[1]]]], na.rm = TRUE))

        # If x has two columns - values +- errors
        else if (length(x.cols) == 2)
            xlim = c(
                min(frame[[x.cols[[1]]]] - frame[[x.cols[[2]]]], na.rm = TRUE),
                max(frame[[x.cols[[1]]]] + frame[[x.cols[[2]]]], na.rm = TRUE))

        #If x has three columns - values - errors1, values + errors2
        else if (length(x.cols) == 3)
            xlim = c(
                min(frame[[x.cols[[1]]]] - frame[[x.cols[[2]]]], na.rm = TRUE),
                max(frame[[x.cols[[1]]]] + frame[[x.cols[[3]]]], na.rm = TRUE))
        }

    # Determines ylim if it is not provided
    if (any(is.na(ylim)))
    {
        # If y has only one column - values
        if (length(y.cols) == 1)
            ylim = c(
                min(frame[[y.cols[[1]]]], na.rm = TRUE),
                max(frame[[y.cols[[1]]]], na.rm = TRUE))

        # If y has two columns - values +- errors
        else if (length(y.cols) == 2)
            ylim = c(
                min(frame[[y.cols[[1]]]] - frame[[y.cols[[2]]]], na.rm = TRUE),
                max(frame[[y.cols[[1]]]] + frame[[y.cols[[2]]]], na.rm = TRUE))

        # If y has three columns - values - errors1, values + errors2
        else if (length(y.cols) == 3)
            ylim = c(
                 min(frame[[y.cols[[1]]]] - frame[[y.cols[[2]]]], na.rm = TRUE),
                 max(frame[[y.cols[[1]]]] + frame[[y.cols[[3]]]], na.rm = TRUE))
        }

    # Scales x and y limits if plot is logarithmic
    xlim = FX(xlim)
    ylim = FY(ylim)

    # Creates new frame (compatible fith tikz)
    frame()
    # Creates the base of the plot with given scales. No axes or labels
    plot(NA, xlab = "", ylab = "",
        xlim = (xlim), ylim = (ylim), xaxt = 'n', yaxt = 'n', bty = bty, new = TRUE)

    # Computes flags that indicate if there are error bars
    plotXErr = (length(x.cols) > 1) 
    plotYErr = (length(y.cols) > 1) 

    # IF error bars should be plotted, pick up proper column names 
    # Handles both symmetric and non-symmetric errors
    if (plotXErr)
        x.cols.err = x.cols[c(2, length(x.cols))]

    if (plotYErr)
        y.cols.err = y.cols[c(2, length(y.cols))]

    # Strips inpit frame of NAs
    data = frame[!is.na(frame[[y.cols[1]]]),]

    Plot = function(sel, ind)
    {
        # Performs plot of selected chunk of data using one set of style parameters.
        # Args :
        #   sel : A vector of data[[x.cols[1]]] values that are selected in this group
        #   ind : Index that determines the style used. Used to retrieve style from style vectors,
        #         e.g., current pch is pchs[ind]
        #

        # Selects data in a group
        if (selected.by.ind)
            subData = data[sel,]
        else
            subData = data[data[[selectionCol]] %in% sel,]
        
        # Points plot
        points(FX(subData[[x.cols[1]]]), FY(subData[[y.cols[1]]]),
            pch = pchs[ind], col = cols[ind], cex = pch.size[ind])


        # Lines plot
        lines(FX(subData[[x.cols[1]]]), FY(subData[[y.cols[1]]]),
            lty = ltys[ind], col = cols[ind], lwd = lty.size[ind])

        # If error bars in x direction are present
        if (plotXErr)
            arrows(FX(subData[[x.cols[1]]] - subData[[x.cols.err[1]]]), FY(subData[[y.cols[1]]]),
                   FX(subData[[x.cols[1]]] + subData[[x.cols.err[2]]]), FY(subData[[y.cols[1]]]),
                   code = 3, length = 0, angle = 90, col = cols[ind], lwd = errorBar.size[ind])

        # If error bars in y direction are present
        if (plotYErr)
            arrows(FX(subData[[x.cols[1]]]), FY(subData[[y.cols[1]]] - subData[[y.cols.err[1]]]),
                   FX(subData[[x.cols[1]]]), FY(subData[[y.cols[1]]] + subData[[y.cols.err[2]]]),
                   code = 3, length = 0, angle = 90, col = cols[ind], lwd = errorBar.size[ind])
}

    # If selected is of unknown type (can be selected = NA)
    # Plots plain data with default ind = 1 style
    if (!class(selected) %in% c("numeric", "list"))
    {
        Plot(data[[selectionCol]], 1)
    }
    # If selected is numeric (a vector),
    # Plots selected data with style 1 and non-selected with style 2
    else if (class(selected) == "numeric")
    {
       
        if (selected.by.ind)
            Plot(!(1:nrow(data) %in% selected), 2)
        else
            Plot(data[!data[[selectionCol]] %in% selected, selectionCol], 2)

        Plot(selected, 1)
    }
    # If it is a list,
    # Plots only points that are specified in the list, one style per each group
    else if (class(selected) == "list")
    {
        for (i in 1:length(selected))
            Plot(selected[[i]], i)
    }

    
                                      
    # Calls axis plot function for each possible axis
    if(!all(is.na(x.axis.1)))
        AxisPlotter(x.axis.1, 1, xlim = xlim, ylim = ylim, labs.size = labs.size, names.size = names.size,
            forceScientific = ifelse(length(x.axis.1$ForceSc) == 0, FALSE, x.axis.1$ForceSc),
        decimalDigits = ifelse(length(x.axis.1$DecDgt) == 0, NA, x.axis.1$DecDgt), tex = tex)
    if (!all(is.na(y.axis.1)))
        AxisPlotter(y.axis.1, 2, xlim = xlim, ylim = ylim, labs.size = labs.size, names.size = names.size,
            forceScientific = ifelse(length(y.axis.1$ForceSc) == 0, FALSE, y.axis.1$ForceSc),
            decimalDigits = ifelse(length(y.axis.1$DecDgt) == 0, NA, y.axis.1$DecDgt), tex = tex)
    if (!all(is.na(x.axis.2)))
        AxisPlotter(x.axis.2, 3, xlim = xlim, ylim = ylim, labs.size = labs.size, names.size = names.size,
            forceScientific = ifelse(length(x.axis.2$ForceSc) == 0, FALSE, x.axis.2$ForceSc),
            decimalDigits = ifelse(length(x.axis.2$DecDgt) == 0, NA, x.axis.2$DecDgt), tex = tex)
    if (!all(is.na(y.axis.2)))
        AxisPlotter(y.axis.2, 4, xlim = xlim, ylim = ylim, labs.size = labs.size, names.size = names.size,
            forceScientific = ifelse(length(y.axis.2$ForceSc) == 0, FALSE, y.axis.2$ForceSc),
            decimalDigits = ifelse(length(y.axis.2$DecDgt) == 0, NA, y.axis.2$DecDgt), tex = tex)
}

Tex2Pdf = function(source) {
    require(stringr)
    wd = getwd()
    ind = max(unlist(stringr::str_locate_all(source, c("\\\\", "/"))), na.rm = TRUE)
    file = stringr::str_sub(source, ind + 1)
    dir = stringr::str_sub(source, 1, ind)

    tryCatch({
        setwd(dir)

        system(sprintf("texify %s --pdf --clean --quiet", file))
    }, finally = setwd(wd))
}


#z = data.frame(x = rnorm(100), y = runif(100))

#tikz("C:/Users/iliak/OneDrive/GOOGLEDRIVE/Development/R Libs/Debug/test.tex", width = 7, height = 5, standAlone = TRUE, engine = "pdftex")
#AssignDefaultConstants()
#PlotSelection(z, "x", "y", x.axis.1 = list("Lab" = "italic(phi)", "N" = 10), tex = TRUE)

#dev.off()

#Tex2Pdf("./Debug/test.tex")