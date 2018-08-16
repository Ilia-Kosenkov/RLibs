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

SuppressNotes <- function(args) {
    for (var in args) {
        assign(var, NULL, envir = .GlobalEnv)
    }
}

SuppressNotes(".")

#' @export
Tools.DataFrame.DF2Latex = function(frame, file, frmt='%6.2f', printHeaders=TRUE, insMathHead=TRUE)
{
  sink(file)
  temp=""
  Nc = ncol(frame)
  Nr = nrow(frame)
  tryCatch(
    {
      if(printHeaders)
      {
        temp=sprintf("\\begin{tabular}{|")
        for (i in 1:Nc)
          temp=sprintf("%sc|",temp)
        
        temp=sprintf("%s}\n",temp)
        temp=sprintf("%s\t\\hline\n\t",temp)
        
        for(i in 1:(Nc-1))
        {
          if(insMathHead)
            temp=sprintf("%s$ %s $ & ",temp, names(frame)[i])
          else
            temp=sprintf("%s%s & ",temp, names(frame)[i])
        }
        if(insMathHead)
          temp=sprintf("%s$ %s $ \\\\",temp, names(frame)[Nc])
        else
          temp=sprintf("%s%s \\\\",temp, names(frame)[Nc])
        
        writeLines(temp)
        temp = ""
      }
      writeLines("\t\\hline")
      
      for(i in 1:Nr)
      {
        temp = "\t"
        
        for(j in 1:(Nc-1))
        {
          if(length(frmt) > 1)
            format = paste("%s ", frmt[j], " & ", sep = "")
          else
            format = paste("%s ", frmt[1], " & ", sep = "")
          temp = sprintf(format, temp, frame[i,j])
        }
        if(length(frmt) > 1)
          format = paste("%s ", frmt[Nc], " \\\\ ", sep = "")
        else 
          format = paste("%s ", frmt[1], " \\\\ ", sep = "")
        temp = sprintf(format, temp, frame[i,Nc])
        
        writeLines(temp)
      }
      
      writeLines("\t\\hline")
      writeLines("\\end{tabular}")
    },
    finally = sink()
  )
}

#' @export
Tools.DataFrame.DF2Latex2 = function(frame, file, frmt = '%6.2f', printHeaders = TRUE, 
                                     insMathHead = TRUE, insMathBody = FALSE, insMathBefore = FALSE, insMathAfter = FALSE,
                                     cols = 'c', NA.symb = NA_character_,
                                     beforeHead = NA, afterHead = NA)
{
    if (insMathBody)
        mB = '$'
    else
        mB = ''
    if (insMathHead)
        mH = '$'
    else
        mH = ''
    if (insMathBefore)
        mBe = '$'
    else
        mBe = ''
    if (insMathAfter)
        mAf = '$'
    else
        mAf = ''

    sink(file)
    temp = ""
    Nc = ncol(frame)
    Nr = nrow(frame)
    tryCatch(
    {
        if (printHeaders)
        {
            temp = sprintf("\\begin{tabular}{")
            
            if (length(cols) == 1)
                for (i in 1:Nc)
                    temp = sprintf("%s%s", temp, cols[[1]])
            else
                for (i in 1:Nc)
                    temp = sprintf("%s%s", temp, cols[[i]])

            temp = sprintf("%s}\n", temp)
            temp = sprintf("%s\t\\hline\n\t", temp)

            if (!all(is.na(beforeHead)))
            {
                for (j in 1:nrow(beforeHead))
                {
                    for (i in 1:ncol(beforeHead))
                    {
                        if (length(frmt) > 1)
                            frmt_t = frmt[[i]]
                        else
                            frmt_t = frmt[[1]]
                            
                        expr = regexpr("[0-9]+", frmt_t, perl = TRUE)

                        format = paste("%s ", mBe, "%", regmatches(frmt_t, expr), "s", mBe, ifelse(i == ncol(beforeHead), "\\\\\n\t", " & "), sep = "")
                        temp = sprintf(format, temp, beforeHead[j,i])
                    }
                }
            }

            for (i in 1:(Nc))
            {
               
                if (length(frmt) > 1)
                    frmt_t = frmt[[i]]
                else
                    frmt_t = frmt[[1]]

                expr = regexpr("[0-9]+", frmt_t, perl = TRUE)

                format = paste("%s ", mH, "%", regmatches(frmt_t, expr), "s", mH, ifelse(i == ncol(frame), "\\\\\n\t", " & "), sep = "")
                temp = sprintf(format, temp, names(frame)[i])
             }
          

            if (!all(is.na(afterHead)))
            {
                for (j in 1:nrow(afterHead))
                {
                    for (i in 1:ncol(afterHead))
                    {
                        if (length(frmt) > 1)
                            frmt_t = frmt[[i]]
                        else
                            frmt_t = frmt[[1]]

                        expr = regexpr("[0-9]+", frmt_t, perl = TRUE)

                        format = paste("%s ", mAf, "%", regmatches(frmt_t, expr), "s", mAf, ifelse(i == ncol(afterHead), "\\\\\n\t", " & "), sep = "")
                        temp = sprintf(format, temp, afterHead[j, i])
                    }
                }
            }
            #writeLines(temp)
            #temp = ""
       }

        writeLines(paste(temp, ifelse(nchar(temp) > 0, "", "\t"), "\\hline", sep = ""))

        for (i in 1:Nr)
        {
            temp = "\t"

            for (j in 1:(Nc - 1))
            {
                if (length(frmt) > 1)
                    frmt_t = frmt[[j]]
                else
                    frmt_t = frmt[[1]]
                if (is.na(frame[i, j]))
                {
                    expr = regexpr("[0-9]+", frmt_t, perl = TRUE)
                    
                    format = paste("%s ", mB,"%", regmatches(frmt_t, expr), "s", mB, " & ", sep = "")
                }
                else
                    format = paste("%s ", mB, frmt_t, mB, " & ", sep = "")

                temp = sprintf(format, temp, ifelse(is.na(frame[i, j]), NA.symb, frame[i,j]))
            }
            
            if (length(frmt) > 1)
                frmt_t = frmt[[Nc]]
            else
                frmt_t = frmt[[1]]
            if (is.na(frame[i, Nc]))
            {
                expr = regexpr("[0-9]+", frmt_t, perl = TRUE)
                format = paste("%s ", mB,"%", regmatches(frmt_t, expr), "s", mB, " \\\\ ", sep = "")
            }
            else
                format = paste("%s ", mB, frmt_t, mB,  " \\\\ ", sep = "")

           
            temp = sprintf(format, temp, ifelse(is.na(frame[i, Nc]), NA.symb, frame[i, Nc]))

            writeLines(temp)
        }

        writeLines("\t\\hline")
        writeLines("\\end{tabular}")
    },
    finally = sink())
}

#' @importFrom stringr str_length
Tools.String.IndexOfChar = function(string, charPattern)
{
  #require(stringr)
  
  n = str_length(string)
  
  for (i in 1:n)
  {
    if (substr(string, i,i) == substr(charPattern, 1, 1))
      return(i)
  }
  return (-1)
}

Tools.DataFrame.Print <- function(frame, file, frmt = "%8.2f",
                                 printHeaders = TRUE, append = FALSE) {
  temp <- ""
  Nc <- ncol(frame)
  Nr <- nrow(frame)

  tryCatch({
        sink(file, append = append)

        if (printHeaders) {
            sizes <- as.integer(
                sapply(regmatches(frmt, regexec("%([0-9]*)", frmt)),
                "[[", 2))
            if(any(is.na(sizes)))
                stop("Explicit column width is required in [frmt].")
            if(length(sizes) != ncol(frame))
                sizes <- rep(sizes[1], ncol(frame))

            hdrFrmt <- sapply(sizes, function(sz) sprintf("%%%ss", sz))
            header <- paste(
                sapply(seq_len(length(hdrFrmt)),
                    function(i) sprintf(hdrFrmt[i], names(frame)[i])),
                collapse = "")
            writeLines(header)
        }

        if (length(frmt) != ncol(frame))
            bodyFrmt <- rep(frmt[1], ncol(frame))
        else
            bodyFrmt <- frmt
        for(j in 1:Nr) {

            data <- as.list(frame[j, ])
            body <- paste(sapply(seq_len(length(bodyFrmt)),
                            function(i) sprintf(bodyFrmt[i], data[[i]])),
                          collapse = "")
            writeLines(body)
        }
    },
    finally = sink()
  )
}

Tools.Interpolate = function(x, args, vals)
{
  for (i in 1:(length(args)-1))
  {
    if ((x >= args[i]) & (x <= args[i+1]))
    {
      return(vals[i] + (vals[i+1] - vals[i]) * (x-args[i])/(args[i+1] - args[i]))
    }
  }
  
  return (NA)
}

Tools.AverageAndScatter = function(x, dx, limits = c(0.16, 0.84), nRuns = 1000, nUpd = 5, nSample = 10000) 
{
    #require(rjags)
    
    n = min(length(x), length(dx))
    x = x[1:n]
    dx = dx[1:n]
    jagsModel = "
    model
    {
        mean ~ dunif(lower, upper)
        
        for(i in 1:length(obsX))
        {
            obsX[i] ~ dnorm(mean, 1.0/ (obsdX[i]^2))
        }
    }
    "

    meanEst = mean(x)
    size = max(x + dx) - min(x-dx)
    lower = meanEst - size
    upper = meanEst + size

    initials = list("lower" = lower, "upper" = upper, "obsX" = x, "obsdX" = dx)

    logConnection = textConnection(NULL, open = "w")

    modelText = textConnection(jagsModel)
    
    sink(logConnection)

    model = jags.model(modelText, initials, n.chains = 1, n.adapt = nRuns)
    for (i in 1:ifelse(nUpd < 1, 1, nUpd))
    {
        update(model, nRuns, quiet = TRUE)
    }

    result = coda.samples(model, "mean", nSample, thin = 10)[[1]]

    mean = mean(result)
    sigmas = quantile(result, limits)
    close.connection(modelText)
    sink()
    
    return (list("Mean" = mean, "Limits" = sigmas))
}

#' @importFrom grDevices contourLines
Tools.GetContour = function(x, y, prob, n = 30)
{
    # Calculates contours for a given 2D distribution (pairs of [x_i, y_i])
    # and given probabilities (vectorized over latter).
    # Args :
    #   x    : x coordinates of sampled distribution points
    #   y    : y coordinates of sampled distribution points
    #   prob : Vector of probabilities (e.g. c(0.5, 0.68, 0.95, 0.995)).
    #   n    : The number of points used in kernel density estimator (MASS::kde2d).
    #          THe larger n the more detailed estimation is and the more time it takes.   
    # Returns: List of contours as generated by contourLines()

    if (!is.numeric(x) || !is.numeric(y))
        stop("x and y should be numeric.")

    if (length(x) != length(y))
        stop("x and y should be of the same length.")

    if (!is.numeric(prob) || prob < 0 || prob > 1)
        stop("prob should be a number from [0, 1] range.")

    if (!is.numeric(n) || n <= 1)
        stop("n should be a positive number at least greater than 1.")

    # Depends on MASS package
    #require(MASS)
    # Calculates 2D density
    dens = kde2d(x, y, n = n)

    # Aranges values in ascending order
    Z = sort(dens$z)
    # Bin size in x and y directions
    dx = diff(dens$x[1:2])
    dy = diff(dens$y[1:2])

    # Returns all cumulative sums:
    # z1; z1+z2; ...; z1+z2+z3+...zN
    # Times 2D bin size
    CZ = cumsum(Z) * dx * dy

    # For each prob gives Z
    levels = sapply(prob, function(x)
    {
        approx(CZ, Z, xout = 1 - x)$y
    })

    # Generates contour lines at appropriate levels
    cntrs = contourLines(dens, levels = levels)

    for (index in 1:length(levels))
        for (j in 1:length(cntrs)) {
            if (cntrs[[j]]$level == levels[index])
                cntrs[[j]]$prob = prob[index]
            }

    return(cntrs)
}

Tools.GetSigma = function(x)
{
    return(mean(x$Mean - x$Limits[[1]], x$Limits[[2]] - x$Mean))
}

Tools.Assign = function(var, env = .GlobalEnv)
{
    name = deparse(substitute(var))
    
    assign(name, var, envir = env)
}

Tools.Cart2Pol = function(x, ...) {
    args = list(...)
    if (length(args) == 1) {
        y = args[[1]]
    }
    else {
       
        y = x[[2]]
        x = x[[1]]
    }

    return(list("r" = sqrt(x^2+ y^2), "a" = atan2(y, x)))
}

Tools.Pol2Cart = function(r, a) {
    return(list("x" = r * cos (a), "y" = r * sin (a)))
}

Tools.Norm = function(x) {
    return (sqrt(sum(x^2)))
}

#' @export
Tools.IsWithin <- function(x, range) {
    return(sapply(x, function(item) item > range[1] & item < range[2]))
}

#' @importFrom rlang %||%
#' @export
`%??%` <- function(what, if.null) {
    warning("`%??%` is deprecated. Use `rlang::`%||%`.")
    return(what %||% if.null)
}

`+` <- function(e1, e2) UseMethod("+")
`+.default` <- function(e1, e2) .Primitive("+")(e1, e2)
`+.character` <- function(e1, e2)
    if (length(e1) == length(e2)) {
        paste(e1, e2, sep = "")
    } else stop("String Vectors of Different Lengths")

#' @export
WriteFixed <- function(frame, path, frmt = "%8.2f") {
    Tools.DataFrame.Print(frame, file = path, frmt = frmt)
}

#' @title Within
#' @description
#' Returns a subset of x such as x is
#' within [min(range), max(range)].
#' @param x Input subset.
#' @param range Range within which elements of x should be.
#' @return Subset of source vector x.
#' @export
Within <- function(x, range) {
    min <- min(range, na.rm = TRUE)
    max <- max(range, na.rm = TRUE)

    return(x[x >= min & x <= max])
}

#' @title seq_int_len
#' @param length.out Size of the sequence.
#' @return Sequence of ints.
#' @export
seq_int_len <- function(length.out) {
    seq.int(length.out = length.out)
}

#' @title seq_int_along
#' @param along.with Sequence along which indexes are genereated.
#' @return Sequence of ints.
#' @export
seq_int_along <- function(along.with) {
    seq.int(along.with = along.with)
}

#' @title raise
#' @param x Power.
#' @param y Base.
#' @return \code{y ^ x}
#' @export
raise <- function(x, y) y ^ x

#' @title Intersect
#' @param x First vector.
#' @param y Second vector.
#' @param tol The tolerance level.
#' @return Indices of first and second vector.
#' These elements are found to be equal within \code{tol}
#' @importFrom magrittr subtract is_weakly_less_than
#' @importFrom dplyr %>%
#' @export
Intersect <- function(x, y, tol = 0.1) {
    x %>% outer(y, subtract) %>%
        abs %>%
        is_weakly_less_than(tol) %>%
        which(arr.ind = TRUE) -> indices

    return(list(indices[, 1], indices[, 2]))
}

#' @title FancyStep
#' @param range The range within which the ticks are placed.
#' @param n Approximate number of desired ticks.
#' @param modifier Preferred tick placements/
#' @return Returns the size of the step.
#' @import dplyr
#' @importFrom magrittr multiply_by raise_to_power subtract equals
#' @export
FancyStep <- function(range,
    n = 6, modifier = c(1, 2.5, 5)) {

    modifier <- c(0.1 * modifier, modifier)

    # Gets the smallest base_10 step
    largeSteps <- range %>%
        diff %>% abs %>% log10 %>% floor %>% raise(10)

    # Calculates the number of intervals within range
    # for each modifer and selects the ones
    # that produce the closest to n amount of breaks.
    # If there are multuple matches, selects the smallest step
    # or largest number of breaks
    modInd <- largeSteps %>%
        multiply_by(modifier) %>%
        raise_to_power(-1) %>%
        multiply_by(range %>% diff %>% abs) %>%
        subtract(n) %>%
        abs %>%
        equals(min(.)) %>%
        which

    largeSteps * modifier[modInd] %>% min
}

#' @title Order
#' @description Returns ordered collection.
#' @param x Input collection.
#' @return Ordered \code{x}
#' @export
Order <- function(x) {
    x[order(x)]
}

#' @title WithinL
#' @description Returns \code{TRUE}/\code{FALSE} vector indicating
#' which elements are within the range.
#' @param x Input collection.
#' @param low Lower boundary.
#' @param upp Upper boundary.
#' @return Logical vector.
#' @export
WithinL <- function(x, low, upp) {
    x >= low & x <= upp
}

#' @title Log10Floor
#' @param x Input numeric vector.
#' @return Closest power of 10 that is smaller than or equal to the number.
#' @import dplyr
#' @export
Log10Floor <- function(x) {
    x %>% log10 %>% floor %>% raise(10)
}

#' @title Log10Ceiling
#' @param x Input numeric vector.
#' @return Closest power of 10 that is greater than or equal to the number.
#' @import dplyr
#' @export
Log10Ceiling <- function(x) {
    x %>% log10 %>% ceiling %>% raise(10)
}

#' @title RoundIntervalTo
#' @param x An input oredered vector of size 2 (Interval lims).
#' @param rnd Rounding base.
#' @return A vector of size 2, both limits of which are powers of \code{rnd}
#' @export
RoundIntervalTo <- function(x, rnd) {
    rnd * c(floor(x[1] / rnd), ceiling(x[2] / rnd))
}

#' @title GenerateLog10Breaks
#' @param ylim Axis limit.
#' @return log10 breaks.
#' @importFrom magrittr is_weakly_less_than extract2 multiply_by add
#' @import dplyr
#' @export
GenerateLog10Breaks <- function(ylim) {

    if (ylim %>% log10 %>% diff %>% is_weakly_less_than(1)) {
        mult <- ylim %>% min %>% Log10Floor
        yInt <- ylim / mult

        lStep <- yInt %>% FancyStep(4)
        sStep <- 0.1 * lStep

        yRng <- yInt %>% RoundIntervalTo(lStep)

        breaks <- list(
            Large = seq(yRng[1], yRng[2], by = lStep),
            Small = seq(yRng[1], yRng[2], by = sStep))

        inds <- Intersect(breaks$Large, breaks$Small, tol = 0.5 * sStep) %>%
            extract2(2)

        breaks$Small <- breaks$Small[setdiff(1:length(breaks$Small), inds)]

        breaks <- breaks %>%
            lapply(Within, range = yInt) %>%
            lapply(multiply_by, mult)

    } else {

        breaks <- ylim %>% log10 %>%
        GenerateBreaks(largeStep = 1,
            ticks = log10(1:9), op = add)

        breaks$Large <- breaks$Large %>%
            lapply(function(x)
                log10(c(0.2, 0.5, 1, 2, 5)) + x) %>%
            unlist %>%
            unique %>%
            Within(range = log10(ylim))

        inds <- Intersect(breaks$Large, breaks$Small, tol = 1e-4)[[2]]

        breaks$Small <- breaks$Small[setdiff(1:length(breaks$Small), inds)]

        breaks <- breaks %>%
            lapply(Within, range = log10(ylim)) %>%
            sapply(raise, 10)
    }

    return(breaks)
}

#' @title Clamp
#' @param ... Parameter.
#' @return Clamped numerics.
#' @export
Clamp <- function(...) {
    UseMethod("Clamp")
}

#' @title Clamp.numeric
#' @param ... Input parameters.
#' @return Clamps vector.
#' @export
Clamp.numeric <- function(...) {
    args <- list(...)
    x <- args[[1]]
    lower <- args[[2]]
    upper <- args[[3]]
    if (!missing(lower))
        x[x < lower] <- lower
    if (!missing(upper))
        x[x > upper] <- upper

    return(x)
}

#' @title Clamp.data.frame
#' @param .data Input \code{data.frame} or \code{tibble}.
#' @param .var Variable to clamp.
#' @param .range Clamp range.
#' @return \code{.data} whith clamped within \code{.range} column \code{.var}.
#' @importFrom dplyr mutate %>% if_else
#' @importFrom rlang enquo quo_squash !! :=
#' @export
Clamp.data.frame <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        mutate(!!expr := if_else(!!expr > .range[2], .range[2], !!expr)) %>%
        mutate(!!expr := if_else(!!expr < .range[1], .range[1], !!expr))
}

#' @title Expand
#' @param x Input interval.
#' @param factor How large the expansion is.
#' 1 corresponds to 100\% increase in size.
#' @param direction In which way to expand.
#' @return Expanded interval.
#' @export
Expand <- function(x, factor = 1, direction = c(1, 1)) {
    # Expands provided interval x factor times,
    # preserving the center of the interval
    # Args :
    #   x      : input interval
    #   factor : expanding factor
    # Returns :
    #   Expanded interval

    center <- mean(x)

    halfSize <- (diff(x)) / 2

    return(center + (c(-1, 1) * (1 + factor * direction)) * halfSize)
}

#' @title a_ch
#' @description A shortcut to \code{as.character}.
#' @param ... Arguments to convert.
#' @return Character representation of arguments.
#' @importFrom purrr map_chr
#' @export
a_ch <- function(...) map_chr(list(...), as.character)

#' @title Lin
#' @param x0 Where to interpolate.
#' @param x Arguments (size 2).
#' @param y Values (size 2).
#' @return Interpolated value between two provided.
#' @importFrom purrr map_dbl
#' @importFrom magrittr %<>%
#' @export
Lin <- function(x0, x, y) {
    x %<>% unlist %>% as.numeric
    y %<>% unlist %>% as.numeric

    map_dbl(x0, ~ y[1] + diff(y) / diff(x) * (. - x[1]))
}

#' @title UniqueWhichTol
#' @param x Vector to check.
#' @param tol Tolerance level for comparisons.
#' @return Indices of unique elements within given tlerance.
#' @importFrom dplyr %>%
#' @importFrom magrittr subtract is_less_than extract
#' @importFrom purrr map map2 map_lgl
#' @export
UniqueWhichTol <- function(x, tol = .Machine$double.eps) {
    x %>%
        outer(x, subtract) %>%
        abs %>%
        is_less_than(tol) %>% {
            map(seq_int_len(length(x)),
                function(x) extract(., x,))
        } %>%
        map2(seq_int_len(length(x)), ~ c(.y, which(.x))) %>%
        map_lgl(~.x[2] == .x[1]) %>%
        which
}

#' @title UniqueTol
#' @param x Vector to check.
#' @param tol Tolerance level for comparisons.
#' @return Unique elements of the vector.
#' @importFrom dplyr %>%
#' @importFrom magrittr extract
#' @export
UniqueTol <- function(x, tol = .Machine$double.eps) {
    x %>%
        extract(UniqueWhichTol(., tol))
}

#' @title BetweenWhich
#' @param x Ordered vector.
#' @param x0 Value to find.
#' @importFrom purrr map_lgl map
#' @importFrom rlang is_empty
#' @importFrom dplyr %>%
#' @export
BetweenWhich <- function(x, x0) {
    1:(length(x) - 1) %>%
        map_lgl(~(x[.x] <= x0 & x0 < x[.x + 1]) |
                 (x[.x + 1] < x0 & x0 <= x[.x])) %>%
        which %>% {
            if (is_empty(.))
                NA
            else
                map(., ~ .x + 0:1)
            }
}

#' @title FitlerRange
#' @param .data Input table.
#' @param .var Column to filter.
#' @param .range Limits on the column.
#' @return FIltered table.
#' @importFrom rlang enquo quo_squash !!
#' @importFrom dplyr %>% filter
#' @export
FilterRange <- function(.data, .var, .range) {
    expr <- quo_squash(enquo(.var))

    .data %>%
        filter(!!expr >= .range[1] & !!expr <= .range[2])
}

#' @title sec_len
#' @param x Object of some length (a list, a vector, etc)
#' @return Integer sequence from 1 to \code{length(x)}
#' @export
sec_len <- function(x)
    seq.int(length.out = length(x))

#' @title ReadList
#' @param file Path to a file
#' @description Reads specifically formatted sequences of integers from a file.
#' @return A list of indexes generated from file
#' @importFrom stringr str_split str_match str_detect str_replace_all
#' @importFrom purrr some keep map map_if
#' @importFrom magrittr extract extract2
#' @importFrom dplyr %>%
#' @export
ReadList <- function(file) {
    input <- scan(file, what = "", sep = "\n", quiet = TRUE)

    parse <- function(x) {
        str_split(x, ":") %>%
            extract2(1) %>% {
                if (length(.) == 1L)
                    as.numeric(.)
                else if (length(.) == 2L)
                    seq(as.numeric(.[1]), as.numeric(.[2]))
                else
                    stop("Failed to parse.")
                }
    }


    input %>%
        map(str_match,
            pattern = "([[[:alnum:]][[:punct:]]^:]*?)\\ *?:?\\ +?(.*)") %>%
        map(~list(extract(.x,, 2), extract(.x,, 3))) %>%
        map(function(x) {
            x[[2]] <- str_split(x[[2]], " ")[[1]]
            x[[2]] <- x[[2]] %>% extract(nzchar(.))
            if (x[[2]] %>% some(~str_detect(.x, "\"")))
                x[[2]] %<>%
                    str_replace_all("\"", "")
            else
                x[[2]] %<>% map(parse) %>% unlist

            return(x)
        }) %>%
        map(function(x) {
            if (str_detect(x[[1]], "\""))
                x[[1]] %<>% str_replace_all("\"", "")
            else
                x[[1]] %<>% parse

            return(x)
        }) %>%
        map_if(~length(.x) > 1, function(x) {
            negInds <- x[[2]] %>% keep(~.x < 0)
            x[[2]] %<>% keep(~.x >= 0) %<>% setdiff(abs(negInds))
            x
        }) -> parsed

    result <- parsed %>%
        map(function(x) if (length(x) > 1) extract2(x, 2) else NA) %>%
        setNames(parsed %>% map_chr(~extract(.x, 1) %>% as.character))

    return(result)
}