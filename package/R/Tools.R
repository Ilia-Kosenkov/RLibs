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
Tools.IsWithin = function(x, range) {
    return(sapply(x, function(item) item > range[1] & item < range[2]))
}

#' @export
`%??%` <- function(what, if.null) ifelse(is.null(what), if.null, what)

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
#' @returns Subset of source vector x.
#' @export
Within <- function(x, range) {
    min <- min(range, na.rm = TRUE)
    max <- max(range, na.rm = TRUE)

    return(x[x >= min & x <= max])
}
