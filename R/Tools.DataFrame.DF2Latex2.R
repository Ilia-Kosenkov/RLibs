Tools.DataFrame.DF2Latex2 <-
function(frame, file, frmt = '%6.2f', printHeaders = TRUE, 
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
