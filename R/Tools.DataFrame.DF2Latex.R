Tools.DataFrame.DF2Latex <-
function(frame, file, frmt='%6.2f', printHeaders=TRUE, insMathHead=TRUE)
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
