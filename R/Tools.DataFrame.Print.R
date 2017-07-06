Tools.DataFrame.Print <-
function(frame, file, frmt='%8.2f', printHeaders=TRUE, append=FALSE)
{
  temp = ''
  Nc = ncol(frame)
  Nr = nrow(frame)
  
  tryCatch(
    {
      sink(file, append = append)
  
      if(printHeaders)
      {
        for (i in 1:Nc)
        {
          if (length(frmt) > 1)
          {
            start = Tools.String.IndexOfChar(frmt[i], '%')+1
            stop = Tools.String.IndexOfChar(frmt[i], '.')-1
            if(stop < start)
            stop = max(Tools.String.IndexOfChar(frmt[i], 's'), Tools.String.IndexOfChar(frmt[i], 'd')) - 1
            
            headFrmt = sprintf("%s%ss", '%s%', substr(frmt[i], start, stop))
           
          }
          else
          {
            start = Tools.String.IndexOfChar(frmt[1], '%')+1
            stop = Tools.String.IndexOfChar(frmt[1], '.')-1
            if(stop < start)
              stop = Tools.String.IndexOfChar(frmt[i], 's')-1
            
            headFrmt = sprintf("%s%ss", '%s%', substr(frmt[1], start, stop))
            
          }
          temp = sprintf(headFrmt, temp, names(frame)[i])
        }
           writeLines(temp)
          temp =''
        
      }
        for(j in 1:Nr)
        {
          for (i in 1:Nc)
          {
            if (length(frmt) > 1)
            {
              bodyFrmt = sprintf("%s%s", '%s', frmt[i])
            }
            else
            {
              bodyFrmt = sprintf("%s%s", '%s', frmt[1])
            }
            
            temp = sprintf(bodyFrmt, temp, frame[j, i])
          }
          writeLines(temp)
          temp = ''
        }
      
     
    },
  
    finally = sink()
  )
    
}
