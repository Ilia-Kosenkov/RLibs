Tools.String.IndexOfChar <-
function(string, charPattern)
{
  require(stringr)
  
  n = str_length(string)
  
  for (i in 1:n)
  {
    if (substr(string, i,i) == substr(charPattern, 1, 1))
      return(i)
  }
  return (-1)
}
