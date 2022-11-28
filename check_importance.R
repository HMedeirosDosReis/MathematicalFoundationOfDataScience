check_importance <- function(p_val=0.99, importance)
{
  for (i in 1:ncol(importance)) 
  {
    if(importance[3,i] < p_val)
    {
    } 
    else 
    {
      r <- i
      return(r)
    }
  }
}