# CREATING MATRIX OF IMAGES (nxn)

create_matrix <- function(folders,nphotos=20,n=100, predictor=TRUE)
{
  if(predictor)
  {
    X <- c()
    
    tic("runtime")
    for (i in 1:length(folders)) 
    {
      name <- folders[i]
      setwd(paste0(reduced_dir, "/",name))
      photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                    pattern = NULL, all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
      for (j in 1:nphotos*0.8) 
      {
        pic <- grayscale(load.image(photos[j]))
        pic <- resize(pic, n, n)
        vector <- as.data.frame(pic)[,3]
        X <- cbind(X, vector)
      }
    }
    toc()
    return(X)
  }
  else
  {
    y<-c()
    
    for (i in length(folders)*0.8:20) 
    {
      
      setwd(paste0(reduced_dir, "/",folders[i]))
      photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                    pattern = NULL, all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
      y <- c(y, rep(i, each=20))
    }
    return(y)
  }
}
