# CREATING MATRIX OF IMAGES (nxn)

create_matrix <- function(folders,nphotos=60,n=100)
{

  X <- c()
    for (i in 1:length(folders))
    {
      setwd(paste0(reduced_dir, "/",folders[i]))
      photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                    pattern = NULL, all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
      for (j in 1:nphotos) 
      {
        pic <- grayscale(load.image(photos[j]))
        pic <- resize(pic, n, n)
        pic <- matrix(pic,nrow=n)[-c(c(1:floor(0.25*n)),c(floor(0.75*n+1):n)),-c(c(1:floor(0.25*n)),c(floor(0.75*n+1):n))]
        vector <- matrix(pic, ncol=1)[,1]
        X <- cbind(X, vector)
      }
    }
  return(X)
}
