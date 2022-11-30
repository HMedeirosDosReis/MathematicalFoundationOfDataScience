################## MAIN #######################

library(Rcpp)
library(imager)
library(tictoc)
library(ggplot2)
library(jpeg)
library(magick)
library(e1071)

source("") ###### For all the files

main <- function()
{
  ###########NEED FILTER DATA
  print("Which user? (0,1,2,3,4)")
  user<-readline()
  reduced_dir<-set_directory(user)
  if (length(list.files(reduced_dir)) == 0)
  {
    stop("Empty Folder")
  }
  folders <- dir(path = reduced_dir, pattern = NULL, all.files = FALSE,
                 full.names = FALSE, recursive = FALSE,
                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  if (length(folders) < length(folders))
  {
    stop("Missing at least 1 folder")
  }
  
  matrix_images <- create_matrix(folders)
  
  meanface <- matrix(rowMeans(matrix_images), nrow = n)
  
  # full PCA of image matrix -- need to do pca of transpose for some --------
  
  # do NOT center and scale, this causes the faces to become darker than we want
  
  tic("pca runtime")
  faces_pca <- prcomp(t(matrix_images), center = FALSE, scale. = FALSE)
  toc()
  
  importance <- as.data.frame(summary(faces_pca)$importance)
  imp_val <- check_importance(importance)
  
  # scree plot for PCA (first 10 components)
  
  var_explained = faces_pca$sdev^2 / sum(faces_pca$sdev^2)
  qplot(c(1:10), var_explained[1:10]) +
    geom_line() +
    xlab("Principal Component") +
    ylab("Variance Explained") +
    ggtitle("Scree Plot") +
    ylim(0, 1)
  
  response_var <- create_matrix(folders,nphotos=20,n=100, predictor=FALSE)
  
  mydata <- as.data.frame(cbind(response_var, faces_pca$x[,1:imp_val]))
  mydata$response_var <- as.factor(mydata$response_var)
  
  classifier <- svm(response_var ~ ., data = mydata)
  prediction <- predict(classifier, newdata = faces_pca$x[,1:imp_val])
  folders[as.integer(prediction)]
  
  
}
