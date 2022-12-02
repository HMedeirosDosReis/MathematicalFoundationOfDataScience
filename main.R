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
  
  
  # prepare to do SVM
  response_var <- c()
  for (i in 1:length(folders)) 
  {
    setwd(paste0(reduced_dir, "/",folders[i]))
    photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                  pattern = NULL, all.files = FALSE,
                  full.names = FALSE, recursive = FALSE,
                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    response_var <- c(y, rep(i, each=60))
  }
  
  # no PCA
  
  mydata <- as.data.frame(cbind(response_var, faces_pca$x))
  mydata$response_var <- as.factor(mydata$response_var)
  
  
  # split data
  
  tic("index PCA splitting")
  data_vec <- 1:length(mydata[,1])
  train_vec <- data_vec[data_vec%%60 <= 48 & data_vec%%60 != 0]
  val_vec <- data_vec[data_vec%%60 <= 54 & data_vec%%60 > 48]
  test_vec <- data_vec[data_vec%%60 <= 60 & data_vec%%60 > 54 | data_vec%%60 == 0]
  
  train <- mydata[train_vec,]
  val <- mydata[val_vec,]
  test <- mydata[test_vec,]
  toc()
  
  # choosing the best kernel parameter for our final model
  
  accuracy <- c()
  max <- 0
  b_gamma <- 0
  gammas <- c(0.1,0.01,0.001,0.0001,0.00001,0.000001)
  
  for (i in 1:length(gammas)) 
  {
    classifier <- svm(y ~ ., data = train, gamma = gammas[i], kernel = "radial")
    prediction <- predict(classifier, newdata = val)
    m <- sum(prediction == val$y)/length(prediction)
    accuracy <- c(accuracy, m)
    if (m == max(accuracy))
    {
      max <- m
    }
    if (i == length(gammas))
    {
      print(paste0("Best gamma is ", gammas[i]))
      b_gamma <- gammas[i]
    }
  }
  
  # accuracy of validation set
  
  classifier <- svm(y ~ ., data = train, gamma = b_gamma, kernel = "radial")
  prediction <- predict(classifier, newdata = val)
  sum(prediction == val$y)/length(prediction)
  
  # accuracy of test set
  
  prediction2 <- predict(classifier, newdata = test)
  sum(prediction2 == test$y)/length(prediction2)
  
  # with pca
  # index splitting ~0.01 s
  
  tic("index PCA splitting")
  data_vec <- 1:length(mydata[,1])
  train_vec <- data_vec[data_vec%%60 <= 48 & data_vec%%60 != 0]
  val_vec <- data_vec[data_vec%%60 <= 54 & data_vec%%60 > 48]
  test_vec <- data_vec[data_vec%%60 <= 60 & data_vec%%60 > 54 | data_vec%%60 == 0]
  
  train <- mydata[train_vec,]
  val <- mydata[val_vec,]
  test <- mydata[test_vec,]
  toc()
  
  # choosing the best kernel parameter for our final model
  
  accuracy <- c()
  max <- 0
  gammas <- c(0.1,0.01,0.001,0.0001,0.00001,0.000001)
  
  for (i in 1:length(gammas)) {
    classifier <- svm(y ~ ., data = train, gamma = gammas[i], kernel = "radial")
    prediction <- predict(classifier, newdata = val)
    m <- sum(prediction == val$y)/length(prediction)
    accuracy <- c(accuracy, m)
    if (m == max(accuracy)){
      max <- m
    }
    if (i == length(gammas)){
      print(paste0("Best gamma is ", gammas[i]))
    }
  }
  
  # accuracy of validation set
  
  classifier <- svm(y ~ ., data = train, gamma = 0.000001, kernel = "radial")
  prediction <- predict(classifier, newdata = val)
  sum(prediction == val$y)/length(prediction)
  
  # accuracy of test set
  
  prediction2 <- predict(classifier, newdata = test)
  sum(prediction2 == test$y)/length(prediction2)
}
