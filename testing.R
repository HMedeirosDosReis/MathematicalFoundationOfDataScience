library(Rcpp)
library(imager)
library(tictoc)
library(ggplot2)
library(jpeg)
library(magick)
library(e1071)


# set wd to reduced images ------------------------------------------------

# Josh wd desktop and laptop: 

reduced_dir <- "C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project/Images/Reduced Images"
reduced_dir <- "C:/Users/thema/Dropbox/Topics in Math Stats 5931/Final Project/Images/Reduced Images"

# Henri wd: 

reduced_dir <- 

# Jennifer wd:
  
reduced_dir <- "C:/Users/jans7/OneDrive - Marquette University/Fall 2022/MSSC 5931 - Topics in Math or Stats/Project/NewFace_20"

# setting working directory -----------------------------------------------

setwd(reduced_dir)

# checking for empty folders ----------------------------------------------

if (length(list.files(reduced_dir)) == 0){
  stop("Empty Folder")
}

folders <- dir(path = reduced_dir, pattern = NULL, all.files = FALSE,
               full.names = FALSE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

if (length(folders) < length(folders)){
  stop("Missing atleast 1 folder")
}

# converting picture to vector function for coding checks  ----------------

# (not utilized in the for loop to create matrix of images)
n = 100

pic_to_vector <- function(i, j){
  setwd(paste0(reduced_dir, "/",folders[i]))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  pic <- grayscale(load.image(photos[j]))
  pic <- resize(pic, n, n)
  vector <- as.data.frame(pic)[,3]
  return(vector)
}

# Putting data into matrix -----------------------------------------

# CREATING MATRIX OF IMAGES (nxn)

X <- c()

tic("runtime")
for (i in 1:length(folders)) {
  name <- folders[i]
  setwd(paste0(reduced_dir, "/",name))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  for (j in 1:20) {
    pic <- grayscale(load.image(photos[j]))
    pic <- resize(pic, n, n)
    vector <- as.data.frame(pic)[,3]
    X <- cbind(X, vector)
  }
}
toc()

# plotting mean face for fun (he don't look great)

meanface <- matrix(rowMeans(X), nrow = n)

# full PCA of image matrix -- need to do pca of transpose for some --------

# do NOT center and scale, this causes the faces to become darker than we want

tic("pca runtime")
faces_pca <- prcomp(t(X), center = FALSE, scale. = FALSE)
toc()

# checking for total explained variance limit
# paramater p: our goal for explained variance

importance <- as.data.frame(summary(faces_pca)$importance)

p = 0.99

for (i in 1:ncol(importance)) {
  if(importance[3,i] < p){
  } else {
    r <- i
    print(r)
    break
  }
}

# scree plot for PCA (first 10 components)

var_explained = faces_pca$sdev^2 / sum(faces_pca$sdev^2)
qplot(c(1:10), var_explained[1:10]) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# plotting Eigenfaces and reconstructing images ---------------------------

# faces_pca$x are the scores of the images

setwd("C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project")
setwd("../..")

# first eigenface and plot

EigenFaces <- faces_pca$rotation[,1:r]

ef_1 <- matrix(EigenFaces[,1], ncol = 1)
ef_1_mat <- matrix(ef_1, nrow = n)

plot(as.cimg(ef_1_mat))

# reconstruct: whatever # image it is, do plot as.cimg(matrix(restr[#,], ncol = n))

# restr <- faces_pca$x[,1:r]%*%t(EigenFaces)
# 
# restr <- scale(restr, center = -1*faces_pca$center, scale = 1/faces_pca$scale)


# random person sampling --------------------------------------------------

rperson <- sample(1:length(folders), 1)

setwd(paste0(reduced_dir, "/",folders[rperson]))
photos <- dir(path = paste0(reduced_dir, "/",folders[rperson]), 
              pattern = NULL, all.files = FALSE,
              full.names = FALSE, recursive = FALSE,
              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

rpic <- sample(1:length(photos), 1)

pic <- grayscale(load.image(photos[rpic]))

img <- resize(pic, n, n)

place <- 0

for (i in 1:(rperson-1)) {
  setwd(paste0(reduced_dir, "/",folders[i]))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  place <- length(photos) + place
}

place <- place + rpic

layout(t(1:3))
plot(pic)
plot(as.cimg(matrix(X[,place], ncol = n)))
plot(as.cimg(matrix(restr[place,], ncol = n)))
title(name_associated_w_pic_num(reduced_dir, place))

# trying SVM --------------------------------------------------------------

y<-c()

for (i in 1:length(folders)) {
  setwd(paste0(reduced_dir, "/",folders[i]))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  y <- c(y, rep(i, each=20))
}

y <- data.frame(y = y)

data <- cbind(y, faces_pca$x[,1:])

classifier <- svm(faces_pca$x[,1:r], y)
prediction <- predict(classifier, t(testing))
folders[as.integer(round(prediction))]

#plot(as.cimg(matrix(ourpred, ncol = n)))
#title(title(name_associated_w_pic_num(reduced_dir, place)))
