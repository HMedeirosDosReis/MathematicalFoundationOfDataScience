library(Rcpp)
library(imager)
library(tictoc)
library(ggplot2)
library(jpeg)

# # resizes to some new size
# tr <- resize(test, 720, 96/72*720)
# plot(test)
# plot(tr)

# # makes it a matrix
# as.matrix(tr)

# set wd to reduced images
# jdseidma for desktop, thema for laptop

reduced_dir <- "C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project/Images/Reduced Images"
setwd(reduced_dir)

folders <- dir(path = reduced_dir, pattern = NULL, all.files = FALSE,
               full.names = FALSE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# photos <- dir(path = paste0(reduced_dir, "/",folders[1]), pattern = NULL, all.files = FALSE,
#               full.names = FALSE, recursive = FALSE,
#               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# setwd(paste0(reduced_dir, "/",folders[1]))

# individual picture converted to vector

# set n = # pixels for width and height

n = 100

setwd(paste0(reduced_dir, "/",folders[1]))
photos <- dir(path = paste0(reduced_dir, "/",folders[1]), 
              pattern = NULL, all.files = FALSE,
              full.names = FALSE, recursive = FALSE,
              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
pic <- grayscale(load.image(photos[1]))
img <- resize(pic, n, n)
vector <- as.data.frame(img)[,3]

# comparing 250x250 image to nxn image

layout(t(c(1:2)))
plot(pic)
plot(img)


# converting picture to vector function for coding checks 
# (not utilized in the for loop to create matrix of images)

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

# name <- paste0(reduced_dir, "/",folders[1])
# photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
#               pattern = NULL, all.files = FALSE,
#               full.names = FALSE, recursive = FALSE,
#               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# length(photos)


## CREATING MATRIX OF IMAGES (nxn)

X <- c()

tic("runtime")
for (i in 1:length(folders)) {
  name <- folders[i]
  setwd(paste0(reduced_dir, "/",name))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  for (j in 1:length(photos)) {
    pic <- grayscale(load.image(photos[j]))
    pic <- resize(pic, n, n)
    vector <- as.data.frame(pic)[,3]
    X <- cbind(X, vector)
  }
}
toc()

# writing as a CSV file to prevent running above code every time

tic("writing csv")
write.csv(X,"C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project\\imagematrix.csv", row.names = FALSE)
toc()

# loading CSV file into R

setwd("C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project")

tic("loading in matrix")
X2 <- read.csv('imagematrix.csv')
toc()

# plotting mean face for fun (he don't look great)

meanface <- matrix(rowMeans(X), nrow = n)

writeJPEG(t(meanface), 'meanface.jpg')

# full PCA of image matrix -- need to do pca of transpose for some reason??

tic("pca runtime")
faces_pca <- prcomp(t(X), center = TRUE, scale. = TRUE)
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

# new pca with only first r principal components

tic("reduced pca runtime")
reduced_faces_pca <- prcomp(X2, center = TRUE, scale. = TRUE, rank. = r)
toc()

# scree plot for PCA

var_explained = faces_pca$sdev^2 / sum(faces_pca$sdev^2)
qplot(c(1:10), var_explained[1:10]) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# pca_img <- as.cimg(pca_pic$x[,1:50]%*%t(pca_pic$rotation[,1:50]))
# plot(pca_img)
# plot(pic)
# object.size(pic_mat)
# object.size(pca_pic$x[,1:50])
# 
# compressed <- pca_pic$x[,1:250]%*%t(pca_pic$rotation[,1:250])
# 
# writeJPEG(compressed, 'test_pca.jpg')
# 
# image_plot <- function(path, plot_name) {
#   require('jpeg')
#   img <- readJPEG(path)
#   d <- dim(img)
#   plot(0,0,xlim=c(0,d[2]),ylim=c(0,d[2]),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
#   title(plot_name, line = -0.5)
#   rasterImage(img,0,0,d[2],d[2])
# }
# 
# image_plot('test_pca.jpg', '50 components')


# why is each column of rotation not an eigenface???

setwd("C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project")

EigenFaces = faces_pca$rotation[,1:r]

ef_1 <- matrix(EigenFaces[,1], ncol = 1)
ef_1_mat <- matrix(ef_1, nrow = n)
writeJPEG(ef_1_mat, 'test_ef_1.jpg')

ef_2 <- matrix(EigenFaces[,2], ncol = 1)
ef_2_mat <- matrix(ef_2, nrow = n)
writeJPEG(ef_2_mat, 'test_ef_2.jpg')

ef_3 <- matrix(EigenFaces[,3], ncol = 1)
ef_3_mat <- matrix(ef_3, nrow = n)
writeJPEG(ef_3_mat, 'test_ef_3.jpg')


## to plot eigenfaces and reconstruct

# eigenfaces: make a matrix of the column of the rotation matrix (defined above)

plot(as.cimg(ef_1_mat))
plot(as.cimg(ef_2_mat))
plot(as.cimg(ef_3_mat))

# reconstruct: whatever # image it is, do plot as.cimg(matrix(restr[#,], ncol = n))

restr <- faces_pca$x[,1:r]%*%t(EigenFaces)

restr <- scale(restr, center = -1*faces_pca$center, scale = 1/faces_pca$scale)

# to plot these, need to rerun code at beginning of document

layout(t(1:3))
plot(pic)
plot(as.cimg(matrix(X[,1], ncol = n)))
plot(as.cimg(matrix(restr[1,], ncol = n)))


plot(as.cimg(matrix(restr[2,], ncol = n)))

layout(t(1:3))
plot(pic)
plot(img)
plot(as.cimg(matrix(restr[2,], ncol = n)))