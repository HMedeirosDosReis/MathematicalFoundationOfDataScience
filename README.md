# MathematicalFoundationOfDataScience
Project for MSSC-5931 Mathematical Foundations of Data Science class at Marquette University 

Meeting 11/7:

  Today: 
   - download dataset "folder A"
   - upload in R
   - function that fetch for min faces to be 20
   - make directory in R will create a "folder B" that only has the data set of min faces in it
   - load image

Meeting 11/9:
  - Josh's work on PCA
  - Time issue
    - resize
  - Resized using package imager 
    - cleans data and scales all image
   - plots original, resized, reconstruct

  Next Time:
   - JEN: some demographic analysis of data set
   - import our images and change scale
   - potential error with black line at top image
   - HENRI: downside of reconstruction: you have to call the # picture it is 
     - create a vector that names goes with number on matrix
   - Clean data
     - make perfect environment that check if input is correct if not fail
   - how to use LR to predict
   - how to change pixel values so that the photos arent so dark
     - add to the gray scale maybe a percent
       - depending on pixel value, how would we choose that value
       - maybe when it not 0
  
 ------------------------
 HW:
   - Get 20 pictures of ourselves

  Start Powerpoint
  - what is PCA
  - math behind it
  - whats are data set and what is the standards of pictures
  - What libraries were used


  Project goal:
  - how many components be used that give a good amount of explained variance
  - predict name associated with the face with good accuracy and fast
    - Add our faces
  - add more...




------------------------
library(magick)
> image_read("/Users/hhenr/Downloads/lfw/lfw/lfw/Aaron_Eckhart/Aaron_Eckhart_0001.jpg")

prcomp

imager

Rcpp

tictoc

ggplot

jpeg
