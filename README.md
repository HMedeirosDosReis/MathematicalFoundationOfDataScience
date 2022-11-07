# MathematicalFoundationOfDataScience
Project for MSSC-5931 Mathematical Foundations of Data Science class at Marquette University 

Meeting 11/7:
  Today: 
   - download dataset "folder A"
   - upload in R
   - function that fetch for min faces to be 20
   - make directory in R will create a "folder B" that only has the data set of min faces in it
   - load image
  Next Time:
    - some demographic analysis of data set
    - check scale of all images
    - import are images and change scale
  HW:
    - Get 20 pictures of ourselves
    - download data set
    - look at prcomp
    - magick - image_quanti

  Start Powerpoint
  - what is PCA
  - math behind it
  - whats are data set and what is the standards of pictures


  Project goal:
  - how many components be used that give a good amount of explained variance
  - predict name associated with the face with good accuracy and fast
        - Add our faces
  - add more...




------------------------
library(magick)
> image_read("/Users/hhenr/Downloads/lfw/lfw/lfw/Aaron_Eckhart/Aaron_Eckhart_0001.jpg")

prcomp

