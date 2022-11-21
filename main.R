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
}