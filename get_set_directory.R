#---------------- NEEDS COMMENTING -----------------


set_directory <- function(option = 0)
{
  dire <- NULL
  if(option==0)
  {
    print("What is the path to the folder where all the pictures are located? ")
    dire <- readline()
  }
  else if(option==1)
    dire<-"C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project/Images/Reduced Images"
  else if(option==2)
    dire<-"C:/Users/thema/Dropbox/Topics in Math Stats 5931/Final Project/Images/Reduced Images"
  else if(option==3)
    dire<-"C:/Users/hhenr/Documents/test"
  else if(option==4)
    dire<-"C:/Users/jans7/OneDrive - Marquette University/Fall 2022/MSSC 5931 - Topics in Math or Stats/Project/NewFace_20"
  return(dire)
}
