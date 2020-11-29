
#-------------------------Setting up the enviroment----------------------------
if (Sys.info()['sysname'] == "Windows"){
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    
}else{
    setwd(system("pwd", intern = T))}

## Installs if needed the packages specified in requirements.txt
#If there are libraries not added yet, please add them to the txt file
req <- read.delim('requirements.txt', header = F, sep = "\b")[,1]
for (i in req)
    if (!require(i, character.only = T))
        install.packages(i)
rm(i, req)

#--------------------------Vectorizing the dataset---------------------------
library(OpenImageR)
dim.rgb <- c(36000, 3)# dimenssion of the rgb matrix
dim.image <- c(200, 180, 3) # dim of the image

# Getting the names of the images
paths <- list.files('images')
# Since we have the images inside the folder data, we add the data directory to the path in order to make it work
paths <- paste('images',paths, sep = '/')



images <- matrix(0,150,200*180*3) # Allocating the space for the images
for(i in 1:150){
    rgb.vector <- readImage(paths[i])
    rgb.vector <- as.numeric(as.vector(rgb.vector))
    images[i,] <- rgb.vector 
}

# Labeling the images
labels <-rep(1:25,6)
labels <- sort(labels)
images <- cbind(labels, images)

save(images,file = 'rdata/images.RData')

load('rdata/images.RData')
