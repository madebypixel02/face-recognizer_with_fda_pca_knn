#-------------------------SETTING UP ENVIROMENT----------------------------
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

#--------------------------PREPARING THE DATASET---------------------------

library(OpenImageR)

# Getting the names of the images
paths <- list.files('images')
# Since we have the images inside the folder data, we add the data directory to the path in order to make it work
paths <- paste('images',paths, sep = '/')

vectorize.images <- function(paths){
    images <- matrix(0,length(paths),200*180*3) # Allocating the space for the images
    for(i in 1:length(paths)){
        rgb.vector <- readImage(paths[i])
        rgb.vector <- as.numeric(as.vector(rgb.vector))
        images[i,] <- rgb.vector 
    }
    
    return(images)
}

images <- vectorize.images(paths)


# Labeling the images
labels <-rep(1:25,6)
labels <- sort(labels)
# images <- cbind(labels, images)

save(images,file = 'rdata/images.RData')
save(labels, file = 'rdata/labels.RData')
save(vectorize.images, file = 'rdata/vectorize_images.RData')

#-----------------------------------EXERCISE A----------------------------------
# We need it for doing mda
pca <- function(X, scale = T){ # Returns the matrix with the principal components
    #x_bar <- rep(0, dim(X)[2])
    X <- scale(X, center = T, scale = T)
    
    # Computing the covariance/correlation matrix
    Sigma <- X%*%t(X)
    
    # Getting the eigenvectors and eigenvalues
    eigen <- eigen(Sigma)
    
    P <- t(X) %*% eigen$vectors # Matrix with the eigenvectors
    
    PC <- X%*%P
    
    return(list(PC = PC, P=P, D=eigen$values))
    
}

# Actual fda
mda <- function(data, labels) {
    
    p <- ncol(data)
    q <- length(unique(labels))
    classes <- unique(labels)
    
    m_i = matrix(0,q,p)
    
    # For loop for the means
    j <- 1
    for (i in classes) {
        
        if (nrow(data[labels==i,])>1){
            m_i[j,] = colMeans(data[labels==i,])
        }else{
            m_i[j,] = data[labels==i,]
        }
        j <- j + 1
    }
    m = colMeans(data)
    
    # For loop for the Si, to get the global SW
    SW = matrix(0,p,p)
    for (i in 1:25) {
        SW = SW + cov(data[labels==i,])*(nrow(data[labels==i,])-1)
    }
    
    # For loop for the SB
    SB <- matrix(0,p,p)
    j <- 1
    for (i in classes) {
        SB = SB + (nrow(data[labels==i,])*(m_i[j,] - m) %*% t(m_i[j,] - m))
        j <- j + 1
    }
    
    eigen_stuff = eigen(solve(SW)%*%SB)
    
    return(list(m=m,P=eigen_stuff$vectors, D=eigen_stuff$values))
    
}

save(pca, file = 'rdata/pca.RData')
save(mda, file = 'rdata/mda.RData')
#----------------------------------EXERCISE B----------------------------------
load('rdata/pca.RData')
load('rdata/mda.RData')

# Classifier that only implements knn
knn.classifier <- function(parameters, newdata){  # the parameters and the new images
    results <- NULL
    for(f in 1:nrow(newdata)){ 
        test <- newdata[f,]

        dmatrix=dist(rbind(test,parameters$train), method = parameters$method, diag = TRUE, upper = TRUE)
        
        dmatrix=as.matrix(dmatrix)
 
        
        dmatrix=dmatrix[1,-1]

        ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE)
        
        labels_sel <- parameters$labels[ordenados$ix[1:parameters$k]]
        
        uniqv <- unique(labels_sel)
        
        
        votes <- tabulate(match(labels_sel, uniqv))
        
        uniqv <- uniqv[which(votes == max(votes))] 
        
        
        # The prediction is 0 whether there is a draw or whether the distance is greater than its or the threshold
        
        suppressWarnings(if(length(uniqv) == 1){
            distance <- mean(ordenados$x[which(labels_sel==uniqv)])
            
            if(parameters$threshold != FALSE){ # If threshold is not false, it should be a value or a vector
                if(length(parameters$threshold) > 1){   # For a customized threshold
                    
                    if (parameters$threshold[which(unique(parameters$labels) == uniqv)]< distance){
                        uniqv <- 0
                    }
                }else if(parameters$threshold < distance){ # For a general threshold
                    uniqv <- 0
                }
            }
            
        } else{
            uniqv <- 0
            distance <- mean(ordenados$x)
        }
        )
        
        results <- rbind(results,cbind(predicted.label = uniqv, distance = distance ))
        
    }
    
    if(parameters$details == F){
        return(results[,1])
    }else{
        return(results)
    }
}


# Classifier that takes into account knn, mda, and pca
classifier <- function(parameters, new.images){


    # Vectorizing the images if we get the path
    suppressWarnings(if(class(new.images) == "character"){
        new.images <- vectorize.images(new.images)
    })
    
    # pca
    pc <- pca(rbind(parameters$train, new.images))
    # projecting the data into the first n eigenvectors
    parameters$train <- parameters$train%*%pc$P[,1:12] # Train
    new.images.pca <- new.images%*%pc$P[,1:12]         # Test

    
    # fda
    fda <- mda(parameters$train, parameters$labels)
    # Projecting the data into the first n eigenvectors
    parameters$train <- parameters$train%*%fda$P[,1:parameters$nmda] # Train
    new.images.pca.fda <- new.images.pca%*%fda$P[,1:parameters$nmda] # Test

    # knn
    return(knn.classifier(parameters, new.images.pca.fda))
    
    
}

# Parameters
#parameters  <- list(train = images, labels = labels,               # Train data, train labels
#                    method = 'gaussian',                           # Distance method
#                    k = 1,                                         # Number of KNN
#                    threshold = F,                                 # FALSE if there is not threshold, vector is there is a threshold (size 1, general threshold; size = classes, customized threshold)
#                    nmda = 3,                                      # Number of projections retained
#                    details = T)                                   # Specify whether return just the predicted labels or return all the details (method, k, ...)

save(knn.classifier,file = 'rdata/knn_classifier.RData')
save(classifier, knn.classifier, pca, mda, vectorize.images, file = 'rdata/classifier.RData')
#-------------------------------------TRAINING----------------------------------
'-----------------EXTRA TEST FOR PLOTS---------------------'
"We are going to fix the number of pc in pca. In addition, we are going to bound 
the number of eigenvectors of fda that we are going to test on based on the variance"

load('rdata/images.RData')
load('rdata/labels.RData')
load('rdata/pca.RData')
load('rdata/mda.RData')

data = images
A = pca(data)
cumsum(A$D)/sum(A$D)
# We are keeping aronud 75 percent of variance 12 eigenvectors

B = mda(A$PC[,1:12], labels)

prop.var = B$D/sum(B$D)
cummulative.var = cumsum(B$D/sum(B$D))
prop.var
cummulative.var
# we are going to test among 4 and 11 eigenvectors (84 and 99 percent)

data.pca = A$PC[,1:12]

data.new = as.matrix(data.pca)%*%B$P[,1:3]
data.df <- cbind.data.frame(labels= labels,data.new)
colnames(data.df) = c("labels","LDA1","LDA2","LDA3")

# Scatterplot for two LDAs.
ggplot(data.df[,-1]/max(data.df[,-1]), aes(LDA1, LDA2, color=labels))+
    geom_point()

# Scatterplot for three LDAs.
library(scatterplot3d)
scatterplot3d(data.df[,-1]/max(data.df[,-1]), color = labels)


#----------------------------------k, %fda, method------------------------------
load('rdata/images.RData')
load('rdata/labels.RData')
load('rdata/vectorize_images.RData')
load('rdata/pca.RData')
load('rdata/mda.RData')
load('rdata/knn_classifier.RData')
load('rdata/classifier.RData')

#Getting the data

library(doParallel)
library(foreach)

ncores = detectCores()
cl = makeCluster(ncores-2)
registerDoParallel(cl)

k_folds <- 5
partitions <- sample(rep(1:k_folds, 150/k_folds), replace = F)

pred <-  foreach(j=c('manhattan', 'euclidean', 'canberra', 'maximum'), '.combine'= rbind)%do%{

            foreach(k=1:5, 'combine' = rbind)%do%{  
                               
                    foreach(o=4:10, '.combine' = rbind)%dopar%{ 
                        library(foreach)
                        library(doParallel)
                        
                        temp <- foreach(i=1:k_folds, '.combine' = rbind)%do%{
                            train.data <- images[partitions != i,]
                            train.labels <- labels[partitions !=i]
                            test.data <- images[partitions == i,]
                            test.labels <- labels[partitions == i]
                        
                            parameters  <- list(train = train.data, labels = train.labels,         
                                                method = j, k = k, threshold = F, 
                                                 nmda = o,                                    
                                                details=F)                                   
                            
                            cbind(test.labels, classifier(parameters, test.data))
                        }
                        cbind(round(mean(temp[,1]==temp[,2]),3),j, k, o )
                   
                }
            }
        
        
    }
stopCluster(cl)

# Converting data to the rigth format
predictions <- pred
predictions <- rbind(as.data.frame(predictions[1]),as.data.frame(predictions[2]),
                     as.data.frame(predictions[3]),as.data.frame(predictions[4]),
                     as.data.frame(predictions[5]),as.data.frame(predictions[6]),
                     as.data.frame(predictions[7]),as.data.frame(predictions[8]),
                     as.data.frame(predictions[9]),as.data.frame(predictions[10]),
                     as.data.frame(predictions[11]),as.data.frame(predictions[12]),
                     as.data.frame(predictions[13]),as.data.frame(predictions[14]),
                     as.data.frame(predictions[15]),as.data.frame(predictions[16]),
                     as.data.frame(predictions[17]),as.data.frame(predictions[18]),
                     as.data.frame(predictions[19]),as.data.frame(predictions[20])
                     )
colnames(predictions) <- c('Accuracy', 'Method', 'K', 'Eigenvectors.FDA')
predictions$Accuracy <- as.numeric(predictions$Accuracy)
predictions$K<- as.numeric(predictions$K)
predictions$Eigenvectors.FDA <- as.numeric(predictions$Eigenvectors.FDA)
#save(predictions, file = 'rdata/predictions.RData')


# Analysis
load('rdata/predictions.RData')
library(ggplot2)
ggplot(predictions) +
    aes(x = Eigenvectors.FDA, y = Accuracy, col = K) +
    geom_point() +
    facet_wrap(~Method)


filter1 <- which((predictions$Method == 'euclidean' | predictions$Method == 'manhattan' ))  # Getting only the euclidean and manhhatan results
ggplot(predictions[filter1,]) +
    aes(x = K, y = Accuracy, group = Method, col = Method )+
    geom_line(size = 1) +
    facet_wrap(~Eigenvectors.FDA)

ggplot(predictions[filter1,]) +
    aes(x = Eigenvectors.FDA, y = Accuracy, group = Method, col = Method )+
    geom_line(size = 1) +
    facet_wrap(~K)

" 
We are picking 
K = 1
fda eigenvectors = 6
mehtod = gaussian
"
#-----------------------------------threshold-----------------------------------
#Processing the impostors
impostors.path <- list.files('impostors')
impostors.path <- paste('impostors',impostors.path, sep = '/')
impostors.label <- rep(0, length(impostors.path))
impostors <- vectorize.images(impostors.path)
save(impostors.path, impostors.label, file ='rdata/impostors.RData')

# Getting the predictions and the distances with K = 1 nmda = 6, method = euclidean
k_folds <- 5
partitions <- sample(rep(1:k_folds, 150/k_folds), replace = F)
distances <- foreach(i=1:k_folds, '.combine' = rbind)%do%{
    train.data <- images[partitions != i,]
    train.labels <- labels[partitions !=i]
    test.data <- images[partitions == i,]
    test.labels <- labels[partitions == i]
    
    test.labels <- c(test.labels, impostors.label)
    test.data   <- rbind(test.data,impostors)
    
    parameters  <- list(train = train.data, labels = train.labels,          
                        method = 'euclidean', k = 1, threshold = F, 
                        nmda = 6,                                    
                        details=T)                                
    
    cbind(test.labels, classifier(parameters, test.data))
}

# Converting to the appropriate format
distances <- as.data.frame(distances)
distances$distance <- as.numeric(distances$distance)
#save(distances, file = 'rdata/distances.RData')


# Analysis
load('rdata/distances.RData')

filter2 <- which(distances$test.label == distances$predicted.label) # Getting the correct classifications
filter3 <- which(distances$test.label != distances$predicted.label) # Wrong classifications

ggplot() +
    geom_boxplot(data = distances[filter2,], aes(y = distance, x = predicted.label, group = predicted.label, col = 'blue')) +
    geom_boxplot(data = distances[filter3,], aes(y = distance, x = predicted.label, group = predicted.label, col = 'red'))


# Our threshold will be the mean + 3 standard deviations of each class
thresh<- NULL
filtered <- distances[filter2,]
for(i in unique(filtered$predicted.label)){
    d <- filtered$distance[filtered$predicted.label == i]
    m <- mean(d) + 3*sd(d)
    thresh <- rbind(thresh,cbind.data.frame(label = i,threshold=m))
}

ggplot() +
    geom_boxplot(data = distances[filter2,], aes(y = distance, x = predicted.label, group = predicted.label, col = 'RIGHT')) +
    geom_boxplot(data = distances[filter3,], aes(y = distance, x = predicted.label, group = predicted.label, col = 'WRONG')) +
    geom_point(data = thresh, aes(x = label, y = threshold, color = 'THRESHOLD')) +
    ylim(c(0,40000))

#---------------------------------------FINAL RESULT----------------------------
load('rdata/classifier.RData')
load('rdata/impostors.RData')
load('rdata/images.RData')
load('rdata/labels.RData')

final.labels <- c(labels, impostors.label)

parameters  <- list(train = images, labels = labels,               
                    method = 'euclidean', k = 1, threshold = thresh$threshold, 
                    nmda = 6,                                              
                    details = F)

result <- classifier(parameters, rbind(images,impostors))

# Accuracy
mean(result == final.labels)
# 1

save(parameters, file = 'rdata/parameters.RData')
