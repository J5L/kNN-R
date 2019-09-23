#used libraries
library(tidyverse)
library(ggfortify)
library(grid)
library(gridExtra)

#set.seed(08121996)

myKNN <- function(Xtrain, Xtest, label, k, var1 = NULL, var2 = NULL) {

  #function to remove missing values in the data
  clean.data <- function(data) {
    cleandata <- na.omit(data)
    cleandata
  }
  
  remove.nonnumeric<- function(data) {
    cleandata <- data %>% select_if(is.numeric)
    cleandata
  }
  
  
  
  #function to visualize the data
  visualize.data <- function(data, predictor, var1=NULL, var2=NULL) {
   
    #if statement to plot data via PCA if there are more than two variables
    if (ncol(select(data, -predictor)) > 2) {
      dataPCA <- prcomp(scale(select(data, -predictor)))
      
      #png("dataPCA.png")
      dataPCAplot <- autoplot(dataPCA, data = data, colour = predictor)
      #dev.off()
      #else statement to plot scattergram 
    } else {
      qvar1 <- sym(var1)
      qvar2 <- sym(var2)
      qpred <- sym(predictor)
      
      #png("dataplot.png")
      datplot <- ggplot(data) +
        geom_point(aes(x=!!qvar1, y=!!qvar2, colour = !!qpred)) +
        ggtitle("2-dimensional Training Data with Predictor")
      #dev.off()  
    }
  }
  
  
  guess.knn <- function(x, train, trainlabels, k, l) {
    #distance function. parameter l denotes dimension of distance measure 
    #(l=2 euclidean distance)
    xmatrix <- matrix(as.numeric(x), nrow=nrow(train), ncol=length(x), byrow=T)
    #cat("xmatrix1", head(xmatrix))
    xmatrix <- (abs(as.matrix(train)-xmatrix))^l
    #cat("xmatrix2", head(xmatrix))
    
    dist <- (rowSums(xmatrix))^(1/l)
    #dist=finished distance matrix
    dist <- data.frame(distances=dist,label=trainlabels)
    
    dist <- (dist[order(dist$distances),])
    #order distance matrix, then select 1:k neighbours
    dist <- dist[1:k,]
    
    #table counts how often label is in k nearest neighbour, then sorted
    #descending and names[1] gives first 
    guess <- names(sort(-table(dist$label)))[1]
    
    return(guess)
  }
  
  
  knn <- function(train, test, trainlabels, k, trainsample=NULL, l=2) {
    #evaluate if test and train data set meet expectations
    if(ncol(train)!=ncol(test)) {
      stop("Training and test set must contain equal number of features.")
    }
    if(length(trainlabels)!=nrow(train)) {
      stop("Training labels must be of same length as training set.")
    }
    
    #trainsample could be another sample from big train data
    if(is.null(trainsample))
      trainsample <- nrow(train)
    
    subsample <- sample(1:nrow(train), trainsample, replace=F)
    
    train <- train[subsample,]
    trainlabels <- trainlabels[subsample]
    
    results <- apply(test, 1, function(x) guess.knn(x, train, trainlabels, k, l))
    
    return(results)
  }
  
  
  visualize.knn <- function(data, classifiedData, pred, var1=NULL, var2=NULL) {
    #show new classified points in scattergram
    if (is.null(var1) && is.null(var2)) {
      #var1 & var2 optional if only two variables in data, PCA is not necessary
      dataPCA <- prcomp(scale(data))
      data <- cbind(data, pred)
      
      xa<-autoplot(dataPCA, data = data, colour = "pred")
      
      classdataPCA <- prcomp(scale(select(classifiedData,-res)))
      
      xb<-autoplot(classdataPCA, data = classifiedData)
      
      xc<-autoplot(classdataPCA, data = classifiedData, colour = "res")
      
      #png("knnPCA.png")
      knnpcagrid <- grid.arrange(xa, xb, xc, ncol=3,
                    top = textGrob("Classified Data - Unclassified Data - Classified",
                                               gp=gpar(fontsize=10,font=2)))
      #dev.off()
      
    } else {
      
      qvar1 <- sym(var1)
      qvar2 <- sym(var2)
      #non standard evaluation to access ggplot objects via function
      a<-ggplot() +
        geom_point(data=data, aes(!!qvar1, !!qvar2, colour = pred), shape = 23)
      
      b<-ggplot() +
        geom_point(data=classifiedData, aes(!!qvar1, !!qvar2))
      
      c<-ggplot() +
        geom_point(data=data, aes(!!qvar1, !!qvar2, colour = pred), shape = 23) +
        geom_point(data=classifiedData, aes(!!qvar1, !!qvar2, colour = res))
      #png("knnvar.png")
      knngrid <- grid.arrange(a, b, c, ncol=3,
                 top = textGrob("Classified Data - Unclassified Data - Classified",
                                             gp=gpar(fontsize=10,font=2)))
      #dev.off()
    }
  }
  
#-------------------------------------------------------------------------------
  
  
  if (ncol(Xtrain) <= 4 && is.null(var1) && is.null(var2)) {
    stop("For 2-dimensional data please specify column names.")
  }
  
  if (is.null(var1) && is.null(var2)) {
    
  } else {
    Xtrain <- select(Xtrain, c(label, var1, var2))
    Xtest <- select(Xtest, c(label, var1, var2))
  }
  
  Xtrain <- clean.data(Xtrain)
  Xtest <- clean.data(Xtest)
  
  
  trainlabel <- select(Xtrain,c(label))
  
  
  train_ind <- sample(seq_len(nrow(Xtrain)), size = nrow(Xtrain))
  test_ind <- sample(seq_len(nrow(Xtest)), size = nrow(Xtest))
  
  train <- Xtrain[train_ind, ]
  test <- Xtest[test_ind, ]
  #extract label for classification purposes
  
  trainlabel <- trainlabel[train_ind,]

  #real training and test sets without the predictor
  trainreal <- select(train, -label)

  testreal <- select(test, -label)

  colnames(test)[colnames(test) == label] <- "newlabel"
  
  
  
  res <- knn(trainreal, testreal, trainlabel, k)
  result <- cbind(testreal,res)
  confmatrix <- table(result$res, test$newlabel)

  
  mys3 <- list(result = result, plotdata = visualize.data(Xtrain, label, var1, var2),
               plotknn = visualize.knn(data = trainreal, classifiedData = result,
                                       pred = trainlabel, var1, var2),
               confmatrix = confmatrix)
  
  class(mys3) = "myKNN"
  
  return(mys3)
}
