library(h2o)
library(plotly)
iris<-read.csv("Documents/ML/iris.csv",header = FALSE)

irishex<-h2o.uploadFile("Documents/ML/iris.csv",header = FALSE,destination_frame = "iris.hex")
islpit<-h2o.splitFrame(iris.hex)
irishex <-as.h2o(iris3)
summary(irishex)
svd<-h2o.partialPlot()
svd
irishex.shape
bayes@model$pcond
plot_ly()
#generalj tesztadatot eloszlassal

irishex <-as.h2o(iris)
islpit<-h2o.splitFrame(iris.hex)
iris.gbm2 <- h2o.gbm(y = 5, x = 1:4, 
                     training_frame= irishex, ntrees = 15, max_depth = 4, 
                     min_rows =2, learn_rate = 0.01)
iris.gbm2@model$training_metrics
h2o.naiveBayes()

d<-read.csv(file = 'abalone.data.csv',header = FALSE)