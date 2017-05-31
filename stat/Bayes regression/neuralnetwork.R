library(sigmoid)
library(plotly)

myf <- function(x,errmean,errsd) {
  x^3-x^2 + rnorm(n = 1,mean = errmean, sd = errsd)
}

genseq  <- function(n=27,errm=0,errsd=.5) {
  X<-seq(from=-2,to=2,length.out = n)
  Y<-unlist(lapply(X,FUN = myf,errm, errsd))
  as.data.frame(list(x=X,y=Y))
}


n<-27 #samplesize
errm<-0 #error mean
errsd <- 0.1  #error sd
l1size <- 6 #first hidden l.
l2size <- 3 # 2.hidden l
maxit <- 1000 #num of iterations
alp <- 0.1 #learning rate

s <- genseq(n,errsd = errsd) #generate the data
plot(x=s$x,y=s$y)

w1 <- matrix(data = runif(n = length(Xtrain[1,])*l1size,min = 0,max = 0.0001),nrow = l1size)
w2 <- matrix(data = runif(n = (l1size+1)*l2size,min = 0,max = 0.0001),nrow = l2size)
v <- matrix(data = runif(n = l2size+1,min = 0,max = 0.0001),ncol = l2size+1)

for(it in 1:maxit){
  for(i in 1:n){
    x <- matrix(nrow = 2, data = c(1,s$x[i]))
    y <- s$y[i]
    s1 <- w1%*%x
    a1 <- c(1,sigmoid(s1))
    s2 <- w2%*%a1
    a2 <- c(1,sigmoid(s2))
    y_est <- v%*%a2
    #backprop l2
    err2 <- y_est-y
    d2 <- sigmoid_output_to_derivative(sigmoid(s2))
    delt2 <- d2 * (err2*v[1,2:4])
    v <- v - alp * a2*err2    
    w2 <- w2 - t(alp * (a1%*%t(delt2)))
    err1 <- s2 - w2%*%a1 # calculate error by new weights
    d1 <- sigmoid_output_to_derivative(sigmoid(s1))
    v_ <-  t(err1)%*%w2
    delt1 <- d1 * v_[1,2:7] 
    w1 <- w1 - t(alp * (x%*%t(delt1)))
  }
  
  
}
s$t<-unlist(lapply(s$x,test,w1,w2,v))
plot_ly(data = s,x=~x,y=~y, type = 'scatter') %>% add_trace(y=~t, type = 'scatter', mode = 'lines')

test <- function(x,w1,w2,v) {
  x <- matrix(nrow = 2, data = c(1,x))
  s1 <- w1%*%x
  a1 <- c(1,sigmoid(s1))
  s2 <- w2%*%a1
  a2 <- c(1,sigmoid(s2))
  y_est <- v%*%a2
}



