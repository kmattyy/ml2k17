library(plotly)

points <- data.frame(x1=c(-1,-1,1,1),x2=c(-1,1,-1,1),y=c(-1,1,1,-1))
points


fwx <- function(x,w){
  print(x)
  print(w)
  x[1] * w[1] + x[2]*w[2]
}

err <- function(w,p){

  p$fw <- apply(p[,c('x1','x2')],1,function(y) fwx(y,w) )  
}

errw <- function(w1,w2){
  w1^2+w2^2+1
}

w <- c(0,0)
p <- err

w1 <- seq(from=-2,to=2,by=0.05)
w2 <- seq(from=-2,to=2,by=0.05)
grid <- expand.grid(x=w1, y=w2)
grid$z <- with(grid,errw(x,y))

plot_ly(grid, x=~x, y= ~ y,z= ~ z, type='mesh3d',intensity = ~z,colors = colorRamp(rainbow(8)))
