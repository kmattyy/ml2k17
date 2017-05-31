
library(shiny)
library(plotly)
library(mvtnorm)
grade <- 3
noise <- 0.1
nsamples <- 40
alp <- 2.0
bet <- 40
pol_num <- 30

gety <- function(x,params) {
  y <- params[1]
  for (k in 2:length(params)){
    y <- y + params[k] * x ^ (k-1)
  }
    y
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
   
  output$chartplot <- renderPlotly({
    set.seed(input$seed)
    nsamples <- input$nsamples
    noise <- input$noiseval
    alp <- input$alp
    bet <- input$bet
    pol_num <- input$fn
    grade <- input$grade
    nums <- runif(nsamples,min=-pi,max=pi)
    sins <- sin(nums) 
    sin_noisy <- sins + rnorm(nsamples,mean = 0, sd=noise)
    #beta <- 
    phi <- matrix(rep(1,times=nsamples))
    for(pow in 1:grade){
      phi<- cbind(phi, unlist(lapply(sin_noisy, '^', pow)))
    }
    I <- diag(grade+1)
    sigm_inv <- I * alp +  bet * t(phi) %*% phi 
    sigm <- solve(sigm_inv)
    mu <- bet *  ((sigm %*% t(phi)) %*% sin_noisy)
    p <- plot_ly()
    x <- seq(from= -pi, to = pi,length.out = 100)
    for(i in 1:pol_num){
      params <- rmvnorm(n=1,sigma=sigm,mean=mu)
      y <- lapply(x,gety,params)
      p <- p %>% add_lines(x = x, y=y )
    }
    
    #plot_ly(y=sins,x=nums, color = I("red"))%>% add_lines() %>%  add_trace(x=nums, y=sin_noisy, color="green") 
    p %>% add_trace(y=sins,x=nums, color = I("red"))%>% add_trace(y=sin(x),x=x, color = I("red"))  %>%  add_trace(x=nums, y=sin_noisy, color=I("green"))
  })
})

