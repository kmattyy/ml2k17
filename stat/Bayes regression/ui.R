library(shiny)
library(plotly)

shinyUI(fluidPage(
 
  titlePanel("Bayes regression"),

  sidebarLayout(
    sidebarPanel(
        numericInput("seed",value=112, label = "Generator seed"),
        numericInput("nsamples",value= 40, label = "Number of samples"),
        numericInput("noiseval",value = 0.1, label = "Noise"),
        numericInput("grade",value = 3, label = "Polinom grade"),
        numericInput("alp",value = 2.0, label = "Alpha"),
        numericInput("bet",value = 25.0, label = "Beta"),
        numericInput("fn",value = 30, label = "Number of polinoms")
    ),
    
    mainPanel(
       plotlyOutput("chartplot")
    )
  )
))
