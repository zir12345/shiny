library(shiny)
library(ggplot2)  # for the diamonds dataset
library(readxl)
NAF1_2018_BI_1 <- read_excel("Merge/NAF1_2018_BI_1.xlsx")
NAF1_2018_BI_2 <- read_excel("Merge/NAF1_2018_BI_2.xlsx")

fluidPage(
  title = 'Examples of DataTables',
  sidebarLayout(
      conditionalPanel(
          'input.dataset === "NAF1_2018_BI_1"'
        ),
        conditionalPanel(
          'input.dataset === "NAF1_2018_BI_2"'
        )
      ),
  sidebarPanel(
    radioButtons("dist", "Distribution type:",
                 c("Normal" = "norm",
                   "Uniform" = "unif",
                   "Log-normal" = "lnorm",
                   "Exponential" = "exp")),
    br(),
    
    sliderInput("n", 
                "Number of observations:", 
                value = 500,
                min = 1, 
                max = 1000)
  ),
  mainPanel(
      tabsetPanel(type = "tabs",
        id = 'dataset',
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel('NAF1_2018_BI_1', DT::dataTableOutput('mytable1')),
        tabPanel('NAF1_2018_BI_2', DT::dataTableOutput('mytable2'))
      )
    )
)
