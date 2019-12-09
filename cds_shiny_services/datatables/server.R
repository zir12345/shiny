library(shiny)
library(ggplot2)
NAF1_2018_BI_1 <- read_excel("Merge/NAF1_2018_BI_1.xlsx")
View(NAF1_2018_BI_1)
NAF1_2018_BI_2 <- read_excel("Merge/NAF1_2018_BI_2.xlsx")
View(NAF1_2018_BI_2)

function(input, output) {
  data <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  

  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })

    # sorted columns are colored now because CSS are attached to them
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(NAF1_2018_BI_1, options = list(orderClasses = TRUE))
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(NAF1_2018_BI_2, options = list(orderClasses = TRUE))
  })
  
  
}
