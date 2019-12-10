library(shiny)
library(ggplot2)
library(plotly)


plotType <- function(data, x, y, type){
  switch(type,
         "Line" = ggplot(data, aes_string(x, y)) + geom_line(),
         "Scatterplot" = ggplot(data, aes_string(x, y)) + geom_point()
  )
}
function(input, output, session) {
	
	data <- reactive({
				req(input$file1) 
				
				df <- read.csv(input$file1$datapath,
						header = input$header,
						sep = input$sep,
						quote = input$quote)
				
				updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
						choices = names(df), selected = names(df)[sapply(df, is.numeric)])
				updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
						choices = names(df), selected = names(df)[sapply(df, is.numeric)])
				
				return(df)
				
			})
	
	x_axe <- reactive({
				data()[ , input$xcol]
			})
	
	y_axe <- reactive({
				data()[, input$ycol]
			})
	
	style <- reactive({
				input$graph
			})
	
	output$contents <- renderTable({
				data()
			})
	
	output$MyPlot <- renderPlotly({
				#x <- data()[, c(input$xcol, input$ycol)]
				p <- plotType(data(), x_axe(),
						y_axe(),
						style())
				p
				
			})
	
	# Generate a summary table of the data uploaded
	output$summary <- renderPrint({
				y <- data()
				summary(y)
				
			})
	
}
