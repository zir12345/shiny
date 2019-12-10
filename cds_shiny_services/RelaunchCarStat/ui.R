library(shiny)
library(rsconnect)
library(ggplot2)
library(plotly)

plotType <- function(data, x, y, type){
  switch(type,
         "Line" = ggplot(data, aes_string(x, y)) + geom_line(),
         "Scatterplot" = ggplot(data, aes_string(x, y)) + geom_point()
  )
}

ui <- fluidPage(  
		
		sidebarPanel(
				# Input: select a file
				fileInput(inputId = "file1", label = "Choose CSV File",
						multiple = FALSE,
						accept = c("text/csv",
								"text/comma-separated-values, text/plain",
								".csv")
				),
				# Horizontal line
				tags$hr(),
				# Input: Checkbox if file has header
				checkboxInput("header", "Header", TRUE),
				# Input: Select separator
				radioButtons(inputId ="sep", label = "Separator",
						choices = c(Comma = ",",
								Semicolon = ";",
								Tab = "\t"),
						selected = ";"),
				
				radioButtons(inputId = "quote", label = "Quote",
						choices = c(None = "",
								"Double Quote" = '"',
								"Single Quote" = "'"),
						selected = '"'),
				# Horizontal line
				tags$hr(),
				
				selectInput('xcol', 'X Variable', ""),
				selectInput('ycol', 'Y Variable', "", selected = ""),
				# Horizontal line
				tags$hr(),
				# Input: Select the type of graph 
				radioButtons(inputId ="graph", label = "Type of graph:",
						choices = c("Line", 
								"Scatterplot"),
						selected = "Line")
		),
		
		mainPanel(
				tabsetPanel( type = "tabs",
						tabPanel(
								# App title
								titlePanel("Uploading Files"),
								# Output: Data file
								tableOutput("contents")
						
						),
						tabPanel(
								titlePanel("Plot"),
								plotOutput('MyPlot')
						),
						tabPanel(
								titlePanel("Summary Statistics"),
								verbatimTextOutput("summary")
						)
				)
		)
)
