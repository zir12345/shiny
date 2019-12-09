library(shiny)

fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    mainPanel(
      tableOutput('contents')
    )
  ),

tabPanel("First Type",
		pageWithSidebar(
				headerPanel('My First Plot'),
				sidebarPanel(
						
						# "Empty inputs" - they will be updated after the data is uploaded
						selectInput('xcol', 'X Variable', ""),
						selectInput('ycol', 'Y Variable', "", selected = "")
				
				),
				mainPanel(
						plotOutput('MyPlot')
				)
		)
)

)
