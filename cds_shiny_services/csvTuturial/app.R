# Packages
library(dplyr)
library(ggplot2)

# Data
species <- read.csv('data/species.csv',
		stringsAsFactors = FALSE)
animals <- read.csv('data/animals.csv',
		na.strings = '',
		stringsAsFactors = FALSE)

# User Interface
in1 <- selectInput(
		inputId = 'pick_species',
		label = 'Pick a species',
		choices = unique(species[['id']]))
in2 <- sliderInput(
		inputId = 'slider_months',
		label = 'Month Range',
		min = 1, max = 12,
		value = c(1, 12))
side <- sidebarPanel('Options', in1, in2)									    
out1 <- textOutput('species_label')
out2 <- tabPanel(
		title = 'Plot',
		plotOutput('species_plot'))
out3 <- tabPanel(
		title = 'Data',
		dataTableOutput('species_table'))                 
main <- mainPanel(out1, tabsetPanel(out2, out3))
tab1 <- tabPanel(
		title = 'Species',
		sidebarLayout(side, main))
ui <- navbarPage(
		title = 'Portal Project',
		tab1)

# Server
server <- function(input, output) {
	selected_animals <- reactive({
				animals %>%
						filter(species_id == input[['pick_species']]) %>%
						filter(
								month >= input[['slider_months']][1],
								month <= input[['slider_months']][2])
			})
	output[['species_label']] <- renderText({
				species %>%
						filter(id == input[['pick_species']]) %>%
						select(genus, species) %>%
						paste(collapse = ' ')
			})
	output[['species_plot']] <- renderPlot({
				ggplot(selected_animals(), aes(year)) +
						geom_bar()
			})
	output[['species_table']] <- renderDataTable({
				selected_animals()
			})
}

# Create the Shiny App
shinyApp(ui = ui, server = server)