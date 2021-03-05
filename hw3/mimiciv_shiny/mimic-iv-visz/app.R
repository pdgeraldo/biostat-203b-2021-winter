# Pablo Geraldo

# Shiny app to explore MIMIV-IV data
# Specially focused on lab and chart values

library(shiny) # Basic shiny functionality
library(shinydashboard) # Additional functions and layouts
library(tidyverse) # For data manipulation inside the app

# Load the dataset
path <- "/home/pdgeraldo/biostat-203b-2021-winter/hw3/mimiciv_shiny"
mimic <- readRDS(paste0(path,"/icu_cohort.rds"))

# Describe the desired UI
# Here I'm using the shinydasboard::dashboardPage

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
# Code generating the output plot
server <- function(input, output){

    output$labsPlot <- renderPlot({
      
      mimic %>%
        dplyr::select(!!input$var) %>%
        dplyr::filter(!is.na(!!input$var)) %>%
        ggplot(aes(x = !!input$var)) +
        geom_histogram() +
        facet_wrap(. ~!!input$group) +
        theme_minimal()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
