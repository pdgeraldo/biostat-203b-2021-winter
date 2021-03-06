# Pablo Geraldo

# Shiny app to explore MIMIV-IV data
# Specially focused on lab and chart values

library(shiny) # Basic shiny functionality
library(shinydashboard) # Additional functions and layouts
library(tidyverse) # For data manipulation inside the app


# Describe the desired UI
# Here I'm using the shinydasboard::dashboardPage

ui <- dashboardPage(
  
  # Describe the header
  dashboardHeader(title="Exploring MIMIC-IV data"),
  
  # Describe the sidebar (inputs)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "Demog"),
      menuItem("Laboratory", tabName = "Labs"),
      menuItem("Vital Charts", tabName = "Charts")
    )
  ),
  
  # Describe the body (output placeholder)
  dashboardBody(
    # Declare tab items
    tabItems(
      
      # First tab: Demographics
      tabItem(tabName = "Demog",
              fluidRow(
                # Create first column
                column(width = 4,
                # Box for selectInput
                box(title = "Input selection",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    #background = "light-blue",
                    selectInput(inputId = "dvar",
                                label = "Select demographic variable:",
                                choices = c("gender",
                                            "anchor_age",
                                            "insurance",
                                            "language",
                                            "ethnicity"
                                )) # Close selectInput

                ), # Close box for selectInput
                
                box(#title = "Missing entries:", 
                    width = NULL, 
                    background = "maroon",
                    "Missing entries:",
                    textOutput("dnas"))
                
                ), # Close column
                
                #Box for plotOutput
                box(title = "Variable Distribution",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 8,
                  plotOutput("demplot")),
              ) # Close fluidRow
      ), # Close tabItem: Demographics
      
      # Second tab: Labs
      tabItem(tabName = "Labs",
              fluidRow(
                # Create first column
                column(width = 4,
                # Box for selectInput
                box(title = "Input selection",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    #background = "light-blue",
                    selectInput(inputId = "lvar",
                                label = "Select lab measure:",
                                choices = c("bicarbonate","calcium",
                                            "chloride","creatinine",
                                            "glucose","magnesium",
                                            "potassium","sodium",
                                            "hematocrit","wbc",
                                            "lactate")) # Close selectInput
                ), # Close box selectInput
                
                box(#title = "Missing Data", 
                    width = NULL, 
                    background = "maroon",
                    "Missing entries:",
                    textOutput("lnas")),
                
                box(title = "Remove outliers?",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    #background = "light-blue",
                    checkboxInput("lrmna",
                                  "Yes, please!",
                                  FALSE))
                ), # Close column
                
                #Box for plotOutput
                box(title = "Variable Distribution",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 8,
                    plotOutput("labplot"))
              ) # Close fluidRow
      ), # Close tabItem: Labs
      
      # Third tab: Vitals
      tabItem(tabName = "Charts",
              fluidRow(
                # Create first column
                column(width = 4,
                # Box for selectInput
                box(title = "Input selection",
                    solidHeader = TRUE,
                    status = "primary",
                    width = NULL,
                    #background = "light-blue",
                    selectInput(inputId = "cvar",
                                label = "Select vitals measure:",
                                choices = c("heart_rate",
                                            "non_invasive_blood_pressure_systolic",
                                            "non_invasive_blood_pressure_mean",
                                            "respiratory_rate",
                                            "temperature_fahrenheit"
                                            
                                )) # Close selectInput
                ), # Close box selectInput
                
                box(#title = "Missing Data", 
                    width = NULL, 
                    background = "maroon",
                    "Missing entries:",
                    textOutput("cnas")),
                
                box(width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    #background = "light-blue",
                    checkboxInput("crmna",
                                  "Remove outliers?",
                                  FALSE))
                ), # Close column
                
                #Box for plotOutput
                box(title = "Variable Distribution",
                    width = 8,
                    plotOutput("chartplot"))
              ) # Close fluidRow
      ) # Close tabItem: Vitals
      
    ) # Close tabItems
    
  ) # Close dashboardBody
) # Close dashboardPage

# Code generating the output plot
server <- function(input, output) {
  
  # Load the dataset
  path <- "/home/pdgeraldo/biostat-203b-2021-winter/hw3/mimiciv_shiny"
  mimic <- readRDS(paste0(path,"/icu_cohort.rds"))
  
  ## ---- DEMOGRAPHICS ---- ##
  # Reactive data for Demographics
  
  # NA count
  output$dnas <- reactive({
    nrow(mimic[is.na(mimic[[input$dvar]]),])
  })
  
  # Data for plotting
  ddata <- reactive({
    
      mimic %>%
        tidyr::drop_na(!!!input$dvar) %>%
        dplyr::select(!!!input$dvar)
  })
  
  # Creating demplot
  output$demplot <- renderPlot(
    
    if(input$dvar == "anchor_age"){
      ggplot(data = ddata(),
             aes(x = ddata()[[input$dvar]])) +
        geom_histogram() +
        theme_bw() +
        labs(x = input$dvar,
             y = "Counts")
    } else {
      ggplot(data = ddata(), 
             aes(x = ddata()[[input$dvar]], 
                 y = (..count..)/sum(..count..))) +
        geom_bar(aes(fill = ddata()[[input$dvar]])) +
        theme_bw() +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                           labels = scales::percent) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.title = element_blank()) +
        labs(x = input$dvar,
             y = "Percentage")
    }
    
  )
  
  ## ---- LABORATORY ---- ##
  # Reactive data for Labs
  
  # NA count
  output$lnas <- reactive({
    nrow(mimic[is.na(mimic[[input$lvar]]),])
  })
  # Data for plotting
  ldata <- reactive({
    
    if(input$lrmna==TRUE){
      
      m <- mimic %>%
        tidyr::drop_na(!!!input$lvar) %>%
        dplyr::select(!!!input$lvar)
      m$sd <- sd(m[[input$lvar]])
      m$center <- median(m[[input$lvar]])
      m <- m[m[[input$lvar]] < m$center+3*m$sd, ]
      m <- m[m[[input$lvar]] > m$center-3*m$sd, ]
      return(m)
      
    }else{
      
      mimic %>%
        tidyr::drop_na(!!!input$lvar) %>%
        dplyr::select(!!!input$lvar)
    }
  })
  
  # Creating labsplot
  output$labplot <- renderPlot(
    ggplot(data = ldata(), 
           aes(y = ldata()[[input$lvar]])) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(y = input$cvar)
  )
  
  
  ## ---- CHART EVENTS ---- ##
  # Reactive data for Charts
  
  # NA count
  output$cnas <- reactive({
    nrow(mimic[is.na(mimic[[input$cvar]]),])
  })
  
  # Data for plotting
  cdata <- reactive({
    
    if(input$crmna==TRUE){
    
      m <- mimic %>%
        tidyr::drop_na(!!!input$cvar) %>%
        dplyr::select(!!!input$cvar)
      m$sd <- sd(m[[input$cvar]])
      m$center <- median(m[[input$cvar]])
      m <- m[m[[input$cvar]] < m$center+3*m$sd, ]
      m <- m[m[[input$cvar]] > m$center-3*m$sd, ]
      return(m)
      
    }else{
      
      mimic %>%
        tidyr::drop_na(!!!input$cvar) %>%
        dplyr::select(!!!input$cvar)
    }
    
  })
  
  # Creating chartplot
  output$chartplot <- renderPlot(
    ggplot(data = cdata(), 
           aes(y = cdata()[[input$cvar]])) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(y = input$cvar)
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
