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
      menuItem("Vital Charts", tabName = "Charts"),
      menuItem("Cross by", tabName = "Cross")
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
                  status = "primary",
                  solidHeader = TRUE,
                  #background = "light-blue",
                  "Complete observations:",
                  textOutput("drows")),
                
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
                
                box(#title = "Missing entries:", 
                  width = NULL, 
                  solidHeader = TRUE,
                  status = "primary", # line in box
                  #background = "light-blue",
                  "Complete observations:",
                  textOutput("lrows")),
                
                box(#title = "Missing Data", 
                    width = NULL, 
                    background = "maroon",
                    "Missing entries:",
                    textOutput("lnas")),
                
                box(#title = "Remove outliers?",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    #background = "light-blue",
                    "Remove ouliers?",
                    checkboxInput("lrmna",
                                  "Yes, please!",
                                  FALSE)),
                
                box(#title = "Missing Data", 
                  width = NULL, 
                  background = "maroon",
                  "Outliers removed:",
                  textOutput("loutliers"))
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
                
                box(#title = "Missing entries:", 
                  width = NULL, 
                  status = "primary",
                  solidHeader = TRUE,
                  #background = "light-blue",
                  "Complete observations:",
                  textOutput("crows")),
                
                box(#title = "Missing Data", 
                    width = NULL, 
                    background = "maroon",
                    "Missing entries:",
                    textOutput("cnas")),
                
                box(width = NULL,
                    solidHeader = TRUE,
                    status = "primary",
                    #background = "light-blue",
                    "Remove outliers?",
                    checkboxInput("crmna",
                                  "Yes, please!",
                                  FALSE)),
                
                box(#title = "Missing Data", 
                  width = NULL, 
                  background = "maroon",
                  "Outliers removed:",
                  textOutput("coutliers"))
                ), # Close column
                
                #Box for plotOutput
                box(title = "Variable Distribution",
                    width = 8,
                    plotOutput("chartplot"))
              ) # Close fluidRow
      ), # Close tabItem: Vitals
      
      
      ### EXPERIMENTAL
      
      # Fourth tab: Crossing variables
      tabItem(tabName = "Cross",
              fluidRow(
                # Create first column
                column(width = 4,
                       # Box for selectInput
                       box(#title = "LALALA",
                           solidHeader = TRUE,
                           status = "primary",
                           width = NULL,
                           #background = "light-blue",
                           selectInput(inputId = "var",
                                       label = "Select measure of interest:",
                                       choices = c("anchor_age",
                                                   "bicarbonate","calcium",
                                                   "chloride","creatinine",
                                                   "glucose","magnesium",
                                                   "potassium","sodium",
                                                   "hematocrit","wbc",
                                                   "lactate", "heart_rate",
                                                   "non_invasive_blood_pressure_systolic",
                                                   "non_invasive_blood_pressure_mean",
                                                   "respiratory_rate",
                                                   "temperature_fahrenheit"
                                       )) # Close selectInput
                       ), # Close box selectInput
                       
                       box(#title = "Input selection",
                           solidHeader = TRUE,
                           status = "primary",
                           width = NULL,
                           #background = "light-blue",
                           selectInput(inputId = "gvar",
                                       label = "Select grouping variable:",
                                       choices = c("gender",
                                                   "insurance",
                                                   "language",
                                                   "ethnicity"
                                       ))), # Close selectInput
                       
                       box(width = NULL,
                           solidHeader = TRUE,
                           status = "primary",
                           #background = "light-blue",
                           "Remove outliers?",
                           checkboxInput("rmna",
                                         "Yes, please!",
                                         FALSE))
                       
                ), # Close column
                
                #Box for plotOutput
                box(title = "Distribution by Group",
                    width = 8,
                    plotOutput("crossplot"))
              ) # Close fluidRow
      ) # Close tabItem: Cross
      
      #### EXPERIMENTAL
      
      
      
      
      
    ) # Close tabItems
    
  ) # Close dashboardBody
) # Close dashboardPage

# Code generating the output plot
server <- function(input, output) {
  
  # Load the dataset
  path <- "/Users/pdgeraldo/Documents/GitHub/biostat-203b-2021-winter/hw3/mimiciv_shiny"
  mimic <- readRDS(paste0(path,"/icu_cohort.rds"))
  
  ## ---- DEMOGRAPHICS ---- ##
  # Reactive data for Demographics
  
  # observations and NA count
  output$dnas <- reactive({
    nrow(mimic[is.na(mimic[[input$dvar]]),])
  })
  output$drows <- reactive({
    nrow(mimic[!is.na(mimic[[input$dvar]]),])
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
  
  # Observations and NA count
  output$lnas <- reactive({
    nrow(mimic[is.na(mimic[[input$lvar]]),])
  })
  output$lrows <- reactive({
    nrow(mimic[!is.na(mimic[[input$lvar]]),])
  })
  # Outliers count
  output$loutliers <- reactive({
    nrow(mimic[!is.na(mimic[[input$lvar]]),]) - nrow(ldata())
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
  
  # Observations and NA count
  output$cnas <- reactive({
    nrow(mimic[is.na(mimic[[input$cvar]]),])
  })
  output$crows <- reactive({
    nrow(mimic[!is.na(mimic[[input$cvar]]),])
  })
  # Outliers count
  output$coutliers <- reactive({
    nrow(mimic[!is.na(mimic[[input$cvar]]),]) - nrow(cdata())
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
  
  
  ## --- CROSS BY --- ###
  # Data for plotting
  gdata <- reactive({
    
    if(input$rmna==TRUE){
      
      m <- mimic %>%
        tidyr::drop_na(!!!input$var) %>%
        dplyr::select(!!!input$var,
                      !!!input$gvar)
      m$sd <- sd(m[[input$var]])
      m$center <- median(m[[input$var]])
      m <- m[m[[input$var]] < m$center+3*m$sd, ]
      m <- m[m[[input$var]] > m$center-3*m$sd, ]
      return(m)
      
    }else{
      
      mimic %>%
        tidyr::drop_na(!!!input$var) %>%
        dplyr::select(!!!input$var,
                      !!!input$gvar)
    }
  })
  
  # Creating crossplot
  output$crossplot <- renderPlot(
    ggplot(data = gdata(), 
           aes(y = gdata()[[input$var]],
               x = gdata()[[input$gvar]],
               fill = gdata()[[input$gvar]])) +
      geom_boxplot(aes(fill = gdata()[[input$gvar]])) +
      theme_bw() +
      labs(y = input$var) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.title = element_blank())
  )
  
    
} # End server

# Run the application 
shinyApp(ui = ui, server = server)
