# Libraries 
library(tidyverse)
library(shiny)
library(bslib)
library(shinythemes)
library(DT)

# Data that will be used for shiny app

#subset of the df set that selects columns 3,5, etc...
player_info <- df[c(3,5,6,7,8,9,10)]

# UI Section of app:
ui <- fluidPage(
  titlePanel("Club Data Viewer", windowTitle = "Soccer Data Viewer"),
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      selectInput("club", "Select a Club:",
                  choices = unique(player_info$club),
                  width = "100%"
      ),
      
      textInput("club_text", "Enter Club Name:", value = ""),
      selectInput("view_option", "View Option:",
                  choices = c("Player Information", "Visual"),
                  width = "100%"),
      # Filter Options Section
      uiOutput("filter_options")
      
    ),
    mainPanel(
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Summary Statistics"),
            uiOutput("summary_stats")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          conditionalPanel(
            condition = "input.view_option == 'Player Information'",
            DT::dataTableOutput("club_table")
          ),
          conditionalPanel(
            condition = "input.view_option == 'Visual'",
            plotOutput("height_weight_plot")
          )
        )
      )
    )
  )
)

# Server Section of app
server <- function(input, output) {
  
  # Function to get data frame for selected club
  get_club_data <- reactive({
    club_data <- player_info
    
    # Filter data frame based on selected club
    if (!is.null(input$club_text) && input$club_text != "") {
      club_data <- club_data[club_data$club == input$club_text, ]
    } else if (!is.null(input$club)) {
      club_data <- club_data[club_data$club == input$club, ]
    }
    
    return(club_data)
  })
  
  # Filter Options section
  output$filter_options <- renderUI({
    conditionalPanel(
      condition = "input.view_option == 'Player Information'",
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Filter Options"),
            sliderInput("age_slider", "Age:",
                        min = 18, max = 80, value = c(18, 80),
                        width = "100%"
            ),
            sliderInput("height_slider", "Height (cm):",
                        min = 100, max = 220, value = c(100, 220),
                        width = "100%"
            ),
            sliderInput("weight_slider", "Weight (kg):",
                        min = 30, max = 200, value = c(30, 200),
                        width = "100%"
            )
          )
        )
      )
    )
  })
  
  # Player information table
  output$club_table <- DT::renderDataTable({
    filtered_data <- get_club_data()
    # Apply filters
    filtered_data <- filtered_data[
      filtered_data$age >= input$age_slider[1] & filtered_data$age <= input$age_slider[2] &
        filtered_data$height_cm >= input$height_slider[1] & filtered_data$height_cm <= input$height_slider[2] &
        filtered_data$weight_kg >= input$weight_slider[1] & filtered_data$weight_kg <= input$weight_slider[2],
    ]
    
    # DataTables options
    options <- list(
      pageLength = 10,  # Show 10 entries per page
      lengthMenu = c(5, 10, 15, 30, 50),  # Define options for "Show entries" dropdown
      searching = TRUE  # Enable search bar
    )
    DT::datatable(filtered_data, rownames = FALSE, options = options)
  })
  
  # Height vs. weight visualization for each club
  output$height_weight_plot <- renderPlot({
    club_data <- get_club_data()
    p <- ggplot(club_data, aes(x = weight_kg, y = height_cm)) +
      geom_point(color = "blue", size = 3) + 
      labs(x = "Weight in Kilograms", 
           y = "Height in Centimeters", 
           title = "Height vs. Weight by Club") +
      theme_bw()
    
    # geom_smooth is added if view option is Visual
    if (input$view_option == "Visual") {
      p <- p + geom_smooth(color = "black", se = FALSE, method = "lm")
    }
    
    return(p)
  })
  
  # Summary statistics based on selected club
  output$summary_stats <- renderText({
    club_data <- get_club_data()
    summary_stats <- c(
      paste("Total Players:", nrow(club_data)),
      paste("Average Age:", round(mean(club_data$age), 2)),
      paste("Average Height (cm):", round(mean(club_data$height_cm), 2)),
      paste("Average Weight (kg):", round(mean(club_data$weight_kg), 2))
    )
    paste(summary_stats, collapse = "<br>")
  })
}

# Run app
shinyApp(ui = ui, server = server)