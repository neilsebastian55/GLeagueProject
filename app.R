# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load your dataset
GLeagueStats <- read.csv("GLeagueStats.csv")

# Define UI for the app
ui <- fluidPage(
  titlePanel("NBA G League Player Stats Analysis"),
  tags$a("Image is courtesy of NCAA (https://www.ncaa.org/) ", href = "https://www.ncaa.org/news/2023/5/5/media-center-change-to-legal-guarding-position-recommended-for-mens-basketball.aspx"),
  
  sidebarLayout(
    sidebarPanel(
      # Instruction text
      p(strong("Instructions")),
      p("Click a variable to get the distribution, you can use the slider to filter by minutes played!"),
      p("Click the median line button to add a line to the distributions where the median is, press again to remove!"),
      
      # Select variables for analysis using radio buttons
      radioButtons("variables", "Select Variable:",
                   choices = c(setdiff(colnames(GLeagueStats), c("X", "Name")), "ScoreCategory"),
                   selected = "PPG"),  # Default selection is PPG
      
      # Add a slider for Minutes
      sliderInput("mins_slider", "Filter by Minutes:",
                  min = min(GLeagueStats$Mins, na.rm = TRUE),
                  max = max(GLeagueStats$Mins, na.rm = TRUE),
                  value = c(min(GLeagueStats$Mins, na.rm = TRUE), max(GLeagueStats$Mins, na.rm = TRUE)),
                  step = 1),
      
      # Dropdown meun for the graph color
      selectInput("graph_color", "Select Graph Color:",
                  choices = c("skyblue", "forestgreen", "lavender"),
                  selected = "skyblue"),
      
      # Action button for fun fact
      actionButton("action_btn", "Median Line")
    ),
    
    mainPanel(
      # Display variable descriptions
      img(src='https://www.ncaa.org/images/2023/3/30/MBB-WBB_BallHoop.JPG?width=1884&quality=80&format=jpg', align = "right", width = 500, height = 300),
      helpText(strong("Variable Key:")),
      helpText("- ", strong("PPG:"), " Points per game scored by the player."),
      helpText("- ", strong("GP:"), " Number of games played by the player."),
      helpText("- ", strong("Mins:"), " Average minutes played by the player."),
      helpText("- ", strong("FGA:"), " Field goal attempts per game by the player."),
      helpText("- ", strong("FGM:"), " Field goals made per game by the player."),
      helpText("- ", strong("FG%:"), " Field goal percentage (FG%) of the player."),
      helpText("- ", strong("3PA:"), " Three-point attempts per game by the player."),
      helpText("- ", strong("3PM:"), " Three-pointers made per game by the player."),
      helpText("- ", strong("3P%:"), " Three-point percentage (3P%) of the player."),
      helpText("- ", strong("FTA:"), " Free throw attempts per game by the player."),
      helpText("- ", strong("FTM:"), " Free throws made per game by the player."),
      helpText("- ", strong("FT%:"), " Free throw percentage (FT%) of the player."),
      helpText("- ", strong("PPM:"), " Points per minute scored by the player."),
      helpText("- ", strong("PF3PG:"), " Points from 3's per game by the player."),
      helpText("- ", strong("PerPF3:"), " Percentage of total points from 3-pointers by the player."),
      helpText("- ", strong("ScoreCategory:"), " High or Low score category based on PPG, over 20ppg or under. (Categorical)"),
      
      
      # Output descriptive statistics for selected variables
      verbatimTextOutput("desc_stats"),
      
      # Output plots for selected variables
      plotOutput("variable_plots"),
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive function for filtered data
  filtered_data <- reactive({
    GLeagueStats %>%
      filter(Mins >= input$mins_slider[1], Mins <= input$mins_slider[2]) %>%
      mutate(ScoreCategory = ifelse(PPG > 20, "High", "Low"))
  })
  
  # Store button click status
  btn_click <- reactiveVal(FALSE)
  
  # Output descriptive statistics for selected variables
  output$desc_stats <- renderPrint({
    lapply(input$variables, function(variable) {
      paste("Summary for", variable, ":")
      summary(filtered_data()[, variable])
    })
  })
  
  # Output plots for selected variables
  output$variable_plots <- renderPlot({
    par(mfrow = c(ceiling(length(input$variables)/2), 2))
    
    # Loop through variables
    for (variable in input$variables) {
      if (variable == "ScoreCategory") {
        # Plot for the created categorical variable
        barplot(table(filtered_data()[, variable]), main = paste("Distribution of", variable),
                xlab = variable, col = input$graph_color)
      } else {
        # Histogram for numeric variable
        hist(filtered_data()[, variable], main = paste("Distribution of", variable),
             xlab = variable, col = input$graph_color)
      }
    }
    
    # Add a vertical line at the median when the action button is pressed
    if (btn_click()) {
      variable <- input$variables[1]  # Choose the first variable for simplicity
      if (variable == "ScoreCategory") {
        abline(v = median(as.numeric(filtered_data()[, variable])), col = 'red', lwd = 2, lty = 'dashed')
      } else {
        abline(v = median(filtered_data()[, variable], na.rm = TRUE), col = 'red', lwd = 2, lty = 'dashed')
      }
    }
    
    par(mfrow = c(1, 1))
  })
  
  # Observe button click and update reactiveVal
  observeEvent(input$action_btn, {
    btn_click(!btn_click())
  })
}

# Run the app
shinyApp(ui, server)
