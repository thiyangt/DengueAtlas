# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(ISOweek)
library(denguedatahub)

# Prepare the data: convert year + week to week_start
srilanka_weekly_data <- srilanka_weekly_data %>%
  mutate(
    year_week = paste0(year, "-W", sprintf("%02d", week)),
    week_start = ISOweek2date(paste0(year_week, "-1"))  # Monday of the week
  )

# Get unique district names for dropdown
district_choices <- unique(srilanka_weekly_data$district)

# Shiny UI
ui <- fluidPage(
  titlePanel("Weekly Dengue Cases by District in Sri Lanka"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "district",
        label = "Select a District:",
        choices = district_choices,
        selected = "Colombo"
      )
    ),
    
    mainPanel(
      plotOutput("denguePlot", height = "600px")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  output$denguePlot <- renderPlot({
    
    # Filter data for selected district
    district_data <- srilanka_weekly_data %>%
      filter(district == input$district)
    
    # Plot
    ggplot(district_data, aes(x = week_start, y = cases)) +
      geom_line(color = "darkred", linewidth = 1) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title = paste("Weekly Dengue Cases in", input$district),
        x = "Year",
        y = "Weekly Cases"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
}

# Run the app
shinyApp(ui, server)
