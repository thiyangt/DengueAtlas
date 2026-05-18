library(shiny)
library(ggplot2)
library(dplyr)
library(ISOweek)
library(denguedatahub)
library(plotly)

# Data prep
data(srilanka_weekly_data)

srilanka_weekly_data <- srilanka_weekly_data %>%
  mutate(
    year_week = paste0(year, "-W", sprintf("%02d", week)),
    week_start = ISOweek2date(paste0(year_week, "-1"))
  )

district_choices <- unique(srilanka_weekly_data$district)

ui <- fluidPage(
  titlePanel("Weekly Dengue Cases by District in Sri Lanka"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "district",
        "Select District:",
        choices = district_choices,
        selected = "Colombo"
      )
    ),
    
    mainPanel(
      plotlyOutput("denguePlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$denguePlot <- renderPlotly({
    
    district_data <- srilanka_weekly_data %>%
      filter(district == input$district) %>%
      arrange(week_start)
    
    p <- ggplot(district_data, aes(
      x = week_start,
      y = cases,
      group = 1,
      text = paste0(
        "Week: ", week_start,
        "<br>Cases: ", cases
      )
    )) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_point(alpha=0.5, size=1) +
      labs(
        title = paste("Weekly Dengue Cases in", input$district),
        x = "Year",
        y = "Cases"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)