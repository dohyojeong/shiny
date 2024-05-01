#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(rpart.plot)
library(dplyr)

# Data load
school_data <- read.table("https://raw.githubusercontent.com/dohyojeong/dohyocode/main/high_school_raw.txt", 
                          header = TRUE, sep = "\t")

# UI 구성
ui <- fluidPage(
  titlePanel("Comparison of Test Scores by High School Characteristics"),
  
  # 탭 패널 생성
  tabsetPanel(
    tabPanel("Plot 1", uiOutput("map_ui"), leafletOutput("map_plot", width = "900px", height = "700px")),
    tabPanel("Plot 2", uiOutput("histogram_ui"), plotOutput("histogram_plot", width = "900px", height = "600px")),
    tabPanel("Plot 3", uiOutput("decision_tree_ui"), plotOutput("decision_tree_plot", width = "900px", height = "600px"))
  ),
  # CSS 추가
  tags$head(
    tags$style(
      HTML("
      .shiny-tab-content {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      ")
    )
  )
)

# 서버 구성
server <- function(input, output, session) {
  # Plot 1 - Leaflet 지도 UI
  output$map_ui <- renderUI({
    tagList(
      selectInput("school_select", "School Type:", choices = c("All", "Private", "Public"))
    )
  })
  
  # Plot 1 - Leaflet 지도
  output$map_plot <- renderLeaflet({
    filtered_data <- school_data %>%
      filter(if (input$school_select == "All") TRUE
             else if (input$school_select == "Private") School == "Private"
             else School == "Public")
    
    map <- leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(data = filtered_data, 
                       lng = ~longitude, lat = ~latitude,
                       color = "black", # 테두리 색상 추가
                       radius = 4,      # 포인트 크기 줄임
                       stroke = FALSE,   # 테두리 추가
                       fillOpacity = 0.5,
                       fillColor = ifelse(filtered_data$School == "Private", "red", "blue"))
    map
  })
  
  # Plot 2 - 히스토그램 UI
  output$histogram_ui <- renderUI({
    tagList(
      selectInput("covid_select", "COVID-19:", choices = unique(school_data$COVID.19)),
      selectInput("subject_select", "Subjects:", choices = unique(school_data$Subjects)),
      sliderInput("bins_select", "Bins:", min = 1, max = 50, value = 20)
    )
  })
  
  # Plot 2 - 히스토그램
  output$histogram_plot <- renderPlot({
    filtered_data <- school_data %>%
      filter(COVID.19 == input$covid_select, Subjects == input$subject_select)  
    
    ggplot(filtered_data, aes(x = Score, fill = factor(School))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = input$bins_select) +
      scale_fill_manual(values = c("red", "blue")) +  
      labs(title = paste("Histogram of", input$subject_select, "Scores"),
           x = "Average Score", fill = "School") +
      theme_minimal()
  })
  
  # Plot 3 - 의사결정트리 UI
  output$decision_tree_ui <- renderUI({
    tagList(
      selectInput("covid_select_2", "COVID-19:", choices = unique(school_data$COVID.19)),
      selectInput("subject_select_2", "Subjects:", choices = unique(school_data$Subjects)),
      selectInput("school_select_2", "School Type:", choices = c("All", "Private", "Public")),
      sliderInput("cp_select", "Complexity Parameter (CP):", min = 0.001, max = 0.05, value = 0.01, step = 0.001)
    )
  })
  
  # Plot 3 - 의사결정트리
  output$decision_tree_plot <- renderPlot({
    filtered_data <- school_data %>%
      filter(COVID.19 == input$covid_select_2, Subjects == input$subject_select_2,
             if (input$school_select_2 == "All") TRUE
             else if (input$school_select_2 == "Private") School == "Private"
             else School == "Public")
    
    tree_model <- rpart(Score ~ pupil_teacher_ratio + dropout_rate + asp_enrollment +
                          settlement_revenue +  settlement_expense +
                          ppexp + house_price + library_per + divorce + priv_academy,
                        data = filtered_data, cp = input$cp_select)
    
    rpart.plot(tree_model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE,
               main = "Decision Tree")
  })
}

# Shiny 애플리케이션 실행
shinyApp(ui = ui, server = server)
