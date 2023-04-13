#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Load data from Github
data_url <- "https://raw.githubusercontent.com/willjones11/MSDS6396_Case_Study2/main/CaseStudy2-data.csv"
df <- read_csv(data_url)

ui <- fluidPage(
  # Title of app
  titlePanel("Talent Mangement Case Study"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      # Select plot type
      selectInput("plot_type", "Select plot type:",
                  c("Scatter plot" = "scatter", 
                    "Histogram" = "histogram",
                    "Box plot" = "boxplot")),
      
      # Select variable for x-axis
      uiOutput("x_var"),
      
      # Select variable for y-axis (scatter plot  and box plot only)
      uiOutput("y_var")
    ),
    
    # Main panel for displaying plots
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Selection of data for xvar
  x_var <- reactive({
    df %>% select(Age, 	Attrition, 	BusinessTravel,	DailyRate,	Department,
                  DistanceFromHome,	Education,	EducationField,	EmployeeCount,
                  EmployeeNumber,	EnvironmentSatisfaction,	Gender,	HourlyRate,
                  JobInvolvement,	JobLevel,	JobRole,	JobSatisfaction,	MaritalStatus,
                  MonthlyIncome,	MonthlyRate,	NumCompaniesWorked,	Over18,	OverTime,
                  PercentSalaryHike,	PerformanceRating,	RelationshipSatisfaction,	StandardHours,
                  StockOptionLevel,	TotalWorkingYears,	TrainingTimesLastYear,	WorkLifeBalance,
                  YearsAtCompany,	YearsInCurrentRole,	YearsSinceLastPromotion,YearsWithCurrManager
                  )
  })
  
  # selection of data for y_var
  y_var <- reactive({
    df %>% select(Age, 	Attrition, 	BusinessTravel,	DailyRate,	Department,
                  DistanceFromHome,	Education,	EducationField,	EmployeeCount,
                  EmployeeNumber,	EnvironmentSatisfaction,	Gender,	HourlyRate,
                  JobInvolvement,	JobLevel,	JobRole,	JobSatisfaction,	MaritalStatus,
                  MonthlyIncome,	MonthlyRate,	NumCompaniesWorked,	Over18,	OverTime,
                  PercentSalaryHike,	PerformanceRating,	RelationshipSatisfaction,	StandardHours,
                  StockOptionLevel,	TotalWorkingYears,	TrainingTimesLastYear,	WorkLifeBalance,
                  YearsAtCompany,	YearsInCurrentRole,	YearsSinceLastPromotion,YearsWithCurrManager
    )
  })
  
  # Create output for x-axis variable selection
  output$x_var <- renderUI({
    selectInput("x_var_select", "Select variable for x-axis:",
                choices = colnames(x_var()))
  })
  
  # Create output for y-axis variable selection (scatter plot and box plot only)
  output$y_var <- renderUI({
    if (input$plot_type != "histogram" ) {
      selectInput("y_var_select", "Select variable for y-axis:",
                  choices = colnames(y_var()))
    }
  })
  
  # Create output plot
  output$plot <- renderPlot({
    if (input$plot_type == "scatter") {
      # Scatter plot
      ggplot(df, aes_string(x = input$x_var_select, y = input$y_var_select)) + 
        geom_point()
    } else if (input$plot_type == "histogram") {
      # Histogram
      ggplot(df, aes_string(x = input$x_var_select)) + 
        geom_histogram()
    } else {
      # Box plot
      ggplot(df, aes_string(x = input$x_var_select, y = input$y_var_select)) + 
        geom_boxplot()
    }
  })
}


shinyApp(ui, server)