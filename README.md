# Shiny App Code Base for Height Comparison (Simplified and Blocked)

# ---------- BLOCK 1: Packages ----------
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# ---------- BLOCK 2: UI ----------
ui <- fluidPage(
  titlePanel("Height Comparison by Region"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload CSV'),
      uiOutput('col_select_ui'),
      numericInput('alpha', 'Significance level', 0.05, 0.001, 0.2, 0.005),
      selectInput('test_type', 'Test', c('ANOVA'='anova','Kruskal-Wallis'='kruskal'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Data', DTOutput('table')),
        tabPanel('CIs', DTOutput('cis_table')),
        tabPanel('Test', verbatimTextOutput('test_out')), 
        tabPanel('Plots', plotlyOutput('box_plot'))
      )
    )
  )
)

# ---------- BLOCK 3: Helper Function for CI ----------
group_cis <- function(df, height_col, group_col, conf=0.95){
  df %>% group_by(!!sym(group_col)) %>% summarise(
    n=n(), mean=mean(!!sym(height_col)), sd=sd(!!sym(height_col))
  ) %>% mutate(
    se=sd/sqrt(n), tcrit=qt((1+conf)/2, df=n-1),
    lower=mean-tcrit*se, upper=mean+tcrit*se
  )
}

# ---------- BLOCK 4: Server ----------
server <- function(input, output){
  raw_data <- reactive({
    if(is.null(input$file1)){
      data.frame(Height=c(rnorm(50,170,7),rnorm(60,167,6),rnorm(45,172,6)),
                 Region=rep(c('North','South','East'),c(50,60,45)))
    } else {
      read.csv(input$file1$datapath)
    }
  })

  output$col_select_ui <- renderUI({
    df <- raw_data()
    selectInput('height_col','Height column',names(df),names(df)[1])
  })

  data_clean <- reactive({
    df <- raw_data()
    df$Height <- as.numeric(df[[input$height_col]])
    df$Region <- as.factor(df[[2]])
    df
  })

  output$table <- renderDT({datatable(data_clean())})
  output$cis_table <- renderDT({datatable(group_cis(data_clean(),'Height','Region',1-input$alpha))})
  output$test_out <- renderPrint({
    dat <- data_clean()
    if(input$test_type=='anova') print(summary(aov(Height~Region,data=dat)) )
    else print(kruskal.test(Height~Region,data=dat))
  })
  output$box_plot <- renderPlotly({
    dat <- data_clean()
    ggplotly(ggplot(dat,aes(x=Region,y=Height))+geom_boxplot())
  })
}

# ---------- BLOCK 5: Run App ----------
shinyApp(ui, server)

# ---------- DESCRIPTION ----------
# This Shiny app allows users to upload a CSV with 'Height' and 'Region' columns.
# Users can select a significance level and test type (ANOVA or Kruskal-Wallis).
# The app computes group confidence intervals, shows test results, and displays an interactive box plot.

# ---------- README DESCRIPTION ----------
# Height Comparison Shiny App
# 
# This is a simple interactive Shiny web application built in R to compare average heights across different regions.
# Features:
# - Upload your own CSV dataset with 'Height' and 'Region' columns.
# - Compute confidence intervals for each region.
# - Perform ANOVA or Kruskal-Wallis tests.
# - Interactive box plots for visualization.
# 
# Usage:
# 1. Clone or download the repository.
# 2. Open 'app.R' in RStudio.
# 3. Install required packages: shiny, ggplot2, dplyr, DT, plotly.
# 4. Run the app using `runApp()` in RStudio.
# 
# Note: This code is simplified and structured in blocks for easy review and GitHub storage.
