library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Stock Data Analysis - Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
     
      helpText("Please select a symbol."),
      
      textInput("symb", "Enter Valid Stock Symbol:", value = "", width = NULL, placeholder = NULL),
  
      sliderInput("integer", "Last Months Data:", 
                min=1, max=100, value=1),
      numericInput("obs", "Number of Data to Show:", 5)
    ),
    
    mainPanel(textOutput("text1"),textOutput("text2"),plotOutput("plot"), tableOutput("view")))
  )
)
