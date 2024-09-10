library(shiny)

ui <- fluidPage(
  titlePanel("Pregnancy calculator"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("day_last_period", 
                "Date of the last period:",
                value = Sys.Date() - 280, 
                format = "yyyy-mm-dd"),
      
      dateInput("conception_date", 
                "Date of fertilization (if known):",
                value = NULL, 
                format = "yyyy-mm-dd"),
      
      actionButton("calculate", "Oblicz"),
      
      hr(),
      
      h4("Pregnancy data:"),
      verbatimTextOutput("due_date"),
      verbatimTextOutput("current_week"),
      verbatimTextOutput("days_left")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pregnancy progression: ",
                 h4("Child development:"),
                 textOutput("development_info"),
                 plotOutput("pregnancy_progress_plot")),
        
        tabPanel("Prenatal tests",
                 h4("Recommended tests:"),
                 verbatimTextOutput("prenatal_tests")),
        
        tabPanel("tips",
                 h4("Tips:"),
                 textOutput("health_tips")),
        
        tabPanel("Pregnancy history",
                 h4("Enter and view history:"),
                 textOutput("history_info"))
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    lmp_date <- input$last_menstrual_period
    conception_date <- input$conception_date
    
    if (is.null(conception_date)) {
      due_date <- lmp_date + 280
    } else {
      due_date <- conception_date + 266
    }
    
    current_date <- Sys.Date()
    days_left <- as.numeric(due_date - current_date)
    current_week <- as.numeric(difftime(current_date, lmp_date, units = "weeks"))
    
    output$due_date <- renderText({
      paste("Expected date of delivery:", due_date)
    })
    
    output$current_week <- renderText({
      paste("Current week of pregnancy:", current_week)
    })
    
    output$days_left <- renderText({
      paste("Days until the expected date of delivery:", days_left)
    })
    
    output$development_info <- renderText({
      paste("Baby development information for the week", current_week)
    })
    
    output$prenatal_tests <- renderText({
      paste("List of recommended prenatal tests for the week", current_week)
    })
    
    output$health_tips <- renderText({
      paste("Health tips for the week", current_week)
    })
    
    output$history_info <- renderText({
      paste("Enter and view pregnancy history")
    })
    
    output$pregnancy_progress_plot <- renderPlot({
      plot(seq(1, 40), rnorm(40), type = "l", xlab = "Week", ylab = "Development", main = "Pregnancy development")
    })
  })
}

shinyApp(ui = ui, server = server)
