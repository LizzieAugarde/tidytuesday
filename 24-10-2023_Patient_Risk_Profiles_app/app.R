#
# Shiny app to visualise mean risk of each condition for patients with user-selected characteristics
#

library(shiny)

ui <- fluidPage(

    titlePanel("Mean risk of conditions in patient groups"),

    sidebarLayout(
        sidebarPanel(
            selectInput("selected_sex",
                        "Select patient sex:",
                        choices = unique(mean_risk_by_group$sex)),
            selectInput("selected_age",
                        "Select patient age group:",
                        choices = unique(mean_risk_by_group$age_group)),
            selectInput("selected_prior_cond",
                        "Select patient prior condition:",
                        choices = unique(mean_risk_by_group$prior_condition))
        ),
    mainPanel(
      textOutput("table_desc")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table_desc <- renderText({
      paste("Mean risk of conditions in patients who are ", input$selected_sex, ", ", input$selected_age, 
            ", and experienced ", input$selected_prior_cond, "in the last year")
    })
    
    output$age_result <- renderText({
      input$selected_age
    })
    
    output$prior_cond_result <- renderText({
      input$selected_prior_cond
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
