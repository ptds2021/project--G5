#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    anime <- anime

    table_newcommer_temp <- reactive({
        newcommer_recom(anime, input$age, input$gender, input$freetime)
    })
    output$table_newcommer <- DT::renderDT(server = FALSE, {
        datatable(table_newcommer_temp())
    })

})






