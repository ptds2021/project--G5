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
shinyServer(function(input, output, session) {
    anime <- anime


    updateSelectizeInput(session, 'viewed_1', choices = anime$Name, server = TRUE)

    table_newcommer_temp <- reactive({
        newcommer_recom(anime, input$age, input$gender, input$freetime)
    })

    output$table_newcommer <- DT::renderDT(server = FALSE, {
        datatable(table_newcommer_temp())
    })

    output$anime_exp_users_select <- renderUI({
        count=1
        box_list <- list()
        for(count in 1:input$number_anime){
            box_list[[count]] <- selectizeInput(
                                      inputId = sprintf("viewed_%s",
                                                        count),
                                      label = sprintf("Anime %s",
                                                      count),
                                      choices = anime$Name ,
                                      multiple = FALSE)
            count = count+1
        }
        tagList(box_list)
    })
    output$anime_exp_users_score <- renderUI({
        count=1
        box_list <- list()
        for(count in 1:input$number_anime){
            box_list[[count]] <- numericInput(
                inputId = sprintf("weight_viewed_%s",
                                  count),
                label = sprintf("Your score: 1 to 10",
                                count),
                min = 1,
                max = 10,
                value = 5,
                step = 0.5)
            count = count+1
        }
        tagList(box_list)
    })

})




?selectizeInput

