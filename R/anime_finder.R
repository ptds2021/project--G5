
#'@title Application Demo
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@return Launch the shiny application to give you the ability to search for animes
#'@export

anime_finder <- function() {

  library(tidyverse)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(cluster)
  library(ProjectG5)

  anime <- tibble(ProjectG5::anime)

  anime_with_ratings <- tibble(ProjectG5::anime_with_ratings)

  Gender_list <-
    unique(as.vector(str_split(str_c(anime$Genders, collapse = ", "), ", ")[[1]]))


  # Define UI for application that draws a histogram


  header <- dashboardHeader(title = "Demo")

  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Newcommer", tabName = "newcommer_tab"),
      menuItem("User based recommendation", tabName = "experienced_u_tab"),
      menuItem("Item based recommendation", tabName = "experienced_i_tab")
    )
  )

  body <- dashboardBody(
    tabItems(


      # Newcommer tab
      tabItem(tabName = "newcommer_tab",
              box(column(4,
                         numericInput(inputId = "age",
                                      label = "What is your age?",
                                      c(1:100),
                                      value = 20)),
                  column(4,
                         selectInput(inputId = "gender",
                                     label =  "What gender of anime you would want to see?",
                                     choices = Gender_list,
                                     multiple = TRUE,
                                     selected = "Sports")),
                  column(4,
                         sliderInput(inputId = "freetime",
                                     label = "How much time do you have in front of you?",
                                     min = 1,
                                     max = 180,
                                     value = 30,
                                     step = 1)),
                  width = "100%"),

              box(DT::DTOutput("table_newcommer"), width = "100%")

      ),




      # User based Recommendation tab
      tabItem(tabName = "experienced_u_tab",
              column(4,
                     box(
                       selectizeInput(
                         inputId = "viewed_u_tab",
                         label = "Anime selection",
                         choices = NULL ,
                         multiple = TRUE),
                       uiOutput("anime_exp_users_score"),



                       width = "100%"
                     )
              ),
              column(8,
                     box(width = "100%",
                         tags$h3("User Based recommendation"),
                         DT::DTOutput("recom_user_based"))
              ),

      ),
      tabItem(tabName = "experienced_i_tab",
              column(4,
                     box(
                       selectizeInput(
                         inputId = "viewed_i_tab",
                         label = "Select one anime",
                         choices = NULL ,
                         multiple = FALSE),



                       width = "100%"
                     )
              ),
              column(8,
                     box(width = "100%",
                         tags$h3("Item Based recommendation"),
                         DT::DTOutput("recom_item_based"))
              ),

      )
    )

  )

  ui <- dashboardPage(header,
                      sidebar,
                      body)


  server <- shinyServer( function(input, output, session) {


    #Newcommer tab
    table_newcommer_temp <- reactive({
      newcommer_recom(anime, input$age, input$gender, input$freetime)
    })

    output$table_newcommer <- DT::renderDT(server = FALSE, {
      datatable(table_newcommer_temp())
    })




    #User based recommendation tab
    updateSelectizeInput(session, 'viewed_u_tab',
                         selected = "", choices = anime$Name, server = TRUE)

    nb_of_anime <- reactive({
      selectize_count(input$viewed_u_tab)
    })
    anime_names <- reactive({
      selectize_names(input$viewed_u_tab)
    })

    output$anime_exp_users_score <- renderUI({
      create_numeric_input(anime_names(), nb_of_anime())
    })

    all_grades <-reactive({
      score_recovery(nb_of_anime(), input)
    })

    anime_selected_table<- reactive({
      temp_tibble <- tibble(Name = anime_names(), rating = all_grades())

      anime_selected <- left_join(anime, temp_tibble, by = c("Name" = "Name")) %>%
        filter(Name %in% anime_names())%>%
        mutate(user_id = 999999999)
    })


    user_item_u <- reactive({
      item = user_item_matrix(data = anime_with_ratings, adding_row = TRUE, row_data = anime_selected_table())
    })

    user_recommendation = reactive({
      user_based_recom(999999999, user_item_u() , anime_with_ratings, 5, 1, 10)

    })


    output$recom_user_based <- DT::renderDT(user_recommendation())



    #Item based recommendation tab
    updateSelectizeInput(session, 'viewed_i_tab',
                         selected = "", choices = anime$Name, server = TRUE)


    user_item_i <-reactive({
      item <- user_item_matrix()
    })

    item_recommendation_otp = reactive({
      item_recommendation(input$viewed_i_tab, user_item_i(), 5, anime)

    })

    output$recom_item_based <- DT::renderDT(item_recommendation_otp())




  })

  shinyApp(ui = ui, server = server)

}

