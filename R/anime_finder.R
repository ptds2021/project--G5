
#'@title Application Demo
#'
#'@description Function launching the application again
#'
#'@author Marie Bellier, Massimo Finini, Meri Likoska, Vania Rodrigues Telo Ramos, Xavier Renger
#'
#'@return Launch the shiny application to give you the ability to search for animes
#'
#'@import shiny
#'@import shinydashboard
#'@import DT
#'@import dplyr
#'@import tidyverse
#'@import tibble
#'@import stringr
#'
#'
#'
#'@export

anime_finder <- function() {
  globalVariables(c("Duration", "Episodes", "Genders", "Name", "Popularity", "Rating", "Type", "anime",
                    "anime_with_ratings", "item_id", "pivot_wider", "rating", "str_detect", "user_id"))

  anime <- tibble::tibble(ProjectG5::anime)

  anime_with_ratings <- tibble::tibble(ProjectG5::anime_with_ratings)

  Gender_list <-
    base::unique(
      base::as.vector(
        stringr::str_split(
          stringr::str_c(anime$Genders, collapse = ", "),
          ", "
        )[[1]]
      )
    )


  # Define UI for application that draws a histogram

  header <- shinydashboard::dashboardHeader(title = "Anime Recommendation")

  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Homepage",
                                tabName = "homepage_tab"),
      shinydashboard::menuItem("Newcommer",
                               tabName = "newcommer_tab"),
      shinydashboard::menuItem("Item based recommendation",
                               tabName = "experienced_i_tab"),
      shinydashboard::menuItem("User based recommendation",
                               tabName = "experienced_u_tab")
    )
  )

  body <- shinydashboard::dashboardBody(

    shinydashboard::tabItems(
      #Homepage Tab

      shinydashboard::tabItem(tabName = "homepage_tab",

              shiny::tags$h1("Welcome to the anime recommendation Application"),
              shiny::tags$h3("This application was done for a project during
                      the classProgramming tools in Data Science at the
                      University of Lausanne."),
              shiny::tags$h3("It's main aim was to provide a way for a user to
                      get a recommendation for an anime to watch. You can find
                      a brief description in the following."),
              shiny::tags$h1("Ways to find an anime"),
              shiny::tags$h3("Are you a great expert or are you
                      a novice with no knowledge about anime?"),
              shiny::tags$h3("- For the newbies: go to the first tab to have
                      recommandation according to your age, time that you have
                      in front of you and gender that you like."),
              shiny::tags$h3("- For experts: go to the next two tabs.
                      The first one will give you anime recommendations
                      according to the anime you have selected beforehand.
                      The second one will give you recommendations according
                      to the scores you have given to several anime selected
                      beforehand.")

      ),


      # Newcommer tab

      shinydashboard::tabItem(tabName = "newcommer_tab",

            shinydashboard::box(background='purple',
                                shiny::column(4,
                         shiny::numericInput(inputId = "age",
                                      label = "What is your age?",
                                      c(1:100),
                                      value = 20)),
                  shiny::column(4,
                         shiny::selectInput(inputId = "gender",
                                     label =  "What gender of anime would
                                     you want to see?",
                                     choices = Gender_list,
                                     multiple = TRUE,
                                     selected = "Sports")),
                  shiny::column(4,
                                tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black;
                                border-top: 1px solid black ;
                                                  border-bottom: 1px solid black ;}")),
                         shiny::sliderInput(inputId = "freetime",
                                     label = "How much time do you have
                                     in front of you (in minutes) ?",
                                     min = 1,
                                     max = 180,
                                     value = 30,
                                     step = 1)),
                  width = "100%"),

            shinydashboard::box(DT::DTOutput("table_newcommer"), width = "100%")

      ),




      # User based Recommendation tab
      shinydashboard::tabItem(tabName = "experienced_u_tab",
              shiny::column(4,
                     shinydashboard::box(
                       background='purple',
                       shiny::column(9,
                         shiny::selectizeInput(
                           inputId = "viewed_u_tab",
                           label = "Anime selection",
                           choices = NULL ,
                           multiple = TRUE),
                       ),
                       shiny::column(3,
                          shiny::selectInput(inputId = "n_recomm_1",
                                             label = "N\u00b0 recom",
                                             choices = c(5,10,15,20,25))
                       ),
                       shiny::actionButton(inputId = "submit_1",
                                    label = "Generate score boxes"),
                       width = "100%"
                     ),
                     shinydashboard::box(background='purple',
                       shiny::uiOutput("anime_exp_users_score"),
                       shiny::actionButton(inputId = "run_1",
                                    label = "Run"),
                       width = "100%"
                     )
              ),
              shiny::column(8,
                     shinydashboard::box(width = "100%",
                         shiny::tags$h3("User Based recommendation"),
                         DT::DTOutput("recom_user_based"))
              ),

      ),


      # Item based Recommendation tab
      shinydashboard::tabItem(tabName = "experienced_i_tab",
              shiny::column(4,
                     shinydashboard::box(
                       background='purple',
                       shiny::column(9,
                         shiny::selectizeInput(
                           inputId = "viewed_i_tab",
                           label = "Select one anime",
                           choices = NULL ,
                           multiple = FALSE),
                         shiny::actionButton(inputId = "run_2",
                                             label = "Run")
                       ),
                       shiny::column(3,
                            shiny::selectInput(inputId = "n_recomm_2",
                                               label = "N\u00b0 recom",
                                               choices = c( 5 , 10 , 15 , 20 , 25 ),
                         )
                       ),
                      width = "100%"
                     )
              ),
              shiny::column(8,
                     shinydashboard::box(width = "100%",
                         shiny::tags$h3("Item Based recommendation"),
                         DT::DTOutput("recom_item_based"))
              ),

      )
    )

  )

  ui <- shinydashboard::dashboardPage(skin= 'purple',
                                      header,
                                      sidebar,
                                      body)


  server <- shiny::shinyServer( function(input, output, session) {


    #Newcommer tab
    table_newcommer_temp <- shiny::reactive({
      newcommer_recom(anime, input$age, input$gender, input$freetime)
    })

    output$table_newcommer <- DT::renderDT(server = FALSE, {
      datatable(table_newcommer_temp())
    })




    #User based recommendation tab
    shiny::updateSelectizeInput(session, 'viewed_u_tab',
                         selected = "", choices = anime$Name, server = TRUE)

    nb_of_anime <- shiny::reactive({
      selectize_count(input$viewed_u_tab)
    })
    anime_names <- shiny::reactive({
      selectize_names(input$viewed_u_tab)
    })

    anime_test <- shiny::eventReactive(input$submit_1,{
      create_numeric_input(anime_names(), nb_of_anime(), id = "score_viewed")
    })

    output$anime_exp_users_score <- shiny::renderUI({
      anime_test()
    })

    all_grades <- shiny::reactive({
      score_recovery(nb_of_anime(), input, id = "score_viewed")
    })

    anime_selected_table<- shiny::reactive({
      temp_tibble <- tibble::tibble(Name = anime_names(), rating = all_grades())

      anime_selected <- dplyr::left_join(anime,
                                  temp_tibble,
                                  by = c("Name" = "Name")) %>%
        filter(Name %in% anime_names())%>%
        mutate(user_id = 999999999)
    })


    user_item_u <- shiny::reactive({
      item = user_item_matrix(data = anime_with_ratings,
                              adding_row = TRUE,
                              row_data = anime_selected_table())
    })

    user_recommendation = shiny::eventReactive(input$run_1, {
      user_based_recom(999999999,
                       user_item_u() ,
                       anime_with_ratings,
                       as.integer(input$n_recomm_1),
                       1,
                       10)

    })


    output$recom_user_based <- DT::renderDT(user_recommendation())



    #Item based recommendation tab
    shiny::updateSelectizeInput(session, 'viewed_i_tab',
                         selected = "", choices = anime$Name, server = TRUE)


    user_item_i <- shiny::reactive({
      item <- user_item_matrix()
    })

    item_recommendation_otp = shiny::eventReactive(input$run_2, {
      item_recommendation(input$viewed_i_tab,
                          user_item_i(),
                          as.integer(input$n_recomm_2),
                          anime)

    })

    output$recom_item_based <- DT::renderDT(item_recommendation_otp())




  })

  shiny::shinyApp(ui = ui, server = server)

}


