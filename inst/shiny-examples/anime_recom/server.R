#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(cluster)
anime <- read.csv(here::here("data/anime_tidy.csv"))
anime_with_ratings <- read.csv(here::here("data/anime_with_ratings.csv"))

# Define server logic required to draw a histogram
shinyServer( function(input, output, session) {


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










 dissimilarity_matrix <- function(){
        anime_fctr <- anime  %>%
            mutate(
                Popularity_fct = Popularity / max(anime$Popularity)
            ) %>%
            mutate(
                Genders = as.factor(Genders),
                Type = as.factor(Type),
                Episodes = as.factor(Episodes),
                Studios = as.factor(Studios),
                Rating = as.factor(Rating),
                Popularity = as.factor(Popularity)
            )

        anime_distance = anime_fctr[,c("Genders", "Type", "Studios", "Rating", "Popularity_fct")]


        dissimilarity <- daisy(anime_distance, metric = "gower")

        dissimilarity_matrix <- as.matrix(dissimilarity)

        row.names(dissimilarity_matrix) <-  anime_fctr$item_id
        colnames(dissimilarity_matrix) <- anime_fctr$item_id

        return(dissimilarity_matrix)
    }



