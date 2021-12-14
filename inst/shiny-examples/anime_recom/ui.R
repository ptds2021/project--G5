#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)

anime <- read.csv(here::here("data/anime_tidy.csv"))

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
shinyUI(ui)

