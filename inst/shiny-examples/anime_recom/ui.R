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

# Define UI for application that draws a histogram


header <- dashboardHeader(title = "Demo")

sidebar <- dashboardSidebar(
    sidebarMenu(
            menuItem("Newcommer", tabName = "newcommer_tab"),
            menuItem("Second", tabName = "experienced_tab")
    )
)

body <- dashboardBody(
    tabItems(
        # First tab content
        tabItem(tabName = "newcommer_tab",
                box(column(4,
                           numericInput(inputId = "age",
                             label = "Your age",
                             c(1:100),
                             value = 20)),
                    column(4,
                           selectInput(inputId = "gender",
                            label =  "Gender of interest",
                            choices = c("Action",
                              "Comedy",
                              "Shounen",
                              "Thriller",
                              "Mystery",
                              "Police",
                              "Supernatural",
                              "Psychological",
                              "Military",
                              "Super Power",
                              "Fantasy",
                              "Sports"),
                            multiple = TRUE,
                            selected = "Sports",
                            selectize = TRUE)),
                    column(4,
                           sliderInput(inputId = "freetime",
                            label = "Time available in minutes",
                            min = 1,
                            max = 180,
                            value = 30,
                            step = 1)),
                    width = "100%"),

            box(DT::DTOutput("table_newcommer"), width = "100%")

        ),

        # Second tab content
        tabItem(tabName = "experienced_tab",
                column(3, box()),
                column(9,box())
        )
    )

)

ui <- dashboardPage(header,
                    sidebar,
                    body)
shinyUI(ui)

unique(anime$Duration)
