library(shiny)
ui <- navbarPage("MusixMatch",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),
                              sliderInput("num.words", label = "Amount of Words", min = 1, max = 20,
                                          step = 1, value = 10)

                            ),
                            mainPanel(
                               p("Use the Artist tab to search for any artist 
                                 in our database. Simply type in an artist name
                                 in the", strong("Artist Input"), "section. Then
                                 choose the amount of words you want using the", strong("Amount of Words"),
                                 "slider input."),
                               br(),
                              plotlyOutput("artist.plot")
                            )
                          )
                 )
   
)

shinyUI(ui)
