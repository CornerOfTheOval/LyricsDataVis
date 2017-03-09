library(shiny)
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),
                              sliderInput("num.words", label = "Choose the amount of top words", min = 1, max = 50,
                                          step = 1, value = 10)

                            ),
                            mainPanel(
                              plotOutput("artist.plot")
                            )
                          )
                 )
   
)

shinyUI(ui)
