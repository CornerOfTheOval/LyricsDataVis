library(shiny)
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),

                            ),
                            mainPanel(
                              #plotOutput("plot")
                              verbatimTextOutput("artist.test")
                            )
                          )
                 ),
                 tabPanel("Genre",
                          sidebarLayout(
                            sidebarPanel(
                              "he"
                            ),
                            mainPanel(
                              "he" 
                            )
                          )
                 )
)

shinyUI(ui)
