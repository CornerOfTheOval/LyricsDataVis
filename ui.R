library(shiny)
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist")
                              # add slider input for user to choose how many top songs to include
                              # limit --> 50?

                            ),
                            mainPanel(
                              plotOutput("artist.plot")
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
