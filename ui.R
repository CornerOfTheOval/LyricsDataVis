library(shiny)
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", value = "Enter artist"),
                              textInput("lyric", label = "Lyric Input", value = "Enter lyric here")
                              
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
                              selectInput("genre", label = "Genre:",
                                          choices = c("Rock", "Pop", "Rap", "Jazz", "Country", "Hip Hop")
                              )
                            ),
                            mainPanel()
                              verbatimTextOutput("genre.output") 
                            )
                          )
                 )
)

shinyUI(ui)
