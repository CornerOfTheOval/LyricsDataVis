library(shiny)
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),
                              textInput("lyric", label = "Lyric Input", placeholder = "Enter lyric here")
                              
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
                 ),
                 tabPanel("Year",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("year.range", "Year Range:", min = 1955, max = 2017, value = c(1955,2017), step = 1)
                            ),
                            mainPanel(
                              verbatimTextOutput("year.range")
                            )
                          )
                 )
)

shinyUI(ui)
