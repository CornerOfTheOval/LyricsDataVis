library(shiny)
source('apikey.R')
ui <- navbarPage("Navbar",
                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("text", label = "Text Input", value = "Enter lyric here")
                            ),
                            mainPanel(
                              #plotOutput("plot")
                              "he"
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

