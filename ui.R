library(shiny)
#source('apikey.R')
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
                            "he"
                      ),
                      mainPanel(
                             "he" 
                      )
                )
         )
)
shinyUI(ui)
