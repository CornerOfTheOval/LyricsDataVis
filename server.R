library(shiny)
library(httr)
library(jsonlite)
api.key <- "89b7ffa4912ce0420e1611ed804a106b"
server <- function(input, output) {
  artist.data <- reactive({
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.get"
    query.params <- list(artist_id=input$artist)
    
  })
  output$artist.test <- renderText({
    input$artist
  })
  
}

shinyServer(server)