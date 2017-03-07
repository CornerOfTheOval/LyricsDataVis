library(shiny)
library(httr)
library(jsonlite)
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

api.key <- "89b7ffa4912ce0420e1611ed804a106b"
library(dplyr)
server <- function(input, output) {
  artist.data <- reactive({
    q_artist = "Arctic Monkeys"
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.search" # at top of ereactive use input$tab.type, ifstatement, then choose enpoint from API based on that
    query.params <- list(q_artist="Arctic Monkeys", apikey = api.key, page_size=100) 
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params)
    response
    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    data <- parsed.data$message$body$artist_list$artist
    names(data)
    data.new <- data %>% 
      select(artist_id, artist_name) %>% 
    #View(data.new)
      filter(artist_name == q_artist)
    artist_id <- data.new$artist_id #this saves arctic monkeys id
    
    albums.uri <- paste0(base.uri, "artist.albums.get")
    query.params.albums <- list(artist_id=artist_id, apikey = api.key, page_size=100)
    abum.response <- GET(albums.uri, query = query.params.albums)
    album.body <- content(abum.response, "text")
    album.data <- fromJSON(album.body)
    album.data <- album.data$message$body$album_list$album
    View(album.data)
    album.id <- album.data$album_id
    
    # create empty data frame for all songs to be added to
    all.songs <- data.frame()
    all.songs
    # for loop, for however many albums
    length(album.id)
    #test album ids
    listy <- c(10570732, 23648084)
    songs <- c()
    for(val in album.id) { #change listy to album.id
      # for each album, save list of the tracks as a list (there will be diff number of songs, so important ot save as list )
      
    
    # Get tracks for albums
    tracks.uri <- paste0(base.uri, "album.tracks.get")
    #query.params.tracks <- list(album_id=val, apikey = api.key)
    query.params.tracks <- list(album_id=val, apikey = api.key)
    track.response <- GET(tracks.uri, query = query.params.tracks)
    track.body <- content(track.response, "text")
    track.parsed <- fromJSON(track.body)
    names(track.parsed$message$body$track_list$track)
    track.ready <- track.parsed$message$body$track_list$track
    #get lyrics_id
    track.stuff <- track.ready %>% 
      select(lyrics_id)
    songs <- append(songs, track.stuff)
    }
    print(songs)
    
    str(songs$lyrics_id)
    unique(songs$lyrics_id) == length(songs$lyrics_id)
    
    #can get lyrics.id, then use track.lyric API request
    # AFTER GETTING ALL SONGS, do unique on song id!!!
    
  })
  output$artist.test <- renderText({
    input$artist
  })
  
}
shinyApp(ui = ui, server = server)