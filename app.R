library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
source('apikey.R')
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
                              verbatimTextOutput("year.range.output")
                            )
                          )
                 )
)

#=============================================================================================================


server <- function(input, output) {
  artist.data <- reactive({
    q_artist = "Beyonce" 
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.search" # at top of ereactive use input$tab.type, ifstatement, then choose enpoint from API based on that
    query.params <- list(q_artist="Beyonce", apikey = api.key, page_size=100) 
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params)
    response
    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    parsed.data <- flatten(parsed.data)
    data <- parsed.data$message$body$artist_list$artist
    names(parsed.data$message$body$artist_list$artist)
    try <- flatten(parsed.data$message$body$artist_list)
    View(try)
    names(data)
    View(data)
    data.new <- data %>% 
      select(artist_id, artist_name) %>% 
    #View(data.new)
      filter(row_number() == 1) # just saves exact name, does not include collabs, "feat"
    View(data.new)
    artist_id <- data.new$artist_id #this saves arctic monkeys id
    artist_id
    # new get function = new uri
    albums.uri <- paste0(base.uri, "artist.albums.get")
    
    # uses saved artist id from first GET response
    query.params.albums <- list(artist_id=artist_id, apikey = api.key, page_size=200) # max is 100, how do we get all pages
    abum.response <- GET(albums.uri, query = query.params.albums)
    album.body <- content(abum.response, "text")
    album.data <- fromJSON(album.body)
    album.data <- album.data$message$body$album_list$album
    View(album.data)
    
    # checks for duplicated album names
    duplicated(album.data$album_name)
    
    album.data$album_name[1] == album.data$album_name[2]
    equalsIgnoreCase(album.data$album_name[1], album.data$album_name[2])
    # gets rows that are duplicated
    #duplicated equals.ignore.case
    # successfully gets unique albums!!!!
    album.data <- album.data[!duplicated(album.data$album_name),]
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
    songs <- unlist(songs)
    length(songs)
    
    #all unique songs
    good.songs <- unique(songs)
    songs
    lyric.ids <- good.songs[1:length(good.songs)]
    
    # creates data frame with all lyric ids
    lyric.df <- data.frame(lyric.ids)
    View(lyric.df)
    str(songs$lyrics_id)
    unique(songs$lyrics_id) == length(songs$lyrics_id)
    
    #can get lyrics.id, then use track.lyric API request
    
  })
  output$artist.test <- renderText({
    input$artist
  })
  
  #====================================BY YEAR=============================================
  year.data <- reactive({
    #building params for the query
    query.params <- list(f_track_release_group_first_release_date_min = 20000101,#year.range.min(),
                         f_track_release_group_first_release_date_max = 20051231,#year.range.max(),
                         s_track_rating = "desc",
                         f_lyrics_language = "en",
                         apikey = api.key,
                         page_size=100
                         )
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "track.search"
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params) #sending the request

    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    tracks <- parsed.data$message$body$track_list
    tracks <- tracks$track
    print(colnames(tracks))
    tracks <- select(tracks, track_id, track_name, artist_name)
    #View(tracks)
    
    endpoint <- "track.lyrics.get"
    uri <- paste0(base.uri, endpoint)
    lyric.bodies <- data.frame()
    
    for(t in tracks$track_id){
      query.params <- list(track_id = t,#tracks$track_id[1],
                           apikey = api.key
                          )
      response <- GET(uri, query = query.params)
      body <- content(response, "text")
      parsed.data <- fromJSON(body)
      new.lyric <- data.frame(t, parsed.data$message$body$lyrics$lyrics_body)
      lyric.bodies<- rbind(lyric.bodies, new.lyric)
    }
    
  })
  
  #text formatting the values from the selector
  year.range.min <- reactive({
    paste0(input$year.range[1],"0101")
  })
  
  year.range.max <- reactive({
    paste0(input$year.range[2],"1231")
  })
  
  output$year.range.output <- renderText({
    paste(year.range.min(), year.range.max())
    year.data()
  })
  
}
shinyApp(ui = ui, server = server)