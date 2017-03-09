library(shiny)
library(httr)
library(jsonlite)
source('apikey.R')

server <- function(input, output) {
  artist.data <- reactive({
    #q_artist = "Beyonce" 
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
  
  #############################################genre#########################################################
  
  genre.data <- reactive({
    query.params <- list(f_music_genre_id="14", f_lyrics_language = "en",
                         s_track_rating = "desc",apikey = api.key, page_size=100)
    
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
    lyrics.df <- data.frame()
    stop.words <- scan("stop_words2.txt", character(), quote = "")
    
    #for(t in tracks$track_id){
    query.params <- list(track_id = tracks$track_id[3],#t,
                         apikey = api.key
    )
    response <- GET(uri, query = query.params)
    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    
    new.lyrics.body <- parsed.data$message$body$lyrics$lyrics_body
    
    #stripping garbage off the lyrics body
    new.lyrics.body <- str_replace_all(new.lyrics.body, "[\n]" , " ")
    new.lyrics.body <- gsub('This\\sLyrics.*', "", new.lyrics.body)
    new.lyrics.body <- str_to_lower(new.lyrics.body)
    
    new.lyrics.body <- strsplit(new.lyrics.body, " ")
    #its a list and we need it in a vector 
    new.lyrics.body <- unlist(new.lyrics.body)
    #strip insignificant words
    new.lyrics.body <- new.lyrics.body[!new.lyrics.body %in% stop.words]
    #make it into a table to count occurrences
    new.lyrics.count.table <- table(new.lyrics.body)
    #trimming off the non-premium message garbage
    new.lyrics.count.table <- new.lyrics.count.table[-(1:3)]
    print(new.lyrics.count.table)
    #track id will be replaced with t when the for loop is activated
    #new.lyrics <- data.frame(tracks$track_id[1], new.lyrics.body, new.lyrics.count)
    #lyrics.df<- rbind(lyrics.df, new.lyrics)
    #}
    hist(lyric.bodies)
    
  })
  
  output$genre.output <- renderText({
    genre.data()
  })
  
}