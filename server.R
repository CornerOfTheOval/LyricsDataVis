library(shiny)
library(httr)
library(jsonlite)
#install.packages('tm')
library(tm)
source('apikey.R')

server <- function(input, output) {
  
  artist.data <- reactive({
    # Artist Search
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.search" # at top of ereactive use input$tab.type, ifstatement, then choose enpoint from API based on that
    query.params <- list(q_artist = "Rolling Stones", apikey = api.key, page_size = 100) # input$artist
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params)
    body <- httr::content(response, "text")
    parsed.data <- fromJSON(body)
    # Un-nests the lists (similar to flatten) to get data frame of interest
    artist.search <- parsed.data$message$body$artist_list$artist
    artist.ids <- artist.search %>% 
      select(artist_id)
    # stores ids as vector to be passed into a for loop
    artist.ids <- as.vector(artist.ids$artist_id)
    
    # instead of filtering, save df as is (just ids) as vector
    # then, as a for loop, go through and get albums for every single artist listed
    # then, append those album ids into a vector
    # use albums ids to get track
    artist.ids # all artist ids involving the artist, including collabs/feat.
    
    # empty data frame album.list to add all relevent album ids for the artist
    count <- 0
    album.list <- c()
    for(id in artist.ids) {
      
    # new get function = new uri
      albums.uri <- paste0(base.uri, "artist.albums.get")
    
    # uses saved artist id from first GET response
    # artist id is equal to whatever value is in artist.ids for each iteration
      query.params.albums <- list(artist_id = id, apikey = api.key, page_size = 100) 
      album.response <- GET(albums.uri, query = query.params.albums)
      album.body <- httr::content(album.response, "text")
      album.data <- fromJSON(album.body)
      album.data <- album.data$message$body$album_list$album
    # if the data frame is not null, then the for loop continues to gather
    # the album id, if not, it moves on to next element in vector (the next artist id)
     if(!(is.null(album.data))) {
    # makes all album names same case
       album.data$album_name <- tolower(album.data$album_name)

    # filters for unique album names (some are repeated)
        album.data <- album.data[!duplicated(album.data$album_name),]
    
    # saves all album ids for the artist
        album.id <- album.data$album_id
        album.list <- append(album.list, album.id)
     } else {
       count <- count + 1
     }
    }
    # create empty vector for all songs to be appended to
    songs <- c()
    for(val in album.list) { 
      
      # Get tracks for albums
      tracks.uri <- paste0(base.uri, "album.tracks.get")
      #query.params.tracks <- list(album_id=val, apikey = api.key)
      query.params.tracks <- list(album_id = val, apikey = api.key, page_size = 100)
      track.response <- GET(tracks.uri, query = query.params.tracks)
      track.body <- httr::content(track.response, "text")
      track.parsed <- fromJSON(track.body)
      names(track.parsed$message$body$track_list$track)
      track.ready <- track.parsed$message$body$track_list$track
      #get track_id
      track.stuff <- track.ready %>% 
        select(track_id)
      
      # stores each track as vector element
      track.stuff <- as.vector(track.stuff$track_id)
      songs <- append(songs, track.stuff)
    }
    
    # Extract only unique track_ids
    songs.unique <- unique(songs)
    # for loop 
    # for each song, 
    # empty vector to store most common word in each song, append into at end of for loop
    # 
    # words to exclude from analysis
    remove <- c("the", "is", "a", "it", "I")
    track.lyrics <- c()
    for(track in songs.unique) {
      lyric.uri <- paste0(base.uri, "track.lyrics.get")
      lyric.query.params <- list(apikey = api.key, track_id = track) #track
      response <- GET(lyric.uri, query = lyric.query.params)
      if(response$status_code == 200){
        body <- httr::content(response, "text") 
        lyric.parsed <- fromJSON(body)
      
      # Filters for lyric body!!!
        lyric.body <- lyric.parsed$message$body$lyrics$lyrics_body
   
      # gets rid of everything after "This Lyrics", won't count stars anyway
      # but if we were to print out lyrics, would want to get rid of stars
        lyric.body <- gsub('This\\sLyrics.*', "", lyric.body) 
      
      # splits entire lyric into individual words
      # issue with new lines!!!
      # http://stackoverflow.com/questions/26159754/using-r-to-find-top-ten-words-in-a-text
        #lyric.split <- strsplit(lyric.body, "[[:space:]]+")[[1]]
        #lyric.split
        lyric.split <- strsplit(paste(lyric.body, collapse = " "), "[[:space:]]+")[[1]]
      
      # removes common stop words
        lyric.split <- lyric.split[!lyric.split %in% remove]
      
      # this just gets the word itself
      #names(which.max(table(lyric.split)))
      # table() --> sorts each word, put in order, goes in ascneding naturally, so get tail ends!
      #this gives word and freq
        most.freq <- tail(sort(table(lyric.split)), 1) #turn 1 into input for each song
        as.data.frame(most.freq) #this works, use after
        track.lyrics <- append(track.lyrics, most.freq)
      }
    }
    lyric.df <- as.data.frame(as.table(track.lyrics))
    View(lyric.df)
    return(lyric.df)
    
  })
  
  output$artist.test <- renderText({
    input$artist
  })
  
}