library(shiny)
library(httr)
library(jsonlite)
#install.packages('tm')
library(tm)
source('apikey.R')
library(ggplot2)

server <- function(input, output) {
  
  artist.data <- reactive({
    # Artist Search
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.search" 
    query.params <- list(q_artist = input$artist, apikey = api.key, page_size = 100) 
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params)
    body <- httr::content(response, "text")
    parsed.data <- fromJSON(body)
    # Flattens parsed data list to get data frame of interest
    artist.search <- parsed.data$message$body$artist_list$artist
    
    # Selects all artist ids associated with the artist including collabs and feat.
    artist.ids <- artist.search %>% 
      select(artist_id)
    
    # Stores ids as vector to be passed into a loop
    artist.ids <- as.vector(artist.ids$artist_id)
    
    # Empty data frame to add all unique album ids for the artist
    album.list <- c()
    for(id in artist.ids) {
      
      # Gets all albums associated with the artist id
      albums.uri <- paste0(base.uri, "artist.albums.get")
      query.params.albums <- list(artist_id = id, apikey = api.key, page_size = 100) 
      album.response <- GET(albums.uri, query = query.params.albums)
      album.body <- httr::content(album.response, "text")
      album.data <- fromJSON(album.body)
      album.data <- album.data$message$body$album_list$album
      
      # For album ids that result in empty data frames,
      # If the data frame has some value, then the for loop continues 
       if(!(is.null(album.data))) {
          # Makes all album names lowercase
          album.data$album_name <- tolower(album.data$album_name)

          # Filters for unique album names
          album.data <- album.data[!duplicated(album.data$album_name),]
    
          # Saves just unique album ids for the artist
          album.id <- album.data$album_id
          
          # Adds album ids to empty vector
          album.list <- append(album.list, album.id)
       } 
    }
    # Empty vector for all songs to be appended to
    songs <- c()
    for(val in album.list) { 
      
      # Get tracks for albums
      tracks.uri <- paste0(base.uri, "album.tracks.get")
      query.params.tracks <- list(album_id = val, apikey = api.key, page_size = 100)
      track.response <- GET(tracks.uri, query = query.params.tracks)
      track.body <- httr::content(track.response, "text")
      track.parsed <- fromJSON(track.body)
      names(track.parsed$message$body$track_list$track)
      track.ready <- track.parsed$message$body$track_list$track
      # Saves the track id number for every album
      track.stuff <- track.ready %>% 
        select(track_id)
      
      # Stores each track id as a vector 
      track.stuff <- as.vector(track.stuff$track_id)
      
      # Add track ids to empty vector
      songs <- append(songs, track.stuff)
    }
    
    # Extract only unique track_ids
    songs.unique <- unique(songs)
 
    # Empty vector for most common word in each song to be appended to
    track.lyrics <- c()
    
    # common stop words to exclude from analysis
    remove <- c("the", "is", "a", "it", "I", "to", "of")
    
    for(track in songs.unique) {
      
      # Get lyrics for a track
      lyric.uri <- paste0(base.uri, "track.lyrics.get")
      lyric.query.params <- list(apikey = api.key, track_id = track) #track
      response <- GET(lyric.uri, query = lyric.query.params)
      
      # Only proceed through for loop if the page exists
      if(response$status_code == 200) {
        body <- httr::content(response, "text") 
        lyric.parsed <- fromJSON(body)
        lyric.body <- lyric.parsed$message$body$lyrics$lyrics_body
   
        # gets rid of everything after "This Lyrics" commerical use warning
        lyric.body <- gsub('This\\sLyrics.*', "", lyric.body) 
      
        # splits entire lyric into individual words
        # http://stackoverflow.com/questions/26159754/using-r-to-find-top-ten-words-in-a-text
        lyric.split <- strsplit(paste(lyric.body, collapse = " "), "[[:space:]]+")[[1]]
      
        # removes common stop words
        lyric.split <- lyric.split[!lyric.split %in% remove]
      
        # table sorts each word into a box with its matching word, sorts in ascending order
        # select the most frequent word 
        most.freq <- tail(sort(table(lyric.split)), 1) #turn 1 into input for each song
        
        # Add track ids to empty vector
        track.lyrics <- append(track.lyrics, most.freq)
      }
    }
    track.lyrics
    # Saves most common lyric per song as a data frame
    lyrics.df <- as.data.frame(as.table(track.lyrics)) %>% 
      arrange(desc(Freq)) %>% 
      head(10)
    lyrics.df
    ggplot(data = lyrics.df, aes(x = Var1, y = Freq)) +
      geom_point(size = 5) +
      facet_wrap(~Var1)
    
    # counts repeating words together, gather top 10 highest words
    grouped.lyrics <- as.data.frame(table(lyric.df$Var1)) %>% 
      arrange(desc(Freq)) %>% 
      head(10)
    ggplot(data = grouped.lyrics, mapping = aes(x = Var1)) +
      geom_bar()
    
    View(lyric.df)
    return(lyric.df)
    
  })
  
  output$artist.plot <- renderPlot({
    plot <- ggplot(data = artist.data(), mapping = aes(x = Var1)) +
      geom_bar()
    return(plot)
  })
  
}