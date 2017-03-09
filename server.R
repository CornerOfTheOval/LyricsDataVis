library(shiny)
library(httr)
library(jsonlite)
source('apikey.R')
library(ggplot2)
library(plotly)

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
    artist.names <- artist.search %>% 
      select(artist_name)
    # Stores ids as vector to be passed into a loop
    artist.names <- as.vector(artist.names$artist_name)

    # Empty data frame to add all unique album ids for the artist
    song.list <- c()
    for(name in artist.names) {
      
      # Gets all albums associated with the artist id
      song.uri <- paste0(base.uri, "track.search")
      query.params.song <- list(q_artist= name, apikey = api.key, 
                                page_size = 100, f_lyrics_language = "en") 
      song.response <- GET(song.uri, query = query.params.song)
      song.body <- httr::content(song.response, "text")
      song.parsed <- fromJSON(song.body)
      song.data <- song.parsed$message$body$track_list$track 
      if(!is.null(song.data)){
        song.data <- song.data %>% 
          select(track_name, track_id)
        no.dupes <- song.data[!duplicated(song.data$track_name),]
        # all songs for all artists
        song.list <- c(song.list, no.dupes)
       } 
    }
    
    # Extract only unique track_ids
    songs.unique <- unique(song.list$track_id)
    # Empty vector for most common word in each song to be appended to
    track.lyrics <- c()
    
    # common stop words to exclude from analysis
    remove <- c("the", "is", "a", "it", "I", "to",
                "of", "that", "then", "*******", 
                "...", "and", "in", "on", "And", "for", "an", "are", "as", "at", "be", "but")

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
        lyric.body
        # gets rid of everything after "This Lyrics" commerical use warning
        lyric.body <- gsub('This\\sLyrics.*', "", lyric.body) 
        
        # splits entire lyric into individual words
        lyric.split <- strsplit(paste(lyric.body, collapse = " "), "[[:space:]]+")[[1]]
        # removes common stop words
        lyric.split <- lyric.split[!lyric.split %in% remove]
        lyric.split
        # table sorts each word into a box with its matching word, sorts in ascending order
        # select the most frequent word 

        # Add track ids to empty vector
        track.lyrics <- append(track.lyrics, lyric.split)
      }
    }
    word.df <- data.frame(table(track.lyrics)) %>% 
      filter(!track.lyrics == "...") %>% 
      arrange(desc(Freq)) %>% 
      head(input$num.words)
    return(word.df)
    
  })
  # color based on if word is more than 3 letters
  # hover show both values
 #reorder(track.lyrics, -Freq)
  output$artist.plot <- renderPlotly({
    plot <- ggplot(data = artist.data(), mapping = aes(x = track.lyrics, y = Freq)) +
      geom_bar(stat = 'identity', fill = 'skyblue2') +
      labs(x = paste("Top", input$num.words, "words"), y = "Frequency", title = paste(input$artist, "Top Lyrics"))  
    plot <- ggplotly(plot)  
    return(plot)
  })
  

}
shinyServer(server)