library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(dplyr)
source('apikey.R')

ui <- navbarPage("MusixMatch",
          tabPanel("About",
              fluidPage(
                     img(src = "MusixMatchlogo.jpg", height = 280, width = 500),
                         title = "Discover Lyrics",
                         h2("Discovering Lyrics"),

                         p("This app is designed for anyone interested in the lyrical
                           content of music, whether they are songwriters themselves or recreational 
                           listeners."),
                        p("Although a song wouldn't feel the same without its fundamental
                           instruments, melody and rhythm, lyrics stick with us too. Lyrics are more than a few words to
                           sing along to on a drive to work. They can provide insights into the writing patterns and styles of musicians 
                           over time."),
                        p("Knowing about patterns in popular lyrics, whether that's the most common words used by your favorite musician or
                          even a fun, recreational activity can provide helpful inspiration to the aspiring songwriter. Especially
                          individuals interested in the single words themselves, which makes up lyrics"),
                        h3("Using the App"),
                        p("Use the tabs at the top of the page to navigate through the app. Each page will have separate
                          widgets that allow you to interact with some aspect of the musixmatch data."),
                        p("Use the", strong("Artist"), "tab to see the most common lyric word used by musicians."),
                        p("Use the", strong("Date"), "tab to see the most common lyric word within a date range"),
                        p("Use the", strong("Ratings"), "tab to see the present highest ranked musicians"),
                        h3("Our Data: MusixMatch"),
                        p("Our data was
                          aggregated by Musixmatch, the world’s largest lyrics platform. Musixmatch allows users to share,
                          track, and enjoy lyrics from any track."),
                        p("The data set includes information on song lyrics
                           and artists, a particular artist’s discography, songs, and nationality, and the most
                          popular songs by country, among other more narrowly defined data. We decided to focus on lyrics from", 
                        em("artists,"), em("dates"), "and", em("ratings")),
                        br(),
                        p("Visit", a(href="https://developer.musixmatch.com/", "https://developer.musixmatch.com/"),
                          "to access the Musixmatch API.")
              )          
                            
            ),

                 tabPanel("Artist",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),
                              sliderInput("num.words", label = "Amount of Words", min = 1, max = 20,
                                          step = 1, value = 10)
                              
                            ),
                            mainPanel(
                              p("Use the Artist tab to search for any artist 
                                in our database. Simply type in an artist name
                                in the", strong("Artist Input"), "section. Then
                                choose the amount of words you want using the", strong("Amount of Words"),
                                "slider input."),
                              br(),
                              plotlyOutput("artist.plot")
                            )
                            )
                          )
                 
                 )


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
                                page_size = 100, f_lyrics_language = "en",
                                s_track_rating="desc") 
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
    stop.words <- scan("stop_words2.txt", character(), quote = "")
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
        lyric.body <- gsub("[,?!.]", " ", lyric.body)
        
        # splits entire lyric into individual words
        lyric.split <- strsplit(paste(lyric.body, collapse = " "), "[[:space:]]+")[[1]]
        
        # removes common stop words
        lyric.split <- lyric.split[!lyric.split %in% stop.words]
        lyric.split <- tolower(lyric.split)
        # table sorts each word into a box with its matching word, sorts in ascending order
        
        # Add track ids to empty vector
        track.lyrics <- append(track.lyrics, lyric.split)
      }
    }
    word.df <- data.frame(table(track.lyrics)) %>% 
      arrange(desc(Freq)) %>% 
      head(input$num.words)
    return(word.df)
    
  })
  # color based on if word is more than 3 letters
  # hover show both values
  output$artist.plot <- renderPlotly({
    plot <- ggplot(data = artist.data(), 
                   mapping = aes(x = reorder(track.lyrics, -Freq),y = Freq, 
                                 text = paste("Word: \"", track.lyrics,"\" |", "Frequency:", Freq))) +
      geom_bar(stat = 'identity', aes(fill = Freq), color = "gray90") +
      labs(x = paste("Top", input$num.words, "words"), y = "Frequency", title = paste(toupper(input$artist), "Top Lyrics"))  
    plot <- ggplotly(plot, tooltip = "text") %>% 
      config(displayModeBar = F)
    
    return(plot)
  })
  
  
}

shinyApp(ui = ui, server = server)