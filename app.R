library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(dplyr)
source('apikey.R')

ui <- navbarPage("MusixMatch",
                 # Describes data set, stakeholders, data origin
                 tabPanel("About",
                          fluidPage(
                            img(src = "MusixMatchlogo.jpg", height = 280, width = 500),
                            title = "Discover Lyrics",
                            h2("Discovering Lyrics"),
                            p("This app is designed for anyone interested in the lyrical
                              content of music, whether they are songwriters themselves or recreational 
                              listeners."),
                            p("Although a song wouldn't feel the same without its fundamental
                              instruments, melody, and rhythm, lyrics stick with us too. Lyrics are more than a few words to
                              sing along to on a drive to work. They can provide insights into the writing patterns and styles of musicians 
                              over time."),
                            p("Discovering the lyrical patterns in popular songs
                              can provide helpful inspiration to the aspiring songwriter. Especially
                              those who are interested in being successful in the music industry. It's
                              also just a fun way to learn extra details about your favorite songs and music eras."),
                            h3("Using the App"),
                            p("Use the tabs at the top of the page to navigate through the app. Each page will have separate
                              widgets that allow you to interact with some aspect of the musixmatch data."),
                            p("Use the", strong("Song"), "tab to see the most common lyric word used in a specific song."),
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
                 
                 # Song Panel
                 tabPanel("Song",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("artist", label = "Artist Input", placeholder = "Enter artist"),
                              textInput("song", label = "Song Input", placeholder = "Enter song"),
                              sliderInput("num.words", label = "Amount of Words", min = 1, max = 10,
                                          step = 1, value = 5),
                              actionButton("go", "Go")

                            ),
                            # How to Use the Song Panel
                            mainPanel(
                              p("Use the Song tab to search for any song
                                in our database. Simply:",
                                tags$ul(
                                tags$li("Type in an artist name
                                        in the", strong("Artist Input"), "section."),
                                tags$li("Then type the song
                                         title in the", strong("Song Input"), "section."),
                                tags$li("Choose the amount of words you want using the", 
                                        strong("Amount of Words"),"slider input."),
                                tags$li("Click the", strong("Go"), "button to get your chart"),
                                tags$li("Hover over the bars to get the word and its frequency in the song.")
                                )
                              ),  
                              br(),
                              plotlyOutput("artist.plot")
                            )
                            )
                          )
                 )


server <- function(input, output) {
  
  # Uses song, artist and slider input to find lyrics in song
  artist.data <- reactive( {
    # Artist Search
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    # Gets all albums associated with the artist id
    song.uri <- paste0(base.uri, "track.search")
    query.params.song <- list(q_track= input$song, q_artist = input$artist, apikey = api.key, 
                              page_size = 1,
                              s_track_rating="desc") 
    song.response <- GET(song.uri, query = query.params.song)
    song.body <- httr::content(song.response, "text")
    song.parsed <- fromJSON(song.body)
    song.data <- song.parsed$message$body$track_list$track 
    
    # selects first artist in row of artists that match the input
    song.data <- song.data %>% 
      select(track_name, track_id) %>% 
      filter(row_number() == 1)
    # Extract only unique track_ids
    songs.unique <- unique(song.data$track_id)
    # Vector of commonstop words to ignore in analysis
    stop.words <- scan("stop_words2.txt", character(), quote = "")
    
    # Get lyrics for the song
    lyric.uri <- paste0(base.uri, "track.lyrics.get")
    lyric.query.params <- list(apikey = api.key, track_id = songs.unique) 
    response <- GET(lyric.uri, query = lyric.query.params)
    # Only proceed through for loop if the page exists
    if(response$status_code == 200) {
      body <- httr::content(response, "text") 
      lyric.parsed <- fromJSON(body)
      lyric.body <- lyric.parsed$message$body$lyrics$lyrics_body
      
      # Gets rid of everything after "This Lyrics" commerical use warning
      lyric.body <- gsub('This\\sLyrics.*', "", lyric.body) 
      
      # Gets rid of unnessecary punctuation
      lyric.body <- gsub("[,?!.]", " ", lyric.body)
      
      # Splits entire lyric into individual words
      lyric.split <- strsplit(paste(lyric.body, collapse = " "), "[[:space:]]+")[[1]]
      
      # Removes common stop words
      lyric.split <- lyric.split[!lyric.split %in% stop.words]
      
      # Changes all words to lower case
      lyric.split <- tolower(lyric.split)

    }
    
    # table sorts each word into a box with its matching word, 
    # sorts in descending order to get most frequent on top
    # amount selected is based on slider input
    word.df <- data.frame(table(lyric.split)) %>% 
      arrange(desc(Freq)) %>% 
      head(input$num.words)
    return(word.df)
    
  })
  # color based on if word is more than 3 letters
  # hover show both values
  
  # only outputs after "Go" input button is clicked
  output$artist.plot <- renderPlotly({
    input$go
    # Overrides plotly's default hover tooltip, add own labels
    plot <- isolate(ggplot(data = artist.data(), mapping = aes(x = reorder(lyric.split, -Freq), y = Freq, 
                             text = paste("Word: \"", lyric.split,"\" |", "Frequency:", Freq))) +
      geom_bar(stat = 'identity', aes(), fill = "skyblue2") +
      labs(x = paste("Top", input$num.words, "words"), y = "Frequency", title = paste(input$song, "Top Lyrics")))
    # hide plotly default Mode Bar
    plot <- ggplotly(plot, tooltip = "text") %>% 
      config(displayModeBar = F)
    return(plot)
    
  })
  
}

shinyApp(ui = ui, server = server)