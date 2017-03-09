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
                              aggregated by Musixmatch, the world's largest lyrics platform. Musixmatch allows users to share,
                              track, and enjoy lyrics from any track."),
                            p("The data set includes information on song lyrics
                              and artists, a particular artist's discography, songs, and nationality, and the most
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
                              actionButton("go", "Go")),


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
                          
                 ),
                 tabPanel("Year",
                          verticalLayout(
                            sliderInput("year.range", "Year Range:", min = 1955, max = 2017, value = c(1955,2017), step = 1, width = "100%")
                            ,
                            helpText("Use the above slider to select a range of years to see the most common lyrics in that range"),
                            helpText("Hover over a bar to see the word and its frequency!"),
                            hr(),
                            
                            em("Data sampled for the year range will be the top 100 rated english songs for that year range"),
                            strong("Data may take a few seconds to load"),
                            plotlyOutput("year.plot", width = "100%"),
                            br()
                          )
                 ),
                 tabPanel("Ratings",
                          sidebarLayout(
                            sidebarPanel(
                              p("Enter one of the following country 
                                codes to search for the top 100 artists 
                                in our database: AU, CA, DE, ES, FI, IT,
                                PT, GB, GR, NL, PT, or US."),
                              textInput("country", label = "Country Input", placeholder = "Enter country code")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs", 
                                          tabPanel("Plot", plotOutput("ratings.plot")), 
                                          tabPanel("Table", tableOutput("table"))
                              )
                            )
                          )
                 )
        )


#=============================================================================================================

server <- function(input, output) {
  
  #words to be used in filtering the lyrics
  stop.words <- scan("stop_words2.txt", character(), quote = "")
  
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
  
  #====================================BY YEAR=============================================
  
  #useless, couldnt get this to work in time
  top.word <- reactive({
    word <- year.data %>% arrange(desc(Freq)) %>% top_n(1)
    print(word)
  })
  
  year.data <- reactive({
    #building params for the query
    query.params <- list(f_track_release_group_first_release_date_min = 20080101,#year.range.min(),
                         f_track_release_group_first_release_date_max = 20121231,#year.range.max(),
                         s_track_rating = "desc",
                         f_lyrics_language = "en",
                         apikey = api.key,
                         page_size=100
                         )
    
    #switching the get variables for the track.lyrics requests
    endpoint <- "track.lyrics.get"
    uri <- paste0(base.uri, endpoint)
    
    lyrics.words<- ""
    
    #compile all the
    for(t in tracks$track_id){
      query.params <- list(track_id = t,
                           apikey = api.key
      )
      response <- GET(uri, query = query.params)
      body <- content(response, "text")
      parsed.data <- fromJSON(body)
      
      new.lyrics.body <- parsed.data$message$body$lyrics$lyrics_body
      
      #stripping garbage off the lyrics body
      new.lyrics.body <- str_replace_all(new.lyrics.body, "[\n]" , " ")
      new.lyrics.body <- gsub('This\\sLyrics.*', "", new.lyrics.body)
      new.lyrics.body <- gsub("[,?!.()*]", " ", new.lyrics.body)
      new.lyrics.body <- str_to_lower(new.lyrics.body)
      
      new.lyrics.body <- strsplit(new.lyrics.body, " ")
      #its a list and we need it in a vector 
      new.lyrics.body <- unlist(new.lyrics.body)
      #add the new song lyrics to the total lyrics vector
      lyrics.words <- c(new.lyrics.body, lyrics.words)
    }
    #strip insignificant words
    lyrics.words <- lyrics.words[!lyrics.words %in% stop.words]
    #make it into a table to count occurrences
    lyrics.count.df <- table(lyrics.words)
    lyrics.count.df <- lyrics.count.df[-(1:3)]
    #make it a data frame to use with a visualization
    lyrics.count.df <- data.frame(lyrics.count.df)
    #stripping frequencies of 1 in order to better fit more important information
    lyrics.count.df <- filter(lyrics.count.df, Freq > 10)
    
    return(lyrics.count.df)
  })
  
  #This is the actual plot built from the scrubbed data above:
  output$year.plot <- renderPlotly({
    plot <- ggplot(data = year.data(),
                   mapping =  aes(x = reorder(lyrics.words,-Freq),
                                  y = Freq,
                                  text = paste0("Word: \"",lyrics.words,"\" | ","Frequency:",Freq)))+
      geom_bar(stat = "identity",
               position = position_dodge(width=4)#,aes(fill = Freq) COLOR THRAHSED THE TOOLTIP SO IT WAS REMOVED
      )+
      theme(axis.text.x = element_blank())+
      xlab("Lyric")+
      ylab("Frequency")+
      ggtitle("Top Words for Top Songs in Selected Year Range")
    
    plot <- ggplotly(plot,tooltip = c("text")) %>% 
      config(displayModeBar = F)
    return(plot)
  })
  
  #text formatting the values from the selector
  year.range.min <- reactive({
    paste0(input$year.range[1],"0101")
  })
  
  year.range.max <- reactive({
    paste0(input$year.range[2],"1231")
  })
  
  #unused but if removed seems to break even though theyre not used
  output$year.range.output <- renderText({
    paste(year.range.min(), year.range.max())
  })
  
  output$year.word.plot <- renderPlot({
    year.data
  })
  
  output$top.lyric <- renderText({
    top.word()
  })
  

  year.data <- reactive({
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "chart.artists.get" 
    uri <- paste0(base.uri, endpoint)
    query.params <- list(country = input$country,
                         apikey = api.key,
                         page = 1,
                         page_size = 100
    ) 
    
    # get data from "chart.artists.get"
    response <- GET(uri, query = query.params)
    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    countryData <- parsed.data$message$body$artist_list$artist
    artistRating <- select(countryData, artist_name, artist_rating)
    artistRating <- mutate(artistRating, ranking = 1:100)
    newNames <- c("Artist", "Rating", "Ranking")
    colnames(artistRating) <- newNames
    return(artistRating)
  })
  
  # generate a table of the 100 most popular artists for a country
  output$table <- renderTable({
    year.data()
  })
  
  # generate a plot to compare artist popularity (rank) with artist rating 
  output$ratings.plot <- renderPlot({
    plotRatings <- ggplot(year.data(), aes(x = Ranking, y = Rating)) +
      geom_point(span=1) +
      geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) +
      ggtitle("Comparison of Artist Popularity and Rating") +
      labs(x = "Artist Ranking in Popularity", y = "Artist Rating")
    #plotRatings <- ggplot(plotRatings)  
    return(plotRatings)
  })
  
  
 
 
}

shinyApp(ui = ui, server = server)