library(shiny)
library(httr)
library(jsonlite)
source('apikey.R')

server <- function(input, output) {
  artist.data <- reactive({
    q_artist = "Beyonce" # q_artist = input$artist
    base.uri <- "http://api.musixmatch.com/ws/1.1/"
    endpoint <- "artist.search" # at top of ereactive use input$tab.type, ifstatement, then choose enpoint from API based on that
    query.params <- list(q_artist="arctic monkeys", apikey = api.key, page_size=100) 
    uri <- paste0(base.uri, endpoint)
    response <- GET(uri, query = query.params)
    body <- content(response, "text")
    parsed.data <- fromJSON(body)
    
    # Un-nests the lists (similar to flatten) to get data frame of interest
    artist.search <- parsed.data$message$body$artist_list$artist
    artist.ids <- artist.search %>% 
      select(artist_id)
    
    # stores ids as vector to be passed into a for loop
    artist.ids <- as.vector(artist.ids$artist_id)
    #filter(row_number() == 1) # just saves exact name, does not include collabs, "feat"
    # instead of filtering, save df as is (just ids) as vector
    # then, as a for loop, go through and get albums for every single artist listed
    # then, append those album ids into a vector
    # use albums ids to get track
    artist.ids # all artist ids involving the artist, including collabs/feat.
    
    # empty data frame album.list to add all relevent album ids for the artist
    album.list <- c()
    for(id in artist.ids) {
      
    # new get function = new uri
    albums.uri <- paste0(base.uri, "artist.albums.get")
    
    # uses saved artist id from first GET response
    # artist id is equal to whatever value is in artist.ids for each iteration
    query.params.albums <- list(artist_id=id, apikey = api.key) 
    album.response <- GET(albums.uri, query = query.params.albums)
    album.body <- content(album.response, "text")
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
     }
    }
    album.list
    # create empty data frame for all songs to be added to
    all.songs <- data.frame()
    all.songs
    # for loop, for however many albums
    length(album.id)
    #test album ids
    listy <- c(10570732, 23648084)
    songs <- c()
    for(val in album.list) { 
      
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
  
}