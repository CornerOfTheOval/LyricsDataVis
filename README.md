# LyricsDataVis

# Link to App
https://kstack.shinyapps.io/LyricsDataVis/

 The MusiXMatch App takes data from Musixmatch: the world's
 largest catalog of song lyrics and translations

 We were especially curious about the frequencies of individual
 words themselves, as well as some information about
 artist ratings.

 The App is split into 4 parts: 

 About - info about our app
 
 Songs - User inputs a song title and artist name as well
 as the amount of 'top' words they want to show up on the x axis
 Hovering over the graph gives the x axis (word) and y axis 
 (Freq) of that word.
 
 Year - Able to choose date range and return the top lyric
 words in that time range. User manipulates slider input
 
 Ratings - shows the relationship between ratings and rankings for    different  countries. User inputs a two letter 
 country code.
 
Stakeholders
Songwriter 
> get ideas for popular words that they may incorporate into their own songs
  get a sense of what lyrics are popular at certain times
             
Recreational music enjoyer 
> This app is fun for everyone to use. They can input their favorite
songs, their favorite era, their favorite state and learn some little
details about them. 

Challenges
> We faced some big challenges working with the API. To start making
GET requests, we needed an API key, which has a limit of a few thousand 
calls per day. But the nature of our project required us, in some cases,
to use loops to GET the API with a different query parameter every time (we used loops.) This ended up slowing down our server up to 30 seconds. So we had
to make some sacrifices, such as changing Tab functions entirely.

The Future
> Going forward, we hope to get unlimited access to the API keys so we
can deal with a larger quantity of data and gather more overall results. It would also be interesting to be able to compare artists, songs or time periods
and be able to highlight key differences or similarities between them. 
   
 
