# compare diff specs with ranking <- combine two data frames, one downloaded and one ranking
# need to figure out how to open and access the data frame in r
# can do web scraping for the ranking
# ranking of favorite types per year/decade

library(tidyverse)
library(modelr)
library(modelr)
library(mdsr)
library(Hmisc)
library(scales)
library(readxl)
library(lubridate)
library(shiny)
library(plyr)
library(dplyr)
library(collapsibleTree)


as_decade <- function(value){ return(value - value %% 10) }


ABitLessThanAMillionSong <- read_xlsx("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot 100 Audio Features.xlsx")

hot100 <- read.csv("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot Stuff.csv")

genre <- read.csv("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\genres4.csv",
                  sep = ";") %>% slice(1:1046)

medium <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium3.csv")

medium_50 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_50.csv")

medium_60 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_60.csv")

medium_70 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_70.csv")

medium_80 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_80.csv")

medium_90 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_90.csv")

medium_00 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_00.csv")

medium_10 <- read.csv2("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_10.csv")


hot100_dates <- hot100 %>% 
  dplyr::rename(title = Song) %>% 
  select(-SongID) %>% 
  mutate(WeekID = as_date(WeekID, format = "%m/%d/%Y"),
         year = year(WeekID),
         decade = as_decade(year))




data <- inner_join(ABitLessThanAMillionSong,hot100_dates) %>% 
  filter(!is.na(danceability)) %>% 
  mutate(rock = grepl("rock", spotify_genre),
         pop = grepl("pop", spotify_genre),
         country = grepl("country", spotify_genre),
         soul = grepl("soul", spotify_genre),
         mellowGold = grepl("mellow gold", spotify_genre),
         folk = grepl("folk", spotify_genre),
         rap = grepl("rap", spotify_genre),
         adultStandarts = grepl("adult standarts", spotify_genre),
         motown = grepl("motown", spotify_genre),
         hipHop = grepl("hip hop", spotify_genre),
         quietStorm = grepl("quiet storm", spotify_genre),
         funk = grepl("funk", spotify_genre),
         disco = grepl("disco", spotify_genre),
         urbanContemporary = grepl("urban contemporary", spotify_genre),
         rAndB = grepl("r&b", spotify_genre),
         lounge = grepl("lounge", spotify_genre),
         jazz = grepl("jazz", spotify_genre),
         metal = grepl("metal", spotify_genre),
         easyListening = grepl("easy listening", spotify_genre),
         singerSongwriter = grepl("singer-songwriter", spotify_genre),
         genre = NA
  )
data$genre[data$singerSongwriter == TRUE] <- "Singer-Songwriter"
data$genre[data$easyListening == TRUE] <- "Easy Listening"
data$genre[data$metal == TRUE] <- "Metal"
data$genre[data$jazz == TRUE] <- "Jazz"
data$genre[data$lounge == TRUE] <- "Lounge"
data$genre[data$rAndB == TRUE] <- "R&B"
data$genre[data$urbanContemporary == TRUE] <- "Urban Contemporary"
data$genre[data$disco == TRUE] <- "Disco"
data$genre[data$funk == TRUE] <- "Funk"
data$genre[data$quietStorm == TRUE] <- "Quiet Storm"
data$genre[data$hipHop == TRUE] <- "Hip Hop"
data$genre[data$motown == TRUE] <- "Motown"
data$genre[data$adultStandarts == TRUE] <- "Adult Standarts"
data$genre[data$rap == TRUE] <- "Rap"
data$genre[data$folk == TRUE] <- "Folk"
data$genre[data$mellowGold == TRUE] <- "MellowGold"
data$genre[data$soul == TRUE] <- "Soul"
data$genre[data$country == TRUE] <- "Country"
data$genre[data$pop == TRUE] <- "Pop"
data$genre[data$rock == TRUE] <- "Rock"





data_50 <- data %>% 
  subset(WeekID >= "1950-01-01" & WeekID <= "1959-12-31")

data_60 <- data %>% 
  subset(WeekID >= "1960-01-01" & WeekID <= "1969-12-31")

data_70 <- data %>% 
  subset(WeekID >= "1970-01-01" & WeekID <= "1979-12-31")

data_80 <- data %>% 
  subset(WeekID >= "1980-01-01" & WeekID <= "1989-12-31")

data_90 <- data %>% 
  subset(WeekID >= "1989-01-01" & WeekID <= "1999-12-31")


data_00 <- data %>% 
  subset(WeekID >= "2000-01-01" & WeekID <= "2009-12-31")

data_10 <- data %>% 
  subset(WeekID >= "2010-01-01" & WeekID <= "2021-12-31")

  medium_50 <- medium_50 %>% sample_n(10000)
  medium_60 <- medium_60 %>% sample_n(10000)
  medium_70 <- medium_70 %>% sample_n(10000)
  medium_80 <- medium_80 %>% sample_n(10000)
  medium_90 <- medium_90 %>%  sample_n(10000)
  medium_00 <- medium_00 %>%  sample_n(10000)
  medium_10 <- medium_10 %>% sample_n(10000)
  

  medium <- full_join(medium,medium_10)
  
data_50 %>% 
  group_by(spotify_genre) %>% 
  ggplot(aes(WeekID, Week.Position, color = genre)) +
  geom_line() 

medium %>% 
  ggplot(aes(genre, Peak.Position)) +
  geom_boxplot() + coord_flip() + facet_wrap(~ decade, nrow = 3)

medium %>% 
  ggplot(aes(decade, fill = genre)) +
  geom_bar(position = "fill") + theme_classic()
  
df <- data %>% 
  select(spotify_genre) %>% 
  str_split(",") 

table(df) %>%  as_tibble() %>% arrange(desc(n)) %>% view()  

medium %>% 
  select(WeekID) %>%
  mutate(WeekID = dmy(WeekID)) 

write.csv2(table(df) %>%  as_tibble() %>% arrange(desc(n)), file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\genres3.csv", row.names = FALSE)
write.csv2(medium, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium4.csv", row.names = FALSE)
write.csv2(medium_00, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_00.csv", row.names = FALSE)
write.csv2(medium_10, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_10.csv", row.names = FALSE)
write.csv2(medium_50, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_50.csv", row.names = FALSE)
write.csv2(medium_60, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_60.csv", row.names = FALSE)
write.csv2(medium_70, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_70.csv", row.names = FALSE)
write.csv2(medium_80, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_80.csv", row.names = FALSE)
write.csv2(medium_90, file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Data-Science---Project\\medium_90.csv", row.names = FALSE)


```{r, echo=FALSE}

renderPlot({
  medium %>%
    filter(decade == input$yearChoice) %>%
    ggplot(aes(WeekID, Week.Position, color = genre)) +
    geom_line() 
})

data %>% 
  filter(decade == "1950") %>% 
  ggplot(aes(decade, fill = genre)) +
  geom_bar(position = "fill") + theme_classic()


data %>% 
  group_by(genre) %>% 
  summarise((n = n())) %>% view()

collapsibleTree(
  tiny,
  hierarchy = c("decade", "genre", "Weeks.on.Chart"),
  width = 700,
  zoomable = TRUE
)


tiny <- medium %>% sample_n(10000)



```{r summary, echo=FALSE}



if (reactive({input$categoryChoice}) == "spotify_track_duration_ms"){
  summary(lm(spotify_track_duration_ms ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "tempo"){
  summary(lm(tempo ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "valence"){
  summary(lm(valence ~ Peak.Position, medium))
}


if (reactive({input$categoryChoice}) == "liveness"){
  summary(lm(liveness ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "accousticness"){
  summary(lm(accousticness ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "instrumentalness"){
  summary(lm(instrumentalness ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "speechiness"){
  summary(lm(accousticness ~ Peak.Position, medium))
}

if (reactive({input$categoryChoice}) == "loudness"){
  summary(lm(loudness ~ Peak.Position, medium))
}


if (reactive({input$categoryChoice}) == "energy"){
  summary(lm(energy ~ Peak.Position, medium))
}

```
```{r echo = FALSE}
selectInput("categoryChoice", label = "Chose a category:",
            choices = c("energy", "loudness", "speechiness", "accousticness", "instrumentalness", "liveness", "valence", "tempo",
                        "spotify_track_duration_ms"),
            selected = "spotify_track_duration_ms", multiple = FALSE)
```

```{r test, echo=FALSE}
summary(lm(reactive({input$categoryChoice}) ~ Peak.Position, data = medium))
```



summary(lm(energy ~ Peak.Position, data = medium))
