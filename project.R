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

as_decade <- function(value){ return(value - value %% 10) }


ABitLessThanAMillionSong <- read_xlsx("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot 100 Audio Features.xlsx")

hot100 <- read.csv("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot Stuff.csv")

genre <- read.csv("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\genres4.csv", sep = ";") %>% slice(1:1046)

hot100_dates <- hot100 %>% 
  rename(title = Song) %>% 
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

data$genre[data$rock == TRUE] <- "Rock"
data$genre[data$pop == TRUE] <- "Pop"
data$genre[data$country == TRUE] <- "Country"
data$genre[data$soul == TRUE] <- "Soul"
data$genre[data$mellowGold == TRUE] <- "MellowGold"
data$genre[data$folk == TRUE] <- "Folk"
data$genre[data$rap == TRUE] <- "Rap"
data$genre[data$adultStandarts == TRUE] <- "Adult Standarts"
data$genre[data$motown == TRUE] <- "Motown"
data$genre[data$hipHop == TRUE] <- "Hip Hop"
data$genre[data$quietStorm == TRUE] <- "Quiet Storm"
data$genre[data$funk == TRUE] <- "Funk"
data$genre[data$disco == TRUE] <- "Disco"
data$genre[data$urbanContemporary == TRUE] <- "Urban Contemporary"
data$genre[data$rAndB == TRUE] <- "R&B"
data$genre[data$lounge == TRUE] <- "Lounge"
data$genre[data$jazz == TRUE] <- "Jazz"
data$genre[data$metal == TRUE] <- "Metal"
data$genre[data$easyListening == TRUE] <- "Easy Listening"
data$genre[data$singerSongwriter == TRUE] <- "Singer-Songwriter"

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
  subset(WeekID >= "2010-01-01" & WeekID <= "2020-12-31")

  
  
data_50 %>% 
  group_by(spotify_genre) %>% 
  ggplot(aes(WeekID, Week.Position, color = SongID)) +
  geom_line() + theme(legend.position = "none") 
  
df <- data %>% 
  select(spotify_genre) %>% 
  str_split(",") 

table(df) %>%  as_tibble() %>% arrange(desc(n)) %>% view()  

medium %>% 
  select(WeekID) %>%
  mutate(WeekID = dmy(WeekID)) 

write.csv2(table(df) %>%  as_tibble() %>% arrange(desc(n)), file = "C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\genres3.csv", row.names = FALSE)


  