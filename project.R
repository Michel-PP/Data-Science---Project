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


ABitLessThanAMillionSong <- read_xlsx("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot 100 Audio Features.xlsx")

hot100 <- read.csv("C:\\Users\\Michel\\Documents\\UCR\\2nd Semester\\Data Science\\Codes\\Project\\Hot Stuff.csv")


hot100_dates <- hot100 %>% 
  rename(title = Song) %>% 
  select(-SongID) %>% 
  mutate(WeekID = as_date(WeekID, format = "%m/%d/%Y"))


medium %>% 
  select(WeekID) %>%
  mutate(WeekID = dmy(WeekID)) 

data <- inner_join(ABitLessThanAMillionSong,hot100_dates) %>% 
  filter(!is.na(danceability))


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
  
  
  