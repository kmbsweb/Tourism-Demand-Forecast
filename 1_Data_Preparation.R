##packages
library(tictoc)
library(dplyr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)

setwd("~/Desktop/City Bike2")
table <-read.csv("201809-citibike-tripdata.csv",header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)

#character to ymd_hms 
table$starttime <- ymd_hms(table$starttime)

top15_station <- table %>%
  group_by(start.station.name) %>%
  summarise(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  top_n(15,cnt)

# setting interval term
target_interval <- interval(start = ymd("2018-09-12"),
                            end = ymd("2018-09-26"))

# extract top_15 station and interval term
# aggregate by 15 station ymd_h
test <- left_join(table, top15_station, by="start.station.name") %>%
  filter(complete.cases(.)) %>%
  filter(starttime %within% target_interval) %>%
  select(start.station.name,starttime) %>%
  mutate(date = date(starttime)) %>%
  mutate(hour = hour(starttime)) %>%
  unite("DH", date:hour, sep= " ",remove = FALSE) %>%
  mutate(DH_1 = paste0(DH, "-00-00")) %>%
  group_by(start.station.name, DH_1) %>%
  summarise(PD_Volume = n()) %>%
  mutate(DH_1 = ymd_hms(DH_1)) 

time <-read.csv("Time_series.csv",header=T, fileEncoding="Shift_JIS",stringsAsFactors = FALSE)
time$Time_Series <- ymd_hm(time$Time_Series) 

test_a <- test %>%
  filter(start.station.name == "Pershing Square North") 

left_join(time, test_a, by = c("Time_Series" = "DH_1")) %>%
  mutate(PD_Volume = replace_na(PD_Volume, 0)) %>%
  mutate(start.station.name = replace_na(start.station.name, "Pershing Square North")) %>%
  mutate(date = date(Time_Series),
         hour = hour(Time_Series)) %>%
  ggplot(aes(x = hour, y = PD_Volume)) +
  geom_area(aes(color = "#4682b4", fill = "#4682b4"), 
            alpha = 0.3, position = position_dodge(0.8)) +
  scale_x_continuous(breaks = seq(0, 24, by = 6),
                     labels = seq(0, 24, by = 6)) +
  facet_wrap(~ date) +
  scale_color_manual(values = c("#4682b4")) +
  scale_fill_manual(values = c("#4682b4")) +
  theme_minimal()

vis_and_store <- function(x) {
  test_a <- test %>%
    filter(start.station.name == x) 
  
  test_b <- left_join(time, test_a, by = c("Time_Series" = "DH_1")) %>%
    mutate(PD_Volume = replace_na(PD_Volume, 0)) %>%
    mutate(start.station.name = replace_na(start.station.name, x)) %>%
    mutate(date = date(Time_Series),
           hour = hour(Time_Series)) 
  
  write.csv(test_b, file = paste0(x, ".csv"))
  
  test_c <- test_b %>%
    ggplot(aes(x = hour, y = PD_Volume)) +
    geom_area(aes(color = "#4682b4", fill = "#4682b4"), 
              alpha = 0.3, position = position_dodge(0.8)) +
    scale_x_continuous(breaks = seq(0, 24, by = 6),
                       labels = seq(0, 24, by = 6)) +
    facet_wrap(~ date) +
    scale_color_manual(values = c("#4682b4")) +
    scale_fill_manual(values = c("#4682b4")) +
    theme_minimal()             
  return(test_c)
}

vis_and_store("University Pl & E 14 St")