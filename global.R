# library setup---
## data wrangling
library(dplyr)
library(lubridate)
library(glue)
library(scales)
library(tidyr)
library(padr)
#library(rsample)
  
## visualisasi
library(ggplot2)
library(plotly)
library(DT)
  
## shiny
library(shiny)
library(shinydashboard)
library(bslib)
library(caret)
library(randomForest)
#library(knitr)
#library(markdown)
#library(shinyWidgets)
#library(thematic)

#----------------------
options(scipen=999)

flfare <- read.csv("German Air Fares.csv", encoding = "UTF-8")

flfare_clean <- 
  flfare %>%
  # rename(#departure_city = "ï..departure_city",
         #price = "price..â...") %>% 
  rename(price = "price....") %>%
  filter(trim(departure_city) != "", trim(arrival_city) != "") %>% 
  mutate(departure_datetime = dmy_hm(paste(departure_date, " ", departure_time)),
         arrival_datetime = dmy_hm(paste(departure_date, " ", arrival_time)),
         arrival_datetime = as_datetime(ifelse(arrival_datetime<departure_datetime,
                                               arrival_datetime + days(1), arrival_datetime)),
         scrape_date = dmy(scrape_date),
         departure_date = dmy(departure_date),
         price = gsub(pattern = '"', replacement = '', x = price),
         price = gsub(pattern = ',', replacement = '', x = price),
         price = as.numeric(price),
         route = as.factor(paste(substr(departure_city, 1, 3), "-", substr(arrival_city, 1, 3))),
         stops = as.factor(case_when(stops == "(1 stop)" ~ "1 stop",
                                     stops == "(1 Stopp)" ~ "1 stop",
                                     stops == "(2 Stopps)" ~ "2 stops",
                                     TRUE ~ "direct")),
         flight_duration = as.numeric(difftime(arrival_datetime, departure_datetime)),
         airline = as.factor(airline)
  )

flfare_clean2 <- 
  flfare_clean %>% 
  drop_na() %>% 
  filter(flight_duration != 0, trim(airline) != "", route != "-")

flfare_clean3 <- 
  flfare_clean2 %>% 
  select(departure_city, arrival_city, airline, stops, departure_datetime, arrival_datetime, route, flight_duration, price) %>% 
  mutate(departure_city = as.factor(substr(departure_city, 1, 3)),
         arrival_city = as.factor(substr(arrival_city, 1, 3)))

flfare_clean4 <- 
  flfare_clean3 %>% 
  mutate(stops = as.numeric(case_when(stops == "1 stop" ~ 1,
                                      stops == "2 stops" ~ 2,
                                      TRUE ~ 0)))

flfare_clean5 <-
  flfare_clean4 %>%
  mutate(departure_datetime = unclass(departure_datetime),
         arrival_datetime = unclass(arrival_datetime))

flfare_separatedatecomps <- 
  flfare_clean4 %>%
  mutate(dep_day = day(departure_datetime),
         dep_dow = wday(departure_datetime),
         dep_mon = month(departure_datetime),
         dep_hr = hour(departure_datetime),
         dep_woy = week(departure_datetime),
         dep_qtr = quarter(departure_datetime),
         arr_day = day(arrival_datetime),
         arr_dow = wday(arrival_datetime),
         arr_mon = month(arrival_datetime),
         arr_hr = hour(arrival_datetime),
         arr_woy = week(arrival_datetime),
         arr_qtr = quarter(arrival_datetime)) %>% 
  select(-c("departure_datetime", "arrival_datetime"))

# model_lm_step_both_all_experiment <- readRDS("model_lm_experiment.RDS")
# model_rf_allstopnum_2x1 <- readRDS("model_rf_allstopnum_2x1.RDS")
# 
# model_rf_datefixed <- readRDS("model_rf_all_datefixed_2x1.RDS")
# model_rf_datecomp <- readRDS("model_rf_datecomp_2x1.RDS")

model_rf_datecomp_noroute <- readRDS("model_rf_datecomp_noroute_2x1.RDS")

#thematic_shiny(font = "auto")