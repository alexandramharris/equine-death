# Equine Death
# Alexandra Harris

# Source: https://data.ny.gov/widgets/q6ts-kwhk

# App token sign-up: https://data.ny.gov/profile/edit/developer_settings

# API call ----
library("RSocrata")

df <- read.socrata(
  "https://data.ny.gov/resource/q6ts-kwhk.csv"
)


# Set up ----
library(tidyverse)
library(googlesheets4)


# Analysis ----

# Deaths at Saratoga Race Course
saratoga_rc_deaths <- df %>% 
  filter(incident_type =="EQUINE DEATH" & track == "Saratoga Racecourse (NYRA)")

# Deaths by year at Saratoga Race Course
year <- saratoga_rc_deaths %>% 
  group_by(year) %>% 
  summarize(total_deaths = n())

# Searchable table
table <- saratoga_rc_deaths %>% 
  select(horse, incident_date, incident_description) %>% 
  mutate(horse = ifelse(str_detect(horse, "\\("), horse, str_to_title(horse))) %>% 
  arrange(desc(incident_date)) %>% 
  mutate(incident_date = format(incident_date, "%B %e, %Y")) %>% 
  rename(Horse = horse, Date = incident_date, Death = incident_description)

# Assign ID for cache
table$id <- seq.int(nrow(table), 1)

# Load cache
table_cache <- read.csv("table_cache.csv")

# Check for new rows
new_rows <- table[which(! table$id %in% table_cache$id), ]

# Append new rows
table_cache <- rbind(new_rows, table_cache)

# Remove ID
table_cache$id <- NULL

# Reformat date
table_cache$Date <- as.Date(table_cache$Date, "%d-%b-%y")
table_cache <- table_cache %>% 
  mutate(Date = format(Date, "%B %e, %Y"))



# Export ----

# Authorize for Actions
source("functions/func_auth_google.R")          
auth_google(email = "alexandra.harris@timesunion.com",
            service = "gsheet_precipitation",
            token_path = ".secret/gsheet_precipitation")

sheet_write(df, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "data")
sheet_write(year, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "year")
sheet_write(table_cache, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "table")
