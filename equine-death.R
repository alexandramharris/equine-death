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

# Deaths by year at Saratoga Race Course
year <- df %>% 
  filter(incident_type =="EQUINE DEATH" & track == "Saratoga Racecourse (NYRA)") %>% 
  group_by(year) %>% 
  summarize(total_deaths = n()) %>% 
  rename(Year = year, `Horse deaths` = total_deaths)

# Incidents at Saratoga Race Course
incidents_saratoga <- df %>% 
  filter(track == "Saratoga Racecourse (NYRA)") %>% 
  group_by(incident_type = str_to_title(incident_type)) %>%   
  summarize(total_incidents = n()) %>% 
  arrange(desc(total_incidents)) %>% 
  rename(`Incident` = incident_type, Total = total_incidents)

# Division breakdown
division <- df %>% 
  group_by(division, incident_type) %>% 
  summarize(total_incidents = n()) %>% 
  rename(Division = division, `Incident` = incident_type, Total = total_incidents) %>% 
  mutate(Incident = paste0(str_to_upper(str_sub(Incident,1,1)), str_to_lower(str_sub(Incident,2,nchar(Incident))))) %>% 
  pivot_wider(names_from = Division, values_from = Total) %>% 
  arrange(desc(`Harness` + `Thoroughbred`)) %>% 
  filter(`Harness` + `Thoroughbred` >= 100)

# Reorder for graphic
division <- division %>% 
  select(Incident, Thoroughbred, Harness)

# Track and division deaths at each location
track <- df %>% 
  filter(incident_type =="EQUINE DEATH") %>%
  group_by(track, division) %>% 
  summarize(total_deaths = n()) %>% 
  arrange(desc(total_deaths)) %>% 
  rename(Track = track, Division = division, `Horse deaths` = total_deaths)

# Import track locations
locations <- read.csv("track_locations.csv")

# Join with track
track <- left_join(track, locations, by = "Track")

# NYRA track label
track <- track %>% 
  mutate(NYRA = ifelse(grepl("NYRA", Track), "NYRA", "Other"))

# Deaths by trainer
trainer <- df %>% 
  filter(incident_type =="EQUINE DEATH" & track == "Saratoga Racecourse (NYRA)") %>% 
  group_by(trainer = str_to_title(trainer)) %>% 
  summarize(total_deaths = n()) %>% 
  arrange(desc(total_deaths)) %>% 
  rename(Trainer = trainer, Deaths = total_deaths) %>% 
  head(10)

# Searchable table
table <- df %>% 
  filter(incident_type =="EQUINE DEATH" & track == "Saratoga Racecourse (NYRA)") %>%
  select(horse, incident_date, trainer, incident_description) %>% 
  mutate(trainer = str_to_title(trainer)) %>% 
  mutate(horse = ifelse(str_detect(horse, "\\("), horse, str_to_title(horse))) %>% 
  arrange(desc(incident_date)) %>% 
  mutate(incident_date = format(incident_date, "%B %e, %Y")) %>% 
  rename(Horse = horse, Trainer = trainer, Date = incident_date, Death = incident_description)

# Assign ID for cache
table$id <- seq.int(nrow(table), 1)

# Load cache
table_cache <- read.csv("table_cache.csv")

# Check for new rows
new_rows <- table[which(! table$id %in% table_cache$id), ]

# Append new rows
table_cache <- rbind(new_rows, table_cache)

# Save cache
write.csv(table_cache, "table_cache.csv", row.names=FALSE)

# Remove ID
table_cache$id <- NULL


# Export ----

# Authorize for Actions
source("functions/func_auth_google.R")          
auth_google(email = "alexandra.harris@timesunion.com",
            service = "gsheet_precipitation",
            token_path = ".secret/gsheet_precipitation")

sheet_write(df, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "data")
sheet_write(year, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "year")
sheet_write(incidents_saratoga, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "incidents_saratoga")
sheet_write(table_cache, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "table")
sheet_write(trainer, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "trainer")
sheet_write(track, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "track")
sheet_write(division, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "division")
