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
saratoga_rc_deaths <- df %>% 
  filter(incident_type =="EQUINE DEATH" & track == "Saratoga Racecourse (NYRA)")


# Export ----

# Authorize for Actions
source("functions/func_auth_google.R")          
auth_google(email = "alexandra.harris@timesunion.com",
            service = "gsheet_precipitation",
            token_path = ".secret/gsheet_precipitation")


sheet_write(df, ss = "https://docs.google.com/spreadsheets/d/1c8Gpg1iQRu5hJ5xxe-xBzhqIoPOC3zhpE0-vzG-95HI", sheet = "data")
