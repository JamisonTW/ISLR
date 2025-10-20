library(tidyverse)

# reading in raw data
years <- 2020:2024
file_names <- file.path("data", paste0("noaa_", years,".csv"))
noaa_raw <- map_dfr(file_names, ~ {read_csv(.x)})
head(noaa_raw)
# selecting relevant variables and grouping data
noaa <- noaa_raw %>% 
  select(STATE, CZ_NAME, YEAR, EVENT_TYPE, DAMAGE_PROPERTY, DEATHS_DIRECT, DEATHS_INDIRECT, INJURIES_DIRECT, INJURIES_INDIRECT) %>% 
  mutate(
    DAMAGE_PROPERTY = case_when(
      grepl("K", DAMAGE_PROPERTY, ignore.case = TRUE) ~ as.numeric(sub("K", "", DAMAGE_PROPERTY)) * 1e3,
      grepl("M", DAMAGE_PROPERTY, ignore.case = TRUE) ~ as.numeric(sub("M", "", DAMAGE_PROPERTY)) * 1e6,
      grepl("B", DAMAGE_PROPERTY, ignore.case = TRUE) ~ as.numeric(sub("B", "", DAMAGE_PROPERTY)) * 1e9,
      TRUE ~ as.numeric(DAMAGE_PROPERTY)
    )
  ) %>% 
  group_by(STATE, CZ_NAME, YEAR) %>% 
  summarise(
    storms = n(),
    damage = sum(DAMAGE_PROPERTY, na.rm = TRUE),
    deaths = sum(DEATHS_DIRECT, na.rm = TRUE) + sum(DEATHS_INDIRECT, na.rm = TRUE),
    injuries = sum(INJURIES_DIRECT, na.rm = TRUE) + sum(INJURIES_INDIRECT, na.rm = TRUE),
    .groups = "drop"
  )
head(noaa)

fema_raw <- read_csv("data/fema.csv") %>% 
  mutate(
    county = str_remove(designatedArea, "\\s*\\(.*\\)$"), 
    county = str_to_upper(county)  
  )

fema <- fema_raw %>%
  filter(state %in% unique(noaa$STATE)) %>%
  group_by(state, county, fyDeclared) %>%
  summarise(
    num_disasters = n(),
    .groups = "drop"
  ) %>%
  rename(
    STATE = state,
    CZ_NAME = county,
    year = fyDeclared
  )

# Map abbreviations to full names
state_lookup <- data.frame(
  abbr = state.abb,
  full = state.name
)

# Convert FEMA abbreviations to full state names
fema <- merge(fema, state_lookup, by.x = "STATE", by.y = "abbr", all.x = TRUE)
fema$state <- fema$full
fema$full <- NULL

