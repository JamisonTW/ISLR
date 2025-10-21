library(tidyverse)

# data https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/

# reading in raw data
years <- 2020:2023
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

# fema data,to get the number of declared disasters per county per year

fema_raw <- read_csv("data/fema.csv") %>% 
  mutate(
    county = str_remove(designatedArea, "\\s*\\(.*\\)$"), 
    county = str_to_upper(county),
    state = {
      lookup <- setNames(state.name, state.abb)
      full <- lookup[toupper(state)]
      toupper(full)
    }
  )

# group fema data

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

library(tidycensus)

census_api_key("70a95da093399ce5ebc9ad8c1049514f084234ca", install = TRUE, overwrite = TRUE)

# get number of hosueholds per county

households <- purrr::map_dfr(
  years,
  ~ get_acs(
    geography = "county",
    variables = "B11001_001",
    year = .x,
    survey = "acs5",
    output = "tidy"
  ) %>%
    mutate(year = .x) %>% 
    select(GEOID, NAME, estimate, year) %>% 
    rename(
      households = estimate,
      county_state = NAME
    ) %>% 
    separate(
      county_state,
      into = c("county", "state"),
      sep = ", ",
      extra = "merge"
    ) %>% 
    mutate(
      county = toupper(str_remove(county, " County")),
      state = toupper(state)
    ) %>% 
    arrange(state, county)
)

# final joining of data sets
data <- households %>% 
  left_join(noaa, by = c("county" = "CZ_NAME", "state" = "STATE", "year" = "YEAR")) %>% 
  left_join(fema, by = c("county" = "CZ_NAME", "state" = "STATE", "year" = "year")) %>% 
  mutate(
    num_disasters = ifelse(is.na(num_disasters), 0, num_disasters),
    damage_household = damage / households
  )
