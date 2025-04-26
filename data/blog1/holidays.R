library(tidyverse)
library(ggplot2)
library(leaflet)
library(httr)
library(jsonlite)
library(tidygeocoder)
library(htmltools)

# See what the dataset looks like

holidays <- read_csv("niche_cultural_festivals_50.csv")

holidays_geocoded <- holidays %>% 
  geocode(Location, 
          method = 'osm',
          lat = latitude,
          long = longitude)

holidays_geocoded <- holidays_geocoded %>% 
  filter(!is.na(latitude))

saveRDS(holidays_by_season)

holidays_by_season <- holidays_geocoded %>% 
  mutate(season = case_when(
    grepl("January", `Time of Year`, ignore.case = TRUE) ~ "Winter",
    grepl("February|March", `Time of Year`, ignore.case = TRUE) ~ "Late Winter",
    grepl("March|April", `Time of Year`, ignore.case = TRUE) ~ "Spring",
    grepl("May|June", `Time of Year`, ignore.case = TRUE) ~ "Late Spring",
    grepl("July|August", `Time of Year`, ignore.case = TRUE) ~ "Summer",
    grepl("September|October", `Time of Year`, ignore.case = TRUE) ~ "Autumn",
    grepl("November", `Time of Year`, ignore.case = TRUE) ~ "Late Autumn",
    grepl("December", `Time of Year`, ignore.case = TRUE) ~ "Winter",
    TRUE ~ "Unknown"
  ))

saveRDS(holidays_by_season, "data/holidays_by_season.rds")

holiday_map <- leaflet(holidays_by_season) %>% 
  addCircleMarkers(~longitude, 
             ~latitude,
             color = ~case_when(
               season == "Winter" ~ "blue",
               season == "Late Winter" ~ "darkblue",
               season == "Spring" ~ "green",
               season == "Late Spring" ~ "darkgreen",
               season == "Summer" ~ "orange",
               season == "Late Summer" ~ "darkorange",
               season == "Autumn" ~ "red",
               season == "Late Autumn" ~ "darkred",
               TRUE ~ "gray"
             ),
             radius = 6,
             label = ~lapply(paste(
               `Event Name`, 
               Location, 
               Description, sep = "<br>"), HTML),
             stroke = FALSE,
             fillOpacity = 0.8
             ) %>% 
  addProviderTiles(providers$CartoDB.Positron)

holiday_map

  