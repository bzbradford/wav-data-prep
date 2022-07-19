# data prep for WAV dashboard

library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
library(shiny)




# Shapefiles --------------------------------------------------------------

counties <- read_sf("shp/wi-counties.shp") %>%
  clean_names() %>%
  rename(
    county_code = dnr_cnty_c,
    county_name = county_nam,
    shape_area = shapearea,
    shape_len = shapelen
  )
ggplot(counties) + geom_sf()
counties %>% write_sf("shp_clean/wi-counties.shp")


nkes <- read_sf("shp/nke-plans-2022.shp") %>%
  clean_names() %>%
  select(-"shape_le_1") %>%
  rename(shape_len = shape_leng)
ggplot(nkes) + geom_sf()
nkes %>% write_sf("shp_clean/nke-plans-2022.shp")


huc8 <- read_sf("shp/wi-huc-8.shp") %>%
  clean_names() %>%
  rename(shape_area = shape_are)
ggplot(huc8) + geom_sf()
huc8 %>% write_sf("shp_clean/wi-huc-8.shp")


huc10 <- read_sf("shp/wi-huc-10.shp") %>%
  clean_names() %>%
  rename(shape_area = shape_are)
ggplot(huc10) + geom_sf()
huc10 %>% write_sf("shp_clean/wi-huc-10.shp")


huc12 <- read_sf("shp/wi-huc-12.shp") %>%
  clean_names() %>%
  rename(shape_area = shape_are)
ggplot(huc12) + geom_sf()
huc12 %>% write_sf("shp_clean/wi-huc-12.shp")




# Station list -----------------------------------------------------------

stns <- read_csv("stations/cbsm-project-list.csv") %>%
  clean_names() %>%
  select(-starts_with("plan_")) %>%
  drop_na(latitude, longitude) %>%
  distinct(latitude, longitude, .keep_all = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, dnr_region)) %>%
  st_join(select(huc8, huc8_name)) %>%
  st_join(select(huc10, huc10_name)) %>%
  st_join(select(huc12, huc12_name)) %>%
  select(
    station_id,
    primary_station_name,
    wbic,
    official_name,
    county_name,
    dnr_region,
    huc8_name,
    huc10_name,
    huc12_name,
    max_fw_year,
    max_fw_date,
    latitude,
    longitude,
    geometry
    )
ggplot(stns) + geom_sf()

# read_csv("stations/cbsm-project-list.csv") %>%
#   clean_names() %>%
#   drop_na(latitude, longitude) %>%
#   group_by(latitude, longitude) %>%
#   summarise(n = n(), stnids = paste(station_id, collapse = ", ")) %>%
#   arrange(desc(n))

stns %>%
  mutate(label = paste0("[", station_id, "] ", primary_station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

stns %>%
  st_set_geometry(NULL) %>%
  write_csv("baseline_clean/station-list.csv")




# Baseline data -----------------------------------------------------------

baseline_data <- read_csv("baseline/Baseline Data 2019-2021.csv") %>%
  clean_names() %>%
  select(-starts_with("plan_")) %>%
  mutate(start_date = parse_date(start_date, "%m/%d/%Y"))


# Stream flow data
flow_data <- c(
  "baseline/Flow Data 2019.csv",
  "baseline/Flow Data 2020.csv",
  "baseline/Flow Data 2021.csv") %>%
  lapply(read_csv, col_types = cols(.default = "c")) %>%
  bind_rows() %>%
  clean_names() %>%
  select(
    station_id,
    primary_station_name,
    start_date,
    stream_width,
    average_stream_depth,
    average_surface_velocity,
    corrected_streamflow) %>%
  mutate(start_date = parse_date(start_date, "%m/%d/%Y")) %>%
  distinct(station_id, start_date, .keep_all = T)

baseline_joined <- baseline_data %>%
  left_join(flow_data)

baseline_joined %>% write_csv("baseline_clean/baseline-data.csv")

