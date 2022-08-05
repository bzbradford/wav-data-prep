# data prep for WAV dashboard

library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
library(lubridate)




# Shapefiles --------------------------------------------------------------

counties <- read_sf("shp/wi-counties.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(
    ShapeLen = Shapelen,
    ShapeArea = Shapearea
  )
ggplot(counties) + geom_sf()
counties %>% write_sf("shp_clean/wi-counties.shp")

nkes <- read_sf("shp/nke-plans-2022.shp") %>%
  clean_names(case = "big_camel") %>%
  select(-"ShapeLe1") %>%
  rename(ShapeLen = ShapeLeng)
ggplot(nkes) + geom_sf()
nkes %>% write_sf("shp_clean/nke-plans-2022.shp")


huc8 <- read_sf("shp/wi-huc-8.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
ggplot(huc8) + geom_sf()
huc8 %>% write_sf("shp_clean/wi-huc-8.shp")


huc10 <- read_sf("shp/wi-huc-10.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
ggplot(huc10) + geom_sf()
huc10 %>% write_sf("shp_clean/wi-huc-10.shp")


huc12 <- read_sf("shp/wi-huc-12.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
ggplot(huc12) + geom_sf()
huc12 %>% write_sf("shp_clean/wi-huc-12.shp")




# Station list -----------------------------------------------------------

stns <- read_csv("stations/baseline-locs.csv") %>%
  clean_names() %>%
  select(-starts_with("plan_")) %>%
  rename(station_name = primary_station_name) %>%
  drop_na(latitude, longitude) %>%
  distinct(latitude, longitude, .keep_all = T)

therm_stns <- read_csv("stations/therm-locs.csv")

# these thermistor stations are already in the baseline stations
therm_stns %>% filter(station_id %in% stns$station_id)

tp_stns <- read_csv("stations/tp-locs.csv")

# these tp stations are already in the baseline stations
tp_stns %>% filter(station_id %in% stns$station_id)

# additional stations, if missing from the other sources
extra_stns <- read_csv("stations/additional-locs.csv")


all_stns <- stns %>%
  bind_rows(therm_stns) %>%
  bind_rows(tp_stns) %>%
  bind_rows(extra_stns) %>%
  distinct(station_id, .keep_all = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, dnr_region = DnrRegion)) %>%
  st_join(select(huc8, huc8_name = Huc8Name)) %>%
  st_join(select(huc10, huc10_name = Huc10Name)) %>%
  st_join(select(huc12, huc12_name = Huc12Name)) %>%
  select(
    station_id,
    station_name,
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

all_stns %>%
  mutate(label = paste0("[", station_id, "] ", station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

all_stns %>%
  st_set_geometry(NULL) %>%
  write_csv("stations_clean/station-list.csv")



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
    calculated_stream_flow,
    corrected_stream_flow = corrected_streamflow) %>%
  mutate(start_date = parse_date(start_date, "%m/%d/%Y")) %>%
  distinct(station_id, start_date, .keep_all = T) %>%
  mutate(station_id = as.numeric(station_id))

baseline_joined <- baseline_data %>%
  left_join(flow_data) %>%
  rename(
    date = start_date,
    station_name = primary_station_name) %>%
  mutate(station_id = as.numeric(station_id)) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date),
    .after = date
  ) %>%
  select(
    -c(
      "secondary_station_type",
      "dyn_form_code",
      "transparency_trial_1",
      "transparency_trial_2",
      "chloride_sample_collected",
      "point_outfall_number_chloride",
      "tp_sample_collected",
      "point_outfall_number_tp"
    )
  ) %>%
  mutate(d_o_units = "mg/L", .after = d_o) %>%
  mutate(transparency_units = "cm", .after = transparency_average) %>%
  mutate(stream_width_units = "ft", .after = stream_width) %>%
  mutate(stream_depth_units = "ft", .after = average_stream_depth) %>%
  mutate(surface_velocity_units = "ft/s", .after = average_surface_velocity) %>%
  mutate(stream_flow_units = "cfs", .after = corrected_stream_flow) %>%
  relocate(contains("_comment"), .after = everything()) %>%
  rename(stream_flow_comments = additional_comments)

names(baseline_joined)

baseline_joined %>% write_csv("baseline_clean/baseline-data.csv")




# Nutrient data -----------------------------------------------------------

tp_data <- read_csv("nutrient/tp-data-2021.csv") %>%
  mutate(num_obs = rowSums(!is.na(select(., May:October)))) %>%
  filter(num_obs > 0) %>%
  pivot_longer(
    cols = May:October,
    names_to = "month_name",
    values_to = "tp") %>%
  mutate(month = match(month_name, month.name), .before = "month_name") %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")), .after = "month_name")

tp_sites <- read_csv("nutrient/tp-sites-2021.csv")

# if anything show up it needs to be added to the station list
tp_sites %>% filter(!(station_id %in% all_stns$station_id))

tp_joined <- tp_data %>%
  left_join(tp_sites[c("station_id", "station_name", "latitude", "longitude")])

tp_joined %>% write_csv("nutrient_clean/tp-data.csv")




# Thermistor data ---------------------------------------------------------

# for now this is its own project under /Thermistor Data



# Station crossref --------------------------------------------------------

# baseline stations not in station lists
missing_baseline <- baseline_joined %>%
  count(station_id, station_name, name = "baseline_obs") %>%
  filter(!(station_id %in% all_stns$station_id))
missing_baseline %>%
  mutate(wbic = "", official_name = "", latitude = "", longitude = "") %>%
  write_csv("stations/missing-baseline-stations.csv")

