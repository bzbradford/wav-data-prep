# data prep for WAV dashboard

library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
library(lubridate)





# Shapefiles --------------------------------------------------------------

library(rmapshaper)
library(mapview)

counties <- read_sf("shp/wi-counties.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(
    ShapeLen = Shapelen,
    ShapeArea = Shapearea
  )
npts(counties)
ggplot(counties) + geom_sf()
counties %>%
  ms_simplify(0.5) %>%
  write_sf("shp_clean/wi-counties.shp")

nkes <- read_sf("shp/nke-plans-2022.shp") %>%
  clean_names(case = "big_camel") %>%
  select(-"ShapeLe1") %>%
  rename(ShapeLen = ShapeLeng)
npts(nkes)
ggplot(nkes) + geom_sf()
nkes %>%
  ms_simplify(0.5) %>%
  write_sf("shp_clean/nke-plans-2022.shp")

huc8 <- read_sf("shp/wi-huc-8.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
npts(huc8)
ggplot(huc8) + geom_sf()
huc8 %>%
  ms_simplify(0.5) %>%
  write_sf("shp_clean/wi-huc-8.shp")


huc10 <- read_sf("shp/wi-huc-10.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
npts(huc10)
ggplot(huc10) + geom_sf()
huc10 %>%
  ms_simplify(0.5) %>%
  write_sf("shp_clean/wi-huc-10.shp")


huc12 <- read_sf("shp/wi-huc-12.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(ShapeArea = ShapeAre)
npts(huc12)
ggplot(huc12) + geom_sf()
huc12 %>%
  ms_simplify(0.5) %>%
  write_sf("shp_clean/wi-huc-12.shp")




# Station list -----------------------------------------------------------

## CBSM station list ----

stns <- read_csv("stations/cbsm-locs.csv") %>%
  clean_names() %>%
  drop_na(latitude, longitude) %>%
  distinct(latitude, longitude, .keep_all = T)

# additional stations, if missing from the cbsm list
extra_stns <- read_csv("stations/additional-locs.csv")

# already in the baseline stations
extra_stns %>% filter(station_id %in% stns$station_id)


## TP stations ----

tp_stns <- read_csv("stations/tp-locs.csv")

# already in the baseline stations
tp_stns %>% filter(station_id %in% stns$station_id)


## Thermistor stations ----

therm_stns <- read_csv("stations/therm-locs.csv")

# already in the baseline stations
therm_stns %>% filter(station_id %in% stns$station_id)





all_stns.sf <- stns %>%
  bind_rows(extra_stns) %>%
  bind_rows(tp_stns) %>%
  bind_rows(therm_stns) %>%
  distinct(station_id, .keep_all = T) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, DnrRegion, CountyNam)) %>%
  st_join(select(huc8, huc8_name = Huc8Name)) %>%
  st_join(select(huc10, huc10_name = Huc10Name)) %>%
  st_join(select(huc12, huc12_name = Huc12Name)) %>%
  select(
    station_id,
    station_name,
    wbic,
    waterbody,
    county_name = CountyNam,
    dnr_region = DnrRegion,
    huc8_name,
    huc10_name,
    huc12_name,
    latitude,
    longitude,
    geometry
  ) %>%
  arrange(station_id)

all_stns.sf %>%
  mutate(label = paste0("[", station_id, "] ", station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

all_stns <- all_stns.sf %>% st_set_geometry(NULL)
all_stns %>% write_csv("stations_clean/station-list.csv")



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
  rename(stream_flow_comments = additional_comments) %>%
  type_convert()

names(baseline_joined)

baseline_joined %>% write_csv("baseline_clean/baseline-data.csv")




# Nutrient data -----------------------------------------------------------



tp_data <- bind_rows(
  read_csv("nutrient/tp-data-2019.csv"),
  read_csv("nutrient/tp-data-2020.csv"),
  read_csv("nutrient/tp-data-2021.csv")
  ) %>%
  mutate(num_obs = rowSums(!is.na(select(., May:October)))) %>%
  filter(num_obs > 0) %>%
  pivot_longer(
    cols = May:October,
    names_to = "month_name",
    values_to = "tp") %>%
  mutate(month = match(month_name, month.name), .before = "month_name") %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")), .after = "month_name")

# if anything show up it needs to be added to the station list
tp_data %>%
  count(station_id, station_name) %>%
  filter(!(station_id %in% all_stns$station_id)) %>%
  mutate(latitude = "", longitude = "", wbic = "", waterbody_name = "")

# %>%
#   write_csv("nutrient/missing-nutrient-sites.csv")

tp_joined <- tp_data %>%
  select(-station_name) %>%
  left_join(all_stns)

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

