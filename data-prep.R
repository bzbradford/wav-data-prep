# data prep for WAV dashboard

library(tidyverse)
library(sf)
library(janitor)
library(leaflet)
library(lubridate)
library(rmapshaper)
library(mapview)


# Shapefiles --------------------------------------------------------------

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



# Station lists -----------------------------------------------------------

stn_list <- read_csv("stations/swims-all-stations.csv", col_types = list(.default = "c", STATION_ID = "d")) %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    wbic,
    waterbody = official_waterbody_name,
    latitude,
    longitude) %>%
  mutate(station_name = str_replace_all(station_name, "[^[:print:]]", ""))


# Baseline data -----------------------------------------------------------

baseline_obs <- list(
  "baseline/Baseline Data 2019-2021.csv",
  "baseline/Baseline Data 2022.csv") %>%
  lapply(read_csv, col_types = list(.default = "c", STATION_ID = "d")) %>%
  bind_rows() %>%
  clean_names() %>%
  select(
    station_id,
    # station_name = primary_station_name,
    # station_type_code,
    # secondary_station_type,
    # fieldwork_seq_no,
    group_desc,
    date = start_date,
    # sample_header_seq_no,
    # dyn_form_code,
    weather_conditions,
    # sampling_date,
    weather_over_past_2_days,
    current_stream_condition,
    ambient_air_temp = ambient_air_temp_field,
    ambient_air_temp_units,
    water_temperature,
    water_temperature_units,
    d_o_sampling_method,
    d_o,
    d_o_percent_saturation,
    p_h,
    transparency_tube_length,
    # transparency_trial_1,
    # transparency_trial_2,
    transparency_average,
    specific_conductance,
    # chloride_sample_collected,
    # point_outfall_number_chloride,
    # tp_sample_collected,
    # point_outfall_number_tp,
    additional_comments,
    fieldwork_comment) %>%
  distinct(station_id, date, .keep_all = T)

add_units <- function(.data, col, units) {
  mutate(.data, "{col}_units" := case_when(is.na(.data[[col]]) ~ "", T ~ units), .after = {{col}})
}

# Stream flow data
flow_obs <- list(
  "baseline/Flow Data 2019.csv",
  "baseline/Flow Data 2020.csv",
  "baseline/Flow Data 2021.csv",
  "baseline/Flow Data 2022.csv") %>%
  lapply(read_csv, col_types = list(.default = "c", STATION_ID = "d")) %>%
  bind_rows() %>%
  clean_names() %>%
  mutate(stream_flow_cfs = case_when(
    !is.na(stream_flow_cfs) ~ stream_flow_cfs,
    T ~ corrected_streamflow
  )) %>%
  select(
    station_id,
    date = start_date,
    stream_width,
    average_stream_depth,
    average_surface_velocity,
    stream_flow_cfs,
    flow_method_used) %>%
  replace_na(list(flow_method_used = "Not Specified")) %>%
  distinct(station_id, date, .keep_all = T)

baseline_data <- baseline_obs %>%
  left_join(flow_obs) %>%
  mutate(
    date = parse_date(date, "%m/%d/%Y"),
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date),
    .after = date
  ) %>%
  add_units("d_o", "mg/L") %>%
  add_units("transparency_average", "cm") %>%
  add_units("stream_width", "ft") %>%
  add_units("average_stream_depth", "ft") %>%
  add_units("average_surface_velocity", "ft/s") %>%
  relocate(contains("_comment"), .after = everything()) %>%
  rename(stream_flow_comments = additional_comments) %>%
  type_convert() %>%
  arrange(year, station_id, date)

baseline_final <- baseline_data %>%
  left_join(all_stns) %>%
  relocate(station_name, .after = station_id) %>%
  arrange(station_id, date)



# Nutrient data -----------------------------------------------------------

tp_data <- list(
  "nutrient/tp-data-2019.csv",
  "nutrient/tp-data-2020.csv",
  "nutrient/tp-data-2021.csv"
) %>%
  lapply(read_csv, col_types = cols(.default = "c", station_id = "d")) %>%
  bind_rows() %>%
  select(-"station_name") %>%
  mutate(num_obs = rowSums(!is.na(select(., May:October)))) %>%
  filter(num_obs > 0) %>%
  pivot_longer(
    cols = May:October,
    names_to = "month_name",
    values_to = "tp") %>%
  mutate(month = match(month_name, month.name), .before = "month_name") %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")), .after = "month_name") %>%
  arrange(year, station_id, date)

tp_final <- tp_data %>%
  left_join(all_stns) %>%
  relocate(station_name, .after = station_id) %>%
  arrange(station_id, date)



# Thermistor data ---------------------------------------------------------

parse_hobos <- function(dir, year) {
  require(tidyverse)
  require(janitor)
  require(tools)

  c_to_f <- function(c) {
    c * 1.8 + 32
  }

  f_to_c <- function(f) {
    (f - 32) * 5 / 9.0
  }

  temp_check <- function(temp, unit) {
    ifelse(
      unit == "F",
      (temp > 20) & (temp < 100),
      (temp > -5) & (temp < 40)
    )
  }

  files <- list.files(dir, "*.csv", full.names = T)

  if (length(files) == 0) stop("No csv files found in directory '", dir, "'")

  # if (!is.character(files)) stop("Unable to get list of files from given directory: ", dir)

  # sort files
  files <- files[order(nchar(files), files)]

  raw_data <- lapply(files, function(file) {

    if (file_ext(file) != "csv") return(warning("File '", file, "' is not a csv!"))
    fname <- gsub(".csv", "", basename(file))

    tryCatch({

      first_line <- readLines(file, n = 1)
      skip <- 0

      if (grepl("Plot", first_line)) skip <- 1

      import <- suppressMessages(read_csv(file, skip = skip, col_types = cols()))
      message(paste0("\nSN: ", fname))

      if (grepl("(*F)", names(import)[3])) {
        unit <- "F"
      } else if (grepl("(*C)", names(import)[3])) {
        unit <- "C"
      } else {
        stop("FATAL: Unable to determine temperature units! Require (°F) or (°C) in temperature column name!")
      }

      data <- import %>%
        select(DateTime = 2, Temp = 3) %>%
        mutate(Temp = round(Temp, 2)) %>%
        mutate(Unit = unit) %>%
        drop_na() %>%
        mutate(LoggerSN = as.numeric(fname), .before = 1) %>%
        mutate(DateTime = lubridate::parse_date_time(DateTime, c("mdy HMS p", "mdy HMS", "ymd HMS"))) %>%
        mutate(Date = as.Date(DateTime), .before = DateTime) %>%
        mutate(TempOK = temp_check(Temp, Unit))

      cat(paste0(
        " => ", nrow(data), " obs\n",
        " => ", as.Date(min(data$Date)), " - ", as.Date(max(data$Date)), "\n",
        " => ", min(data$Temp), " - ", max(data$Temp), " °", unit, "\n"))
      if (lubridate::year(min(data$Date)) != lubridate::year(max(data$Date))) {
        message("WARNING: Multiple years in data range!")
      }
      if (!all(data$TempOK)) {
        before <- nrow(data)
        data <- filter(data, TempOK)
        after <- nrow(data)
        message("WARNING: Removed ", before - after, " temperature value(s) out of range!")
      }

      data %>%
        mutate(
          TempF = ifelse(Unit == "F", Temp, round(c_to_f(Temp), 2)),
          TempC = ifelse(Unit == "C", Temp, round(f_to_c(Temp), 2))) %>%
        select(-c("Temp", "Unit", "TempOK"))
    },
      error = function(e) { message("FATAL: Failed to parse data for SN ", fname,": ", e, "\n") }
    )
  })

  bind_rows(raw_data) %>%
    mutate(Year = year, .before = Date) %>%
    clean_names()
}


## Read Hobo data ----

#' File format expectations:
#' - Row 1 may be skipped, often contained 'plot title' or other heading.
#' - Obs # in column 1
#' - Date in column 2
#' - Temperature in column 3
#'   - Units must be specified in column name

hobos_2020 <- parse_hobos(dir = "therm/hobodata/2020", year = 2020)
hobos_2021 <- parse_hobos(dir = "therm/hobodata/2021", year = 2021)
hobos_2022 <- parse_hobos(dir = "therm/hobodata/2022", year = 2022)

hobo_data_raw <- bind_rows(
  hobos_2020,
  hobos_2021,
  hobos_2022
)


## Hobo checks ----

# Collect list of SNs by year
hobo_sns <- hobo_data_raw %>%
  distinct(year, logger_sn)

# Load thermistor inventory, matching SNs with WAV Stns
therm_inventory <- read_csv("therm/therm-inventory.csv")

# Join the inventory
therm_stns <- hobo_sns %>%
  left_join(therm_inventory) %>%
  left_join(stn_list)

# Any loggers missing stations?
therm_stns %>%
  filter(is.na(station_id))


## Add locations to hobo data and trim----

hobo_data <- hobo_data_raw %>%
  left_join(therm_stns) %>%
  drop_na(station_id) %>%
  mutate(
    after_deploy = ifelse(is.na(date_deployed), TRUE, date_time > date_deployed),
    before_removed = ifelse(is.na(date_removed), TRUE, date_time < date_removed)
  ) %>%
  filter(after_deploy & before_removed) %>%
  select(logger_sn:temp_c, station_id, station_name, latitude, longitude) %>%
  filter(year == lubridate::year(date))

# should show that no rows are missing location data
hobo_data %>% filter(is.na(latitude))


## Hobo summary ----

# 2022 summary
therm_stns %>% filter(year == 2022)
hobo_data %>%
  filter(year == 2022) %>%
  group_by(station_id) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    deploy_length = max_date - min_date) %>%
  summarize(
    min_date = mean(min_date),
    max_date = mean(max_date),
    deploy_length = mean(deploy_length)
  )





# Trim station list -------------------------------------------------------

## Check baseline ----

# number of baseline stations
baseline_data %>% count(station_id)

# any baseline stations missing from list?
baseline_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check nutrient ----

# number of nutrient stations
tp_data %>% count(station_id)

# any nutrient stations missing?
tp_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check thermistor ----

therm_stns %>% count(station_id)

therm_stns %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))



## Stations to keep ----

keep_stns <- unique(c(
  baseline_data$station_id,
  tp_data$station_id,
  therm_stns$station_id
))

# create sf
all_stns.sf <- stn_list %>%
  filter(station_id %in% keep_stns) %>%
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

# Plot stations
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

all_stns <- all_stns.sf %>%
  st_set_geometry(NULL)

baseline_stns <- all_stns %>%
  filter(station_id %in% baseline_data$station_id)

tp_stns <- all_stns %>%
  filter(station_id %in% tp_data$station_id)


# Export data -------------------------------------------------------------

all_stns %>% write_csv("stations_clean/station-list.csv")
# all_stns %>% write_csv("../Dashboard/data/station-list.csv")

baseline_final %>% write_csv("baseline_clean/baseline-data.csv")
# baseline_final %>% write_csv("../Dashboard/data/baseline-data.csv.gz")

tp_final %>% write_csv("nutrient_clean/tp-data.csv")
# tp_final %>% write_csv("../Dashboard/data/tp-data.csv")

therm_inventory %>% write_csv("therm_clean/therm-info.csv")
# therm_inventory %>% write_csv("../Dashboard/data/therm-info.csv")

hobo_data %>% write_csv("therm_clean/therm-data.csv.gz")
# hobo_data %>% write_csv("../Dashboard/data/therm-data.csv.gz")


# Misc --------------------------------------------------------------------

all_stns %>%
  filter(station_id == 223252)

all_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude)

baseline_stn_dupes <- baseline_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude) %>%
  select(station_id, station_name, latitude, longitude, everything())

baseline_stn_dupes %>% write_csv("baseline station duplicates.csv")

baseline_final %>%
  filter(station_id %in% baseline_stn_dupes$station_id) %>%
  arrange(latitude, longitude) %>%
  select(station_id, station_name, latitude, longitude, everything()) %>%
  write_csv("baseline data assigned to duplicate stations.csv")

tp_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude)


baseline_data %>%
  filter(station_id == 10040536, year == 2022)




