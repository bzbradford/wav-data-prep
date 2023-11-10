# data prep for WAV dashboard

library(tidyverse)
library(janitor)
library(lubridate)
library(sf)


# Shapefiles --------------------------------------------------------------

library(leaflet)
library(rmapshaper)
library(mapview)


## County Bounds ----

counties <- read_sf("shp/wi-counties.shp") %>%
  clean_names(case = "big_camel") %>%
  rename(
    ShapeLen = Shapelen,
    ShapeArea = Shapearea
  )

# npts(counties)
# counties_simp <- st_simplify(counties, preserveTopology = T, dTolerance = 100)
counties_simp <- ms_simplify(counties, 0.5)
# leaflet() %>%
#   addPolylines(data = counties, color = "black", weight = 2) %>%
#   addPolylines(data = counties_simp, color = "red", weight = 1)
# npts(counties_simp)
counties_simp %>% write_sf("shp_clean/wi-counties.shp")


## NKEs ----

nkes <- read_sf("shp/nke-plans-2022.shp") %>%
  clean_names(case = "big_camel") %>%
  select(-"ShapeLe1") %>%
  rename(ShapeLen = ShapeLeng) %>%
  drop_na(PlanId) %>%
  st_transform(4326)

nke_data <- nkes %>%
  select(
    nke_plan_name = PlanName,
    nke_plan_purpose = PurposeDe,
    nke_plan_objective = Objective,
    nke_start = StartDate,
    nke_end = EndDate) %>%
  mutate(across(where(is_character), ~str_to_sentence(str_trim(gsub("[\r\n]", "", .x)))))

npts(nkes)
ggplot(nkes) + geom_sf()

# simplify
nkes_simp <- ms_simplify(nkes, 0.5)
nkes_simp %>% write_sf("shp_clean/nke-plans-2022.shp")


## HUCs ----

# huc6 basins
huc6 <- read_sf("shp/wi-huc-6.shp") %>%
  clean_names(case = "big_camel") %>%
  st_transform(crs = 4326) %>%
  select(
    MajorBasin = MajorBasi,
    geometry
  )

# load huc8 subbasins and join huc6 info
huc8 <- read_sf("shp/wi-huc-8.shp") %>%
  clean_names(case = "big_camel") %>%
  select(-ShapeLen) %>%
  st_join(huc6, largest = T) %>%
  select(
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeAre,
    geometry
  )

# load DNR's watershed shapefile to get watershed ID numbers
dnr_ws_colnames <- read_csv("shp/wi-dnr-watersheds-colnames.csv")$colname
dnr_watersheds <- read_sf("shp/wi-dnr-watersheds.shp") %>%
  select(-OBJECTID) %>%
  st_transform(crs = 4326) %>%
  setNames(c(dnr_ws_colnames, "geometry")) %>%
  janitor::clean_names(case = "big_camel")

# load huc10 watersheds and join huc8 info
huc10 <- read_sf("shp/wi-huc-10.shp") %>%
  clean_names(case = "big_camel") %>%
  st_join(select(huc8, -Area), largest = T) %>%
  select(
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeAre,
    geometry
  )

leaflet() %>%
  addPolygons(data = huc10, color = "red", fillColor = "red") %>%
  addPolygons(data = dnr_watersheds, label = ~WSHED_NAME)

# load huc12 watersheds and join huc10 info
huc12 <- read_sf("shp/wi-huc-12.shp") %>%
  clean_names(case = "big_camel") %>%
  st_join(select(huc10, -Area), largest = T) %>%
  select(
    Huc12Code, Huc12Name,
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeAre,
    geometry
  )

# checks
npts(huc8)
npts(huc10)
npts(huc12)

# plot
ggplot(huc6) + geom_sf()
ggplot(huc8) + geom_sf()
ggplot(huc10) + geom_sf()
ggplot(huc12) + geom_sf()

# simplify shapefiles
huc8_simp <- ms_simplify(huc8, 0.5)
huc10_simp <- ms_simplify(huc10, 0.5)
huc12_simp <- ms_simplify(huc12, 0.5)
# dnr_ws_simp <- ms_simplify(dnr_watersheds, .5)

# save locally
huc8_simp %>% write_sf("shp_clean/wi-huc-8.shp")
huc10_simp %>% write_sf("shp_clean/wi-huc-10.shp")
huc12_simp %>% write_sf("shp_clean/wi-huc-12.shp")
# dnr_ws_simp %>% write_sf("shp_clean/wi-dnr-watersheds.shp")

## Export shapes ----

counties_simp %>% saveRDS("../WAV Dashboard/data/shp/counties")
nkes_simp %>% saveRDS("../WAV Dashboard/data/shp/nkes")
huc8_simp %>% saveRDS("../WAV Dashboard/data/shp/huc8")
huc10_simp %>% saveRDS("../WAV Dashboard/data/shp/huc10")
huc12_simp %>% saveRDS("../WAV Dashboard/data/shp/huc12")
# dnr_ws_simp %>% saveRDS("../WAV Dashboard/data/shp/dnr_ws")


# Load station list -----------------------------------------------------------

stn_list <- "stations/SWIMS Station Export v20230627.csv" %>%
  read_csv(col_types = list(.default = "c", STATION_ID = "d", LATITUDE = "d", LONGITUDE = "d")) %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    wbic,
    waterbody = official_waterbody_name,
    latitude,
    longitude) %>%
  mutate(station_name = str_squish(str_replace_all(station_name, "[^[:alnum:][:punct:] ]", ""))) %>%
  distinct(station_id, .keep_all = T) %>%
  arrange(station_id) %>%
  drop_na(station_id, latitude, longitude)

stn_list <-  read_csv("stations/WAV Stations v20230824.csv", col_types = list(.default = "c", STATION_ID = "d", LATITUDE = "d", LONGITUDE = "d")) %>%
  clean_names() %>%
  select(
    station_id,
    station_name = primary_station_name,
    wbic,
    waterbody = official_waterbody_name,
    latitude,
    longitude) %>%
  mutate(station_name = str_squish(str_replace_all(station_name, "[^[:alnum:][:punct:] ]", ""))) %>%
  distinct(station_id, .keep_all = T) %>%
  arrange(station_id) %>%
  drop_na(station_id, latitude, longitude)

# Baseline data -----------------------------------------------------------

## Baseline observations ----

baseline_in <- read_csv(
  "baseline/Baseline 2015-2023 v20231010.csv",
  col_types = list(.default = "c")) %>%
  clean_names()

names(baseline_in)

baseline_obs <- baseline_in %>%
  select(
    fieldwork_seq_no,
    datetime = start_datetime,
    station_id,
    station_type_code,
    ambient_air_temp,
    ambient_air_temp_units,
    water_temp,
    water_temp_units,
    d_o = do_mg,
    d_o_percent_saturation = do_pct,
    ph,
    transparency_tube_length,
    transparency_average = transparency_avg,
    specific_cond,
    weather_conditions,
    weather_last_2_days,
    current_stream_condition,
    additional_comments,
    fieldwork_comment
  ) %>%
  mutate(
    across(c(ambient_air_temp, water_temp, d_o, d_o_percent_saturation, ph, transparency_tube_length, transparency_average, specific_cond), as.numeric),
    across(c(weather_last_2_days, additional_comments, fieldwork_comment), ~ str_to_sentence(str_squish(.x))),
    across(c(fieldwork_seq_no, station_id), as.integer),
    across(datetime, ~ parse_datetime(.x, "%Y-%m-%d %h:%M %p"))
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  arrange(datetime) %>%
  filter(datetime >= "2015-1-1") %>%
  distinct(station_id, date, .keep_all = T)


## Baseline flow ----

flow_in <- read_csv("baseline/Streamflow 2015-2023 v20231010.csv", col_types = list(.default = "c")) %>%
  clean_names()

names(flow_in)

flow_obs <- flow_in %>%
  select(
    fieldwork_seq_no,
    datetime = start_datetime,
    station_id,
    stream_width,
    average_stream_depth,
    average_surface_velocity,
    entered_streamflow = stream_flow_cfs,
    calculated_streamflow = calculated_streamflow_cfs,
    corrected_streamflow = calculated_corrected_streamflow_cfs,
    flow_method_used
  ) %>%
  mutate(
    across(datetime, ~ parse_datetime(.x, "%Y-%m-%d %h:%M %p")),
    across(c(fieldwork_seq_no, station_id), as.integer),
    across(stream_width:corrected_streamflow, as.numeric),
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  mutate(streamflow_cfs = coalesce(entered_streamflow, corrected_streamflow, calculated_streamflow), .before = entered_streamflow) %>%
  replace_na(list(flow_method_used = "Not Specified")) %>%
  distinct(station_id, date, .keep_all = T)


## Join baseline obs + flow ----

add_units <- function(.data, col, units) {
  mutate(.data, "{col}_units" := case_when(is.na(.data[[col]]) ~ "", T ~ units), .after = {{col}})
}

baseline_data <- baseline_obs %>%
  left_join(flow_obs) %>%
  mutate(
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
  arrange(year, station_id, date)


## Determine which fieldwork events are missing all of the key baseline parameters ----
key_baseline_vars <- c(
  "ambient_air_temp",
  "water_temp",
  "d_o",
  "transparency_average",
  "streamflow_cfs"
)

has_key_baseline_data <- baseline_data %>%
  select(fieldwork_seq_no, all_of(key_baseline_vars)) %>%
  pivot_longer(2:last_col()) %>%
  summarize(valid = sum(!is.na(value)) > 0, .by = fieldwork_seq_no)
valid_fieldwork_seq_no <- has_key_baseline_data %>%
  filter(valid) %>%
  pull(fieldwork_seq_no)
message(nrow(baseline_data) - length(valid_fieldwork_seq_no), " fieldwork events dropped due to having no key baseline data")


## Final baseline join and filter ----
baseline_final <- baseline_data %>%
  left_join(stn_list) %>%
  relocate(station_name:longitude, .after = station_id) %>%
  arrange(station_id, date) %>%
  filter(fieldwork_seq_no %in% valid_fieldwork_seq_no)

# export
baseline_final %>% write_csv("baseline_clean/baseline-data.csv")
baseline_final %>%
  write_csv("../WAV Dashboard/data/baseline-data.csv.gz")

baseline_final %>%
  filter(year == 2023, station_id == 10010967) %>%
  view()


# Nutrient data -----------------------------------------------------------

tp_data <- list(
  "nutrient/tp-data-2019.csv",
  "nutrient/tp-data-2020.csv",
  "nutrient/tp-data-2021.csv",
  "nutrient/tp-data-2022.csv"
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
  left_join(stn_list) %>%
  relocate(station_name, .after = station_id) %>%
  arrange(station_id, date)

# export
tp_final %>% write_csv("nutrient_clean/tp-data.csv")
tp_final %>% write_csv("../Dashboard/data/tp-data.csv.gz")


# Thermistor data ---------------------------------------------------------

parse_hobos <- function(dir, year) {
  require(tidyverse)
  require(lubridate)
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
      (temp > 0) & (temp < 100),
      (temp > -40) & (temp < 40)
    )
  }

  warn <- function(sn, msg) {
    message(" => ", msg)
    # warning("SN ", sn, ": ", msg, call. = F)
  }

  files <- list.files(dir, "*.csv", full.names = T)

  if (length(files) == 0) stop("No csv files found in directory '", dir, "'")

  # if (!is.character(files)) stop("Unable to get list of files from given directory: ", dir)

  # sort files
  files <- files[order(nchar(files), files)]

  raw_data <- lapply(files, function(file) {

    # check file name and get logger SN
    if (file_ext(file) != "csv") stop("File '", file, "' is not a csv!")
    message("\nReading ", basename(file), "...")
    sn <- gsub(".csv", "", basename(file))

    first_line <- readLines(file, n = 1)
    skip <- 0

    if (grepl("Plot", first_line)) skip <- 1

    import <- read_csv(file, skip = skip, col_select = 1:3, col_types = "ccc")
    message(paste0("SN: ", sn))

    if (grepl("(*F)", names(import)[3], useBytes = T)) {
      unit <- "F"
    } else if (grepl("(*C)", names(import)[3], useBytes = T)) {
      unit <- "C"
    } else {
      stop("FATAL: Unable to determine temperature units! Require (°F) or (°C) in temperature column name!")
    }

    data <- import %>%
      select(DateTime = 2, Temp = 3) %>%
      mutate(Temp = round(as.numeric(Temp), 2)) %>%
      drop_na(Temp) %>%
      mutate(Unit = unit) %>%
      mutate(LoggerSN = as.numeric(sn), .before = 1) %>%
      mutate(DateTime = parse_date_time(DateTime, c(
        "%m/%d/%y %H:%M:%S",
        "%m/%d/%y %H:%M:%S %p",
        "%Y-%m-%d %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y %H:%M:%S",
        "%m-%d-%y %H:%M:%S"
      ), exact = T)) %>%
      mutate(Date = as.Date(DateTime), Year = lubridate::year(Date), .after = DateTime) %>%
      mutate(TempOK = temp_check(Temp, Unit))

    print(data)

    cat(paste0(
      " => ", nrow(data), " obs\n",
      " => ", as.Date(min(data$Date)), " - ", as.Date(max(data$Date)), "\n",
      " => ", min(data$Temp), " - ", max(data$Temp), " °", unit, "\n"))

    if (length(unique(data$Year)) > 1) {
      warn(sn, paste0("Multiple years in data range: ", paste(sort(unique(data$Year)), collapse = ", ")))
    }

    if (!all(data$TempOK)) {
      before <- nrow(data)
      bad_temps <- filter(data, !TempOK)
      data <- filter(data, TempOK)
      after <- nrow(data)
      warn(sn, paste0("Removed ", before - after, " temperature value(s) out of range!"))
      print(bad_temps)
    }

    data %>%
      mutate(
        TempF = ifelse(Unit == "F", Temp, round(c_to_f(Temp), 2)),
        TempC = ifelse(Unit == "C", Temp, round(f_to_c(Temp), 2))) %>%
      select(-c("Temp", "Unit", "TempOK"))
  })

  bind_rows(raw_data) %>%
    # mutate(Year = year, .before = Date) %>%
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
) %>%
  arrange(logger_sn, date_time) %>%
  distinct(logger_sn, date_time, .keep_all = T) %>%
  filter(year >= 2020)

# loggers per year
hobo_data_raw %>%
  count(logger_sn, year) %>%
  count(year)

## Hobo checks ----

# Collect list of SNs by year
hobo_sns <- hobo_data_raw %>%
  distinct(year, logger_sn)

# Load thermistor inventory, matching SNs with WAV Stns
therm_inventory <- read_csv("therm/therm-inventory.csv")

# Are we missing data for loggers in the inventory?

therm_inventory %>%
  left_join({
    hobo_sns %>%
      mutate(have_data = T)
    }) %>%
  replace_na(list(have_data = F)) %>% {
    print(.)
    write_csv(., "therm/Thermistors - missing data for loggers in inventory.csv")
  }


# Join the inventory
therm_stns <- hobo_sns %>%
  left_join(therm_inventory) %>%
  left_join(stn_list)

# Any loggers missing stations?
therm_stns %>%
  filter(is.na(station_id)) %>% {
    print(.)
    write_csv(., "therm/Thermistors - missing logger stations.csv")
  }



## Add locations to hobo data and trim----

hobo_data <- hobo_data_raw %>%
  left_join(therm_stns, multiple = "all") %>%
  drop_na(station_id) %>%
  mutate(
    after_deploy = ifelse(is.na(date_deployed), TRUE, date_time > date_deployed),
    before_removed = ifelse(is.na(date_removed), TRUE, date_time < date_removed)
  ) %>%
  filter(after_deploy & before_removed) %>%
  select(date_time:temp_c, logger_sn, device_type, station_id, station_name, latitude, longitude) %>%
  filter(year == lubridate::year(date)) %>%
  mutate(
    month = lubridate::month(date),
    day = lubridate::day(date),
    .after = "year")

# should show that no rows are missing location data
hobo_data %>% filter(is.na(latitude))


## Export ----

therm_inventory %>% write_csv("therm_clean/therm-info.csv")
therm_inventory %>% write_csv("../Dashboard/data/therm-info.csv.gz")

hobo_data %>% write_csv("therm_clean/therm-data.csv")
hobo_data %>% write_csv("../WAV Dashboard/data/therm-data.csv.gz")



# Finalize station list -------------------------------------------------------

## Check baseline ----

# number of baseline stations
baseline_data %>% count(station_id)

# any baseline stations missing from list?
baseline_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))

stn_tally_baseline <- baseline_data %>%
  group_by(station_id) %>%
  count()

## Check nutrient ----

# number of nutrient stations
tp_data %>% count(station_id)

# any nutrient stations missing?
tp_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check thermistor ----

# number of thermistor stations
therm_stns %>% count(station_id)

# any thermistor inventory entries missing a station id?
therm_stns %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Stations to keep ----

keep_stns <- unique(c(
  baseline_data$station_id,
  tp_data$station_id,
  therm_stns$station_id
)) %>%
  na.omit()

count(baseline_data, station_id, name = "baseline_obs")


## Create station SF ----

stn_list.sf <- stn_list %>%
  filter(station_id %in% keep_stns) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, DnrRegion, CountyNam)) %>%
  st_join(select(huc12, -Area)) %>%
  st_join(select(dnr_watersheds, WatershedName, WatershedCode)) %>%
  select(
    station_id, station_name,
    latitude, longitude,
    county_name = CountyNam,
    dnr_region = DnrRegion,
    wbic, waterbody,
    huc12 = Huc12Code,
    sub_watershed = Huc12Name,
    huc10 = Huc10Code,
    watershed = Huc10Name,
    dnr_watershed_name = WatershedName,
    dnr_watershed_code = WatershedCode,
    huc8 = Huc8Code,
    sub_basin = Huc8Name,
    major_basin = MajorBasin,
    geometry
  ) %>%
  distinct(station_id, .keep_all = T)

# dnr_watershed_join <- stn_list.sf %>%
#   select(station_id) %>%
#   st_join(dnr_watersheds) %>%
#   st_set_geometry(NULL)

# Plot stations
stn_list.sf %>%
  mutate(label = paste0("[", station_id, "] ", station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

all_stns <- stn_list.sf %>%
  st_set_geometry(NULL)

baseline_stns <- all_stns %>%
  filter(station_id %in% baseline_data$station_id)

tp_stns <- all_stns %>%
  filter(station_id %in% tp_data$station_id)

all_stns %>% write_csv("stations_clean/station-list.csv")
all_stns %>% write_csv("../WAV Dashboard/data/station-list.csv.gz")


# Misc/Test --------------------------------------------------------------------

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


baseline_final %>%
  group_by(station_id, year) %>%
  tally()


