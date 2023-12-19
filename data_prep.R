# data prep for WAV dashboard


library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(leaflet)
library(plotly)
library(rmapshaper) # ms_simplify


# 1 => Load SWIMS Stations =====================================================

stn_list <-  read_csv(
  "stations/WAV Stations v20230824.csv",
  col_types = list(.default = "c", STATION_ID = "d", LATITUDE = "d", LONGITUDE = "d")) %>%
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



# 2 => Load Shapefiles =========================================================

quickmap <- function(shape) {
  message("Shape has ", nrow(shape), " objects and ", format(mapview::npts(shape), big.mark = ","), " vertices")
  leaflet(shape) %>%
    addTiles() %>%
    addPolygons(
      color = "black",
      weight = 2,
      opacity = .5,
      fillColor = "grey",
      fillOpacity = .1
    )
}

## Counties ----

counties <- read_sf("shp/wi-county-bounds.geojson") %>%
  clean_names("big_camel") %>%
  st_make_valid() %>%
  select(
    CountyName,
    DnrRegion = DnrRegionName,
    geometry
  )

counties.simp <- ms_simplify(counties, .25)

quickmap(counties)
quickmap(counties.simp)


## NKEs ----

nkes <- read_sf("shp/wi-nke-plans-2022.geojson") %>%
  clean_names("big_camel") %>%
  st_make_valid() %>%
  drop_na(PlanId)

nkes.simp <- ms_simplify(nkes, .25)

quickmap(nkes)
quickmap(nkes.simp)

nke_data <- nkes %>%
  select(
    nke_plan_name = PlanName,
    nke_plan_purpose = PurposeDe,
    nke_plan_objective = Objective,
    nke_start = StartDate,
    nke_end = EndDate) %>%
  mutate(across(where(is_character), ~str_to_sentence(str_trim(gsub("[\r\n]", "", .x)))))


## Watersheds ----
# transform to 3071 (WTM) for faster joining

# huc6 basins
huc6.wtm <- read_sf("shp/wi-huc06-basins.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071)

# load huc8 subbasins and join huc6 info
huc8.wtm <- read_sf("shp/wi-huc08-subbasins.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  select(-ShapeLeng) %>%
  st_join(huc6.wtm, largest = T) %>%
  select(
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc10 watersheds and join huc8 info
huc10.wtm <- read_sf("shp/wi-huc10-watersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  st_join(select(huc8.wtm, -Area), largest = T) %>%
  select(
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc12 watersheds and join huc10 info
huc12.wtm <- read_sf("shp/wi-huc12-subwatersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  st_join(select(huc10.wtm, -Area), largest = T) %>%
  select(
    Huc12Code, Huc12Name,
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# convert to WGS
huc6 <- st_transform(huc6.wtm, 4326)
huc8 <- st_transform(huc8.wtm, 4326)
huc10 <- st_transform(huc10.wtm, 4326)
huc12 <- st_transform(huc12.wtm, 4326)

# DNR watersheds (approx HUC10)
dnr_watersheds <- read_sf("shp/wi-dnr-watersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  select(
    DnrWatershedCode = WshedCode,
    DnrWatershedName = WshedName,
    SizeAcres = WatershedSizeAcresAmt,
    SizeSqMiles = WatershedSizeSqMilesAmt,
    TotalLakeAcres = TotalLakeAcresAmt,
    TotalWetlandAcres = TotalWetlandAcresAmt,
    geometry
  )

# simplify
huc8.simp <- ms_simplify(huc8, .5)
huc10.simp <- ms_simplify(huc10, .5)
huc12.simp <- ms_simplify(huc12, .5)
dnr_watersheds.simp <- ms_simplify(dnr_watersheds, .15)

# inspect
quickmap(huc6)
quickmap(huc8)
quickmap(huc8.simp)
quickmap(huc10)
quickmap(huc10.simp)
quickmap(huc12)
quickmap(huc12.simp)
quickmap(dnr_watersheds)
quickmap(dnr_watersheds.simp)


## Major waterbodies ----
# Top 1000 waterbodies in the state by area, for use on the pdf reports

waterbodies <- read_sf("shp/wi-major-lakes.geojson")


## Export shapes ----

counties.simp %>% saveRDS("../WAV Dashboard/data/shp/counties")
nkes.simp %>% saveRDS("../WAV Dashboard/data/shp/nkes")
huc8.simp %>% saveRDS("../WAV Dashboard/data/shp/huc8")
huc10.simp %>% saveRDS("../WAV Dashboard/data/shp/huc10")
huc12.simp %>% saveRDS("../WAV Dashboard/data/shp/huc12")
dnr_watersheds.simp %>% saveRDS("../WAV Dashboard/data/shp/dnr_watersheds")
waterbodies %>% saveRDS("../WAV Dashboard/data/shp/waterbodies")



# 3 => Baseline data ===========================================================

## Baseline observations ----

baseline_in <- read_csv(
  "baseline/Baseline 2015-2023 v20231205.csv",
  col_types = list(.default = "c")) %>%
  clean_names()

names(baseline_in)

baseline_obs <- baseline_in %>%
  select(
    fieldwork_seq_no,
    datetime = start_datetime,
    station_id,
    station_type_code,
    air_temp = ambient_air_temp,
    air_temp_units = ambient_air_temp_units,
    water_temp,
    water_temp_units,
    d_o = do_mg,
    d_o_percent_saturation = do_pct,
    ph,
    specific_cond,
    transparency = transparency_avg,
    transparency_tube_length,
    weather_conditions,
    weather_last_2_days,
    current_stream_condition,
    group_desc,
    fieldwork_comments = fieldwork_comment,
    additional_comments
  ) %>%
  mutate(
    across(c(air_temp, water_temp, d_o, d_o_percent_saturation, ph, specific_cond, transparency, transparency_tube_length), as.numeric),
    across(c(weather_last_2_days, additional_comments, fieldwork_comments), ~ str_to_sentence(str_squish(.x))),
    weather_conditions = str_to_sentence(gsub("_", " ", weather_conditions)),
    across(c(fieldwork_seq_no, station_id), as.integer),
    across(datetime, ~ parse_datetime(.x, "%Y-%m-%d %h:%M %p"))
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  arrange(datetime) %>%
  filter(datetime >= "2015-1-1") %>%
  distinct(station_id, date, .keep_all = T)
# ok if NAs were introduced


## Baseline flow ----

flow_in <- read_csv("baseline/Streamflow 2015-2023 v20231205.csv", col_types = list(.default = "c")) %>%
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
    across(stream_width:corrected_streamflow, as.numeric)
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  mutate(streamflow = coalesce(entered_streamflow, corrected_streamflow, calculated_streamflow), .before = entered_streamflow) %>%
  replace_na(list(flow_method_used = "Not Specified")) %>%
  distinct(station_id, date, .keep_all = T)
# ok if NAs were introduced


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
  add_units("transparency", "cm") %>%
  add_units("stream_width", "ft") %>%
  add_units("average_stream_depth", "ft") %>%
  add_units("average_surface_velocity", "ft/s") %>%
  add_units("streamflow", "cfs") %>%
  relocate(contains("_comment"), .after = everything()) %>%
  arrange(year, station_id, date)


## Determine which fieldwork events are missing all of the key baseline parameters ----
key_baseline_vars <- c(
  "air_temp",
  "water_temp",
  "d_o",
  "ph",
  "specific_cond",
  "transparency",
  "streamflow"
)

# find stations where all FSNs in a year have no baseline data and drop them
has_key_baseline_data <- baseline_data %>%
  select(station_id, year, fieldwork_seq_no, all_of(key_baseline_vars)) %>%
  pivot_longer(all_of(key_baseline_vars)) %>%
  summarize(has_baseline = sum(!is.na(value)) > 0, .by = c(station_id, year, fieldwork_seq_no)) %>%
  mutate(valid_year = any(has_baseline), .by = c(station_id, year)) %>%
  filter(valid_year)

valid_fieldwork_seq_no <- has_key_baseline_data$fieldwork_seq_no

invalid_baseline_data <- baseline_data %>%
  filter(!(fieldwork_seq_no %in% valid_fieldwork_seq_no))

# message says how many baseline fieldworks will be dropped due to lack of data
message(nrow(baseline_data) - length(valid_fieldwork_seq_no), " fieldwork events dropped due to having no key baseline data")



## Final baseline join and filter ----
baseline_final <- baseline_data %>%
  left_join(stn_list) %>%
  relocate(station_name:longitude, .after = station_id) %>%
  arrange(station_id, date) %>%
  filter(fieldwork_seq_no %in% valid_fieldwork_seq_no)

# export
baseline_final %>% write_csv("~clean/baseline-data.csv")
baseline_final %>% saveRDS("../WAV Dashboard/data/baseline-data")



# 4 => Nutrient data ===========================================================

tp_data <- list(
  "nutrient/tp-data-2019.csv",
  "nutrient/tp-data-2020.csv",
  "nutrient/tp-data-2021.csv",
  "nutrient/tp-data-2022.csv",
  "nutrient/tp-data-2023.csv"
) %>%
  lapply(read_csv, col_types = cols(.default = "c", station_id = "d", year = "d")) %>%
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
  arrange(year, station_id, date) %>%
  mutate(tp = as.numeric(if_else(tp == "ND", "0", tp)))
# non-detects replaced with zeros for now, need to implement something better later


# Milwaukee River Keeper data
mrk_tp <- read_csv("nutrient/2023 Milwaukee River Keeper total phosphorus.csv") %>%
  # select total phosphorus, should be all that's in here though
  filter(DNRParameterCode == 665) %>%
  select(
    station_id = StationID,
    volunteer_name = CollectorName,
    datetime = SampleStartDateTime,
    tp = ResultValueNo) %>%
  mutate(
    across(volunteer_name, str_to_title),
    datetime = parse_date_time2(datetime, "mdY HMS Op"),
    date = as.Date(datetime),
    year = year(date),
    month = month(date),
    month_name = month.name[month]) %>%
  mutate(num_obs = sum(!is.na(tp)), .by = c(year, station_id)) %>%
  select(-datetime) %>%
  filter(tp <= 1) # there are 2 anomalous? values

mrk_tp %>%
  arrange(desc(tp))

tp_final <- tp_data %>%
  bind_rows(mrk_tp) %>%
  left_join(stn_list) %>%
  relocate(station_name, .after = station_id) %>%
  arrange(station_id, date)

# export
tp_final %>% write_csv("~clean/tp-data.csv")
tp_final %>% saveRDS("../WAV Dashboard/data/tp-data")


# 5 => Thermistor data =========================================================

# reads in all raw hobo data csv files and parses them
parse_hobos <- function(dir, yr) {
  require(tidyverse)
  require(lubridate)
  require(janitor)
  require(tools)

  c_to_f <- function(c) { c * 1.8 + 32 }
  f_to_c <- function(f) { (f - 32) * 5 / 9.0 }

  temp_check <- function(temp, unit) {
    if_else(
      unit == "F",
      between(temp, 23, 86),
      between(temp, -5, 30)
    )
  }

  errors <- c()

  warn <- function(sn, msg) {
    msg <- paste(sn, "==>", msg)
    message(msg)
    errors <<- c(errors, msg)
  }

  files <- list.files(dir, "*.csv", full.names = T)
  if (length(files) == 0)
    stop("No csv files found in directory '", dir, "'")
  files <- files[order(nchar(files), files)] # sort files

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
      mutate(Temp = as.numeric(Temp)) %>%
      drop_na(Temp) %>%
      mutate(Unit = unit) %>%
      mutate(LoggerSN = as.numeric(sn), .before = 1) %>%
      mutate(DateTime = parse_date_time2(DateTime, c(
        "%Y-%m-%d %H:%M:%S",
        "%m-%d-%Y %H:%M:%S",
        "%m-%d-%y %H:%M:%S",
        "%m/%d/%y %I:%M:%S %p",
        "%m/%d/%y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y %H:%M:%S"
      ), exact = T)) %>%
      mutate(Date = as.Date(DateTime), Year = lubridate::year(Date), .after = DateTime) %>%
      mutate(TempOK = temp_check(Temp, Unit))

    print(data)

    cat(paste0(
      " => ", nrow(data), " obs\n",
      " => ", as.Date(min(data$Date)), " - ", as.Date(max(data$Date)), "\n",
      " => ", min(data$Temp), " - ", max(data$Temp), " °", unit, "\n"))

    if (length(unique(data$Year)) > 1) {
      before <- nrow(data)
      years <- paste(sort(unique(data$Year)), collapse = ", ")
      data <- filter(data, Year == yr)
      after <- nrow(data)
      warn(sn, paste("Multiple years in data range: ", years))
      warn(sn, paste("Removed", before - after, "values from the wrong year."))
    }

    if (!all(data$TempOK)) {
      before <- nrow(data)
      bad_temps <- filter(data, !TempOK)
      data <- filter(data, TempOK)
      after <- nrow(data)
      warn(sn, paste("Removed ", before - after, "temperature value(s) out of range"))
      print(bad_temps)
    }

    data %>%
      mutate(
        TempF = ifelse(Unit == "F", Temp, round(c_to_f(Temp), 2)),
        TempC = ifelse(Unit == "C", Temp, round(f_to_c(Temp), 2))) %>%
      select(-Temp, -Unit, -TempOK)
  })

  cat("\n")
  lapply(errors, function(err) message(err))

  bind_rows(raw_data) %>%
    clean_names() %>%
    filter(year == yr)
}


# cleans hobo data by trimming dates before/after deployment
# checks for temperatures out of range and issues warnings
# prints a status table summarizing all the loggers
# requires the `therm_inventory` table
clean_hobos <- function(hobodata, return_status = FALSE) {
  require(dplyr)

  n_loggers <- n_distinct(hobodata$logger_sn)
  message("Total loggers: ", n_loggers)

  before_counts <- hobodata %>%
    summarize(
      date_min = min(date),
      date_max = max(date),
      days = n_distinct(date),
      .by = logger_sn)

  cleaned <- hobodata %>%
    left_join(therm_inventory, join_by(logger_sn, year)) %>%
    mutate(
      after_deployed = if_else(!is.na(date_deployed), date > date_deployed, TRUE),
      before_removed = if_else(!is.na(date_removed), date < date_removed, TRUE)
    ) %>%
    filter(after_deployed & before_removed) %>%
    select(-c(after_deployed, before_removed)) %>%
    mutate(
      month = lubridate::month(date),
      day = lubridate::day(date),
      yday = lubridate::yday(date),
      # date_time = force_tz(date_time, tzone = "America/Chicago"),
      .after = year) %>%
    arrange(logger_sn, date_time)

  print(cleaned)

  after_counts <- cleaned %>%
    summarize(
      deployed = first(date_deployed),
      removed = first(date_removed),
      clean_days = n_distinct(date),
      station_id = first(station_id),
      latitude = as.character(first(latitude)),
      longitude = as.character(first(longitude)),
      station_name = first(station_name),
      .by = logger_sn)

  counts <- before_counts %>%
    left_join(after_counts, join_by(logger_sn)) %>%
    mutate(
      days_rm = days - clean_days,
      status = case_when(
        is.na(deployed) & is.na(removed) ~ "missing both",
        is.na(deployed) ~ "missing deployment",
        is.na(removed) ~ "missing removal",
        T ~ "OK"
      ),
      .after = clean_days
    )

  print(counts, n = 100)
  message("Total loggers after cleaning: ", n_distinct(cleaned$logger_sn))
  message("Loggers missing deployment dates: ", sum(is.na(counts$deployed)))
  message("Loggers missing removal dates: ", sum(is.na(counts$removed)))
  message("Loggers missing both dates: ", sum(counts$status == "missing both"))

  if (return_status) {
    counts
  } else {
    select(cleaned, -c(contact_name, date_deployed, date_removed))
  }
}


# plotly showing temperature readout for a logger
# adds weather data behind to compare and look for anomalies
makeThermistorPlot <- function(df_hourly, weather = NULL) {
  require(dplyr)
  require(plotly)

  title <- with(first(df_hourly), paste(
    "Year:", year, "|",
    "Logger SN:", logger_sn, "|",
    "Station ID:", station_id, "|",
    "Coords:", paste0(latitude, ", ", longitude)
  ))

  # make sure daily values time is aligned to noon
  df_daily <- df_hourly %>%
    summarize(
      min = min(temp_f),
      mean = mean(temp_f),
      max = max(temp_f),
      .by = date) %>%
    mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

  plt <- plot_ly()

  if (!is.null(weather)) {
    weather <- weather %>%
      mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
    plt <- plt %>%
      add_ribbons(
      data = weather,
      x = ~date_time,
      ymin = ~min_temp,
      ymax = ~max_temp,
      line = list(
        color = "orange",
        width = 0.5,
        opacity = 0.1),
      fillcolor = "orange",
      opacity = 0.1,
      name = "Air temperature") %>%
    add_trace(
      data = weather,
      x = ~ date_time,
      y = ~ avg_temp,
      name = "Mean air temp.",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "orange",
        opacity = .1))
  }

  plt %>%
    add_ribbons(
      data = df_daily,
      x = ~ date_time,
      ymin = ~ min,
      ymax = ~ max,
      line = list(
        color = "lightblue",
        width = 0.5,
        opacity = 0),
      fillcolor = "lightblue",
      opacity = 0.5,
      name = "Daily water temp range") %>%
    add_trace(
      data = df_hourly,
      x = ~date_time,
      y = ~temp_f,
      name = "Hourly Water temp (F)",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#1f77b4",
        width = 0.5,
        opacity = 0.8)) %>%
    add_trace(
      data = df_daily,
      x = ~ date_time,
      y = ~ mean,
      name = "Mean daily water temp.",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "blue")) %>%
    layout(
      title = title,
      showlegend = TRUE,
      xaxis = list(title = "Date and time"),
      yaxis = list(
        title = "Temperature (F)",
        range = c(32, 90),
        fixedrange = T,
        zerolinecolor = "lightgrey"),
      hovermode = "x unified",
      legend = list(
        orientation = "h",
        x = 0.25,
        y = 1),
      margin = list(
        t = 50)) %>%
    config(displayModeBar = F)
}


# cycles through hobo data and plots them each in turn
# can give one or more serial numbers to inspect, or it goes through all of them
inspect_hobos <- function(hobodata, serials = sort(unique(hobodata$logger_sn))) {
  i <- 1
  n <- length(serials)
  for (i in 1:n) {
    sn <- serials[i]
    hobo <- hobodata %>% filter(logger_sn == sn)
    url <- buildAgWxURL(hobo)
    weather <- getAgWxData(url)
    makeThermistorPlot(hobo, weather) %>% print()
    message("Logger ", i, "/", n, ": SN ", sn)
    if (n > 1) {
      resp <- readline("[Enter] for next, q to quit > ")
      if (tolower(resp) %in% c("q")) break
    }
  }
}

# create a URL to get weather data from AgWeather
buildAgWxURL <- function(df) {
  lat = df$latitude[1]
  lng = df$longitude[1]
  start_date = min(df$date)
  end_date = max(df$date)
  if (any(is.na(c(lat, lng, start_date, end_date)))) return(NULL)
  glue::glue("https://agweather.cals.wisc.edu/ag_weather/weather?lat={lat}&long={lng}&start_date={start_date}&end_date={end_date}")
}

# pulls and formats weather data from AgWeather
getAgWxData <- function(url) {
  if (is.null(url)) return(NULL)
  httr::GET(url) %>%
    httr::content() %>%
    pluck("data") %>%
    enframe() %>%
    unnest_wider("value")
}

# saves all the hobo files individually for SWIMS upload
export_hobos <- function(clean_data, logger_serials = unique(clean_data$logger_sn), out_dir = "~clean/hobodata") {
  yr <- clean_data$year[1]
  out_dir <- file.path(out_dir, yr)
  dir.create(out_dir, showWarnings = F)
  for (sn in logger_serials) {
    df <- clean_data %>%
      filter(logger_sn == sn) %>%
      mutate(across(date_time, ~format(.x, "%Y-%m-%d %H:%M:%S")))
    stn_id <- df$station_id[1]
    fname <- glue::glue("Hobo data {yr} - SN {sn} - Stn {stn_id}.csv")
    fpath <- file.path(out_dir, fname)
    message("Writing '", fpath, "'")
    write_csv(df, fpath)
  }
}


## Read and check Hobo data ----

#' File format expectations:
#' - Row 1 may be skipped, often contained 'plot title' or other heading.
#' - Obs # in column 1
#' - Date in column 2
#' - Temperature in column 3
#'   - Units must be specified in column name

# Read in raw hobo csv files
hobos2020.parsed <- parse_hobos("therm/hobodata/2020", 2020)
hobos2021.parsed <- parse_hobos("therm/hobodata/2021", 2021)
hobos2022.parsed <- parse_hobos("therm/hobodata/2022", 2022)
hobos2023.parsed <- parse_hobos("therm/hobodata/2023", 2023)

# Load thermistor inventory, matching SNs with WAV Stns
# update the inventory with correct deploy/retrieve dates
# then re-run the cleaning
therm_inventory <- read_csv("therm/combined-hobo-inventory.csv") %>%
  left_join(select(stn_list, station_id, station_name, latitude, longitude), join_by(station_id))

# clean the hobo data using the deployment dates in the inventory
hobos2020.cleaned <- clean_hobos(hobos2020.parsed)
hobos2021.cleaned <- clean_hobos(hobos2021.parsed)
hobos2022.cleaned <- clean_hobos(hobos2022.parsed)
hobos2023.cleaned <- clean_hobos(hobos2023.parsed)

# generate interactive charts to inspect the data and compare to air temperatures
# currently no way to easily trim internal dates when a logger becomes exposed to the air
# can use this to confirm deployment and retrieval dates
# update dates in the inventory file if desired, then re-run the cleaning
inspect_hobos(hobos2020.cleaned)
inspect_hobos(hobos2021.cleaned)
inspect_hobos(hobos2022.cleaned)
inspect_hobos(hobos2023.cleaned, 20211524)

# save these cleaned datasets
hobos2020.cleaned %>% write_csv("~clean/hobodata/hobos-cleaned-2020.csv.gz")
hobos2021.cleaned %>% write_csv("~clean/hobodata/hobos-cleaned-2021.csv.gz")
hobos2022.cleaned %>% write_csv("~clean/hobodata/hobos-cleaned-2022.csv.gz")
hobos2023.cleaned %>% write_csv("~clean/hobodata/hobos-cleaned-2023.csv.gz")

# export individual CSVs
export_hobos(hobos2020.cleaned)
export_hobos(hobos2021.cleaned)
export_hobos(hobos2022.cleaned)
export_hobos(hobos2023.cleaned, 20211524)


## Merge hobodata ----

hobo_data <- bind_rows(
  hobos2020.cleaned,
  hobos2021.cleaned,
  hobos2022.cleaned,
  hobos2023.cleaned
) %>% filter(!is.na(station_id))

# loggers per year
hobo_data %>%
  count(logger_sn, year) %>%
  count(year)

# Collect list of SNs by year
hobo_serials <- hobo_data %>%
  distinct(year, logger_sn) %>%
  mutate(have_data = T)

# Are we missing data for loggers in the inventory?
therm_info <- therm_inventory %>%
  left_join(hobo_serials) %>%
  replace_na(list(have_data = F)) %>%
  arrange(year)

therm_info %>%
  filter(!have_data) %>%
  {
    print(.)
    write_csv(., "therm/QC/Thermistors - Inventory entries without matching data.csv")
  }

therm_info_export <- therm_info %>%
  filter(have_data) %>%
  select(-have_data)

## Export ----

# export inventory
therm_info_export %>% write_csv("~clean/therm-inventory.csv")
hobo_data %>% write_csv("~clean/therm-data.csv")

# export data to dashboard
therm_info_export %>% saveRDS("../WAV Dashboard/data/therm-inventory")
hobo_data %>% saveRDS("../WAV Dashboard/data/therm-data")



# 6 => Finalize station list ===================================================

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
hobo_data %>% count(station_id)

# any thermistor inventory entries missing a station id?
hobo_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Stations to keep ----

keep_stns <- sort(unique(c(
  baseline_data$station_id,
  tp_data$station_id,
  hobo_data$station_id
))) %>% na.omit()


## Create station SF ----

stn_list.sf <- stn_list %>%
  filter(station_id %in% keep_stns) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, DnrRegion, CountyName)) %>%
  st_join(select(huc12, -Area)) %>%
  st_join(select(dnr_watersheds, DnrWatershedCode, DnrWatershedName)) %>%
  select(
    station_id, station_name,
    latitude, longitude,
    county_name = CountyName,
    dnr_region = DnrRegion,
    wbic, waterbody,
    huc12 = Huc12Code,
    sub_watershed = Huc12Name,
    huc10 = Huc10Code,
    watershed = Huc10Name,
    dnr_watershed_name = DnrWatershedName,
    dnr_watershed_code = DnrWatershedCode,
    huc8 = Huc8Code,
    sub_basin = Huc8Name,
    major_basin = MajorBasin,
    geometry
  ) %>%
  distinct(station_id, .keep_all = T)

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

# baseline_stns <- all_stns %>%
#   filter(station_id %in% baseline_data$station_id)

# tp_stns <- all_stns %>%
#   filter(station_id %in% tp_data$station_id)

all_stns %>% write_csv("~clean/station-list.csv")
all_stns %>% saveRDS("../WAV Dashboard/data/station-list")



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


