library(tidyverse)
library(sf)
library(leaflet)


# how many 2021 baseline or nutrient sites were in a NKE?
baseline_sites_in_nkes <- all_stns.sf %>%
  filter(
    station_id %in% {
      baseline_data %>%
        filter(year(start_date) == 2021) %>%
        pull(station_id) %>%
        unique()
    }
  ) %>%
  st_join(nkes) %>%
  drop_na(PlanId)

tp_sites_in_nkes <- all_stns.sf %>%
  filter(
      station_id %in% {
        tp_data %>%
          filter(year == 2021) %>%
          pull(station_id) %>%
          unique()
      }
  ) %>%
  st_join(nkes) %>%
  drop_na(PlanId)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = nkes,
    weight = 1) %>%
  addCircleMarkers(
    data = baseline_sites_in_nkes,
    color = "black",
    radius = 5,
    weight = 0.5,
    opacity = 1,
    fillColor = "green",
    fillOpacity = 0.75) %>%
  addCircleMarkers(
    data = tp_sites_in_nkes,
    color = "black",
    radius = 5,
    weight = 0.5,
    opacity = 1,
    fillColor = "orange",
    fillOpacity = 0.75)




# Streamflow correction ---------------------------------------------------

baseline_joined %>%
  mutate(flow_correction = corrected_stream_flow / calculated_stream_flow) %>%
  ggplot(aes(x = corrected_stream_flow, y = calculated_stream_flow)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "lm")

hist(log10(baseline_joined$corrected_stream_flow))



lm(corrected_stream_flow ~ calculated_stream_flow, baseline_joined)



# number of volunteers ----------------------------------------------------

baseline_joined %>%
  count(year, group_desc) %>%
  write_csv("baseline group names.csv")




# Relationships -----------------------------------------------------------

names(baseline_joined)

# temperature vs transparency
baseline_joined %>%
  filter(month %in% 5:10) %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  ggplot(aes(x = water_temperature, y = transparency_average / 120)) +
  geom_density_2d_filled() +
  geom_point(color = "white", size = 0.1, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = expansion()) +
  scale_x_continuous(expand = expansion()) +
  facet_wrap(~month_name) +
  labs(
    title = "Water temperature vs transparency",
    x = "Water temperature (C)",
    y = "Water transparency (% of 120cm)"
  )

# streamflow vs transparency
baseline_joined %>%
  filter(month %in% 5:10) %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  ggplot(aes(x = corrected_stream_flow, y = transparency_average / 120)) +
  geom_density_2d_filled() +
  geom_point(color = "white", size = 0.1, alpha = 0.5, shape = 21) +
  # geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = expansion()) +
  scale_x_log10(expand = expansion()) +
  facet_wrap(~month_name) +
  labs(
    title = "Stream flow vs transparency",
    x = "Stream flow (cfs)",
    y = "Water transparency (% of 120cm)"
  )


# streamflow vs transparency
baseline_joined %>%
  filter(month %in% 5:10) %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  ggplot(aes(x = water_temperature, y = d_o)) +
  geom_density_2d_filled() +
  geom_point(color = "white", size = 0.25, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm") +
  ggpmisc::stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\", \"*")),
    formula = y ~ x,
    parse = T,
    color = "white",
    size = 4) +
  scale_y_continuous(expand = expansion()) +
  scale_x_continuous(expand = expansion()) +
  labs(
    title = "Water temperature vs dissolved oxygen",
    x = "Water temperature (C)",
    y = "Dissolved oxygen (mg/L)"
  )



# Volunteer stats ---------------------------------------------------------

baseline_joined %>%
  mutate(weekday = factor(weekdays(date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(year, weekday) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = weekday, y = count, fill = as.character(year))) +
  geom_col(position = position_dodge(width = .5), width = .5) +
  geom_text(aes(label = count), vjust = -0.25, position = position_dodge(width = .5))


# fieldwork by day and year
baseline_joined %>%
  mutate(weekday = factor(weekdays(date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(year, weekday) %>%
  summarize(count = n()) %>%
  mutate(pct_day = count / sum(count)) %>%
  ggplot(aes(x = pct_day, y = as.character(year), fill = weekday)) +
  geom_col(position = position_stack(reverse = T), width = 1, color = "white", size = 0.25) +
  geom_text(aes(label = paste0("n = ", count, "\n(", round(pct_day * 100, 1), "%)")), position = position_stack(reverse = T, vjust = 0.5)) +
  scale_x_continuous(expand = expansion()) +
  scale_y_discrete(expand = expansion()) +
  theme_classic() +
  labs(y = "Year")


baseline_joined %>%
  count(group_desc, station_id) %>%
  count(group_desc) %>%
  group_by(n) %>%
  summarize(volunteers = n()) %>%
  arrange(n) %>%
  mutate(n = fct_inorder(as.character(n))) %>%
  ggplot(aes(x = n, y = volunteers)) +
  geom_col(aes(fill = volunteers), color = "black", show.legend = F) +
  geom_text(aes(label = volunteers), vjust = -.25) +
  scale_y_sqrt() +
  labs(x = "Number of sites", y = "Number of volunteers")

baseline_joined %>%
  count(group_desc) %>%
  group_by(n) %>%
  summarize(volunteers = n()) %>%
  arrange(n) %>%
  mutate(n = fct_inorder(as.character(n))) %>%
  ggplot(aes(x = n, y = volunteers)) +
  geom_col(aes(fill = volunteers), color = "black", show.legend = F) +
  geom_text(aes(label = volunteers), vjust = -.25) +
  scale_y_sqrt() +
  labs(x = "Number of sampling events", y = "Number of volunteers")





# Number of volunteers by year --------------------------------------------


# this doesn't split groups of volunteers

baseline_joined %>%
  group_by(group_desc, year) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)


# all unique volunteer names, keeping misspellings or whatever
# if a name includes a comma (i.e. last name, first name) this will split that name unfortunately

volunteer_names <- paste(unique(baseline_joined$group_desc), collapse = ", ") %>%
  gsub(" and ", " ", .) %>%
  strsplit(", ") %>%
  .[[1]] %>%
  unique() %>%
  sort()


## All years ----

fieldwork_counts <- tibble(volunteer = volunteer_names) %>%
  group_by(volunteer) %>%
  mutate(num_fieldwork = {
    match = grepl(cur_group(), baseline_joined$group_desc)
    length(match[match == TRUE])
  }) %>%
  arrange(desc(num_fieldwork))

fieldwork_counts %>%
  mutate(n = num_fieldwork) %>%
  group_by(n) %>%
  summarize(volunteers = n()) %>%
  arrange(n) %>%
  mutate(n = fct_inorder(as.character(n))) %>%
  ggplot(aes(x = n, y = volunteers)) +
  geom_col(aes(fill = volunteers), color = "black", show.legend = F) +
  geom_text(aes(label = volunteers), vjust = -.25) +
  scale_y_sqrt() +
  labs(x = "Number of fieldwork events", y = "Number of volunteers")


## Each year ----

volunteer_fieldwork_counts <- tibble(year = 2019:2021) %>%
  group_by(year) %>%
  summarize({
    cur_year = cur_group()$year

    volunteer_list <- baseline_joined %>%
      filter(year == cur_year) %>%
      pull(group_desc)

    message(cur_year, " (", length(volunteer_list), " baseline data)")

    results <- tibble(volunteer = volunteer_names) %>%
      group_by(volunteer) %>%
      summarize(
        n = {
          match = grepl(volunteer, volunteer_list)
          length(match[match == TRUE])
        }
      )

    message(" -> ", length(results$n[results$n > 0]), " unique volunteers")

    results
  })

volunteer_fieldwork_counts %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  write_csv("baseline volunteer counts.csv")



# Number of fieldwork events ----------------------------------------------

baseline_final %>%
  group_by(year) %>%
  summarize(
    sites = n_distinct(station_id),
    events = n()
  )

tp_final %>%
  group_by(year) %>%
  summarize(
    sites = n_distinct(station_id),
    events = n()
  )




# Stream widths -----------------------------------------------------------

baseline_final$stream_width %>% summary()
names(test)
unname(test)

quantiles <- baseline_final$stream_width %>%
  quantile(probs = c(.05, .25, .5, .75, .95), na.rm = T) %>%
  {tibble(name = names(.), value = unname(.))}

baseline_final %>%
  ggplot(aes(x = stream_width)) +
  geom_histogram(fill = "#c5050c", alpha = .5) +
  geom_vline(xintercept = quantiles$value, linetype = "dashed") +
  geom_text(
    data = quantiles,
    aes(x = value, y = 0, label = name),
    angle = 90,
    hjust = -.25,
    vjust = -.5) +
  scale_x_sqrt(breaks = quantiles$value, expand = expansion(c(0, .1))) +
  scale_y_continuous(expand = expansion(c(0, .1))) +
  labs(x = "Stream Width (ft.)", y = "Number of observations") +
  theme(axis.text.x = element_text(size = 11))

baseline_final$stream_width %>%
  quantile(probs = c(.25, .5, .75, .95), na.rm = T)




# Correlation matrix ------------------------------------------------------

library(ggcorrplot)

names(baseline_final)

corr <- baseline_final %>%
  select(
    water_temperature,
    ambient_air_temp,
    d_o,
    d_o_percent_saturation,
    transparency_average,
    stream_width,
    average_stream_depth,
    stream_flow_cfs
  ) %>%
  janitor::clean_names("title") %>%
  cor(
    use = "complete.obs",
    method = "spearman")

corr %>%
  ggcorrplot(
    hc.order = T,
    outline.col = "black",
    type = "lower",
    ggtheme = ggplot2::theme_grey,
    lab = T)




# Stream flows ------------------------------------------------------------

library(tidyverse)

# normalize streamflow by log10
baseline_final$stream_flow_cfs %>%
  log10() %>%
  hist()

# streamflow blob plot
baseline_final %>%
  mutate(newdate = yday + as.Date("2022-01-01")) %>%
  ggplot(aes(x = newdate, y = stream_flow_cfs)) +
  geom_density_2d_filled() +
  geom_point(shape = 21, size = 0.5, alpha = .5, color = "black", fill = "white") +
  scale_x_date(
    limits = as.Date(c("2022-04-15", "2022-11-15")),
    breaks = "1 month",
    date_labels = "%b",
    expand = expansion()) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::label_number(),
    # labels = scales::trans_format("log10", scales::math_format(10^.x)),
    expand = expansion()) +
  labs(x = NULL, y = "Stream Flow (cfs)") +
  theme(legend.position = "none")

# streamflow median by week
baseline_final %>%
  mutate(week = ceiling(yday / 7)) %>%
  group_by(week) %>%
  summarize(
    cfs = median(stream_flow_cfs, na.rm = T),
    n = n()) %>%
  mutate(date = as.Date("2022-01-01") + (week - 1) * 7) %>%
  ggplot(aes(x = date, y = cfs)) +
  geom_col(aes(fill = cfs), color = "black") +
  geom_text(aes(label = n), vjust = -.5, size = 3) +
  stat_smooth(
    geom = "line",
    method = "loess",
    span = 0.3,
    se = F,
    color = "#c5050c",
    size = 2,
    alpha = .75) +
  scale_x_date(
    limits = as.Date("2022-01-01") + c(10, 48) * 7,
    breaks = "1 month",
    date_labels = "%b",
    expand = expansion()) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(
    x = NULL,
    y = "Median Stream Flow (cfs)",
    title = "Median stream flow by week (2019-2022)",
    caption = "Number of observations shown above each bar") +
  theme(legend.position = "none")

ggsave("analysis/Median stream flow by week.png")

# streamflow medians by any chunk size
# have to adjust the geom_smooth span size to fit the line right
days <- 14
baseline_final %>%
  mutate(week = ceiling(yday / days)) %>%
  group_by(week) %>%
  summarize(
    cfs = median(stream_flow_cfs, na.rm = T),
    n = n()) %>%
  mutate(date = as.Date("2022-01-01") + (week - 1) * days) %>%
  ggplot(aes(x = date, y = cfs)) +
  geom_col(aes(fill = cfs), color = "black") +
  geom_text(aes(label = n), vjust = -.5, size = 3) +
  stat_smooth(
    geom = "line",
    method = "loess",
    span = .5,
    se = F,
    color = "#c5050c",
    size = 2,
    alpha = .75) +
  scale_x_date(
    limits = as.Date("2022-01-01") + c(10, 48) * 7,
    breaks = "1 month",
    date_labels = "%b",
    expand = expansion()) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(
    x = NULL,
    y = "Median Stream Flow (cfs)",
    title = "Median stream flow by fornight (2019-2022)",
    caption = "Number of observations shown above each bar") +
  theme(legend.position = "none")

ggsave("analysis/Median stream flow by fortnight.png")



# Pecatonica River wav stations ------------------------------------------

# pecatonica pride

library(leaflet)

stn_coverage <- read_csv("analysis/station data coverage.csv")

stn_list.sf %>%
  st_set_geometry(NULL) %>%
  left_join(stn_coverage) %>%
  janitor::clean_names(case = "big_camel") %>%
  write_csv("analysis/2019-2022 WAV Station List.csv")

stn_list.sf %>%
  filter(huc8_name == "Pecatonica") %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = huc8) %>%
  addPolygons(data = huc10, color = "green", label = ~Huc10Name) %>%
  addMarkers(label = ~station_name)

stn_list.sf %>%
  filter(huc10_name %in% c("East Branch Pecatonica River", "Spafford Creek-Pecatonica River")) %>%
  st_set_geometry(NULL) %>%
  left_join(stn_coverage) %>%
  janitor::clean_names(case = "big_camel") %>%
  write_csv("analysis/Pecatonica sites.csv")




# 2022 Data Analysis ------------------------------------------------------

## Counties monitoried in 2022 ----

get_stns <- function(d, y = 2022) {
  d %>%
    filter(year == y) %>%
    pull(station_id) %>%
    unique() %>%
    sort()
}

stns_2022 <- list()
stns_2022$baseline <- get_stns(baseline_data)
stns_2022$nutrient <- get_stns(tp_data)
stns_2022$thermistor <- get_stns(hobo_data)
stns_2022$all <- sort(unique(unlist(stns_2022)))

lapply(stns_2022, length)

stn_list.sf %>%
  filter(station_id %in% stns_2022$all) %>%
  distinct(county_name)

stn_list.sf %>%
  filter(station_id %in% stns_2022$all) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillOpacity = 1
  )



## 2022 Sites Map ----

library(leaflet)
library(shiny)

stn_colors <- list(
  "baseline" = "green",
  "thermistor" = "purple",
  "nutrient" = "orange",
  "current" = "deepskyblue"
)

stns_2022.sf <- all_stns.sf %>%
  select(station_id:longitude, geometry) %>%
  filter(station_id %in% stns_2022$all) %>%
  mutate(
    baseline = station_id %in% stns_2022$baseline,
    nutrient = station_id %in% stns_2022$nutrient,
    thermistor = station_id %in% stns_2022$thermistor,
  )

leaflet() %>%
  fitBounds(lat1 = 42.4, lat2 = 47.1, lng1 = -92.9, lng2 = -86.8) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  # addProviderTiles(providers$CartoDB.Positron) %>%
  # addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(
    data = counties,
    label = ~ lapply(paste0("<b>", CountyNam, " County</b><br>", DnrRegion), HTML),
    fillOpacity = 0.1,
    color = "grey",
    opacity = 0.5,
    fillColor = ~ colorFactor("Dark2", counties$DnrRegion)(DnrRegion),
    weight = 1
  ) %>%
  addPolygons(
    data = nkes,
    label = ~ lapply(paste0("<b>", PlanName, "</b><br>Ends: ", EndDate, "<br>Objective: ", Objective), HTML),
    weight = 1,
    color = "blue",
    fillColor = "blue",
    fillOpacity = 0.1,
    labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
  ) %>%
  addCircleMarkers(
    data = filter(stns_2022.sf, baseline),
    radius = 4,
    color = "black",
    weight = 1,
    fillColor = stn_colors$baseline,
    fillOpacity = 0.75
  ) %>%
  addCircleMarkers(
    data = filter(stns_2022.sf, nutrient),
    radius = 4,
    color = "black",
    weight = 1,
    fillColor = stn_colors$nutrient,
    fillOpacity = 0.75
  ) %>%
  addCircleMarkers(
    data = filter(stns_2022.sf, thermistor),
    radius = 4,
    color = "black",
    weight = 1,
    fillColor = stn_colors$thermistor,
    fillOpacity = 0.75
  )


## Stations in NKEs ----

nke_union <- st_union(nkes)

nke_stns_2022.sf <- stns_2022.sf %>%
  st_join(nkes) %>%
  filter(!is.na(Objectid)) %>%
  distinct(station_id, .keep_all = T) %>%
  select(station_id:geometry, baseline, nutrient, thermistor)

stns_in_nkes_2022_long <- stns_in_nkes_2022 %>%
  pivot_longer(
    cols = c(baseline, nutrient, thermistor),
    names_to = "type", values_to = "is_station") %>%
  filter(is_station) %>%
  mutate(type = str_to_title(type)) %>%
  group_by(type) %>%
  mutate(n_stns = n()) %>%
  mutate(label = paste0(type, " (", n_stns, " stations)"))

ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = nke_union, color = "blue", fill = "steelblue", alpha = 0.5) +
  geom_sf(
    data = stns_in_nkes_2022_long,
    aes(fill = label),
    shape = 21,
    size = 2) +
  scale_fill_manual(
    name = "Station Types",
    values = c("darkgreen", "purple", "orange")
  ) +
  labs(title = paste0("2022 WAV Monitoring Stations in NKE Plan Watersheds (", nrow(nke_stns_2022.sf), " total)")) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Verdana"),
    legend.position = c(.8, .9),
    legend.box.background = element_rect(color = "black", fill = "white"),
    legend.box.margin = margin(5, 5, 5, 5))

ggsave("analysis/2022 monitoring stations in NKE plans map.png")


## 2022 Hobo summary ----

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
