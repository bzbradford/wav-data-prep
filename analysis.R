library(tidyverse)


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

