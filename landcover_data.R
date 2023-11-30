# Landcover fractions for HUC 8/10/12 watersheds

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(exactextractr)
library(plotly)

# summary function
extractfn <- function(df) {
  df %>%
    rename(class = value) %>%
    group_by(.data[["huc"]]) %>%
    mutate(total_area = sum(coverage_area)) %>%
    group_by(class, .add = T) %>%
    summarise(
      area = sum(coverage_area),
      total_area = total_area[1],
      pct_area = area / total_area,
      .groups = "drop"
    )
}


# load NLCD ----

# WI NLCD
nlcd <- rast("D:/GIS/raster/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover.tif")
st_crs(nlcd)
plot(nlcd)


# HUC 8 ----

huc8_attr <- huc8 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(huc = Huc8Code, huc_name = Huc8Name) %>%
  mutate(huc_level = 8, .before = everything())

huc8_ls <- huc8 %>%
  st_transform(crs = crs(nlcd)) %>%
  mutate(huc = Huc8Code) %>%
  exact_extract(
    x = nlcd, y = .,
    include_cols = "huc",
    coverage_area = T,
    summarize_df = T,
    fun = extractfn
  )


# HUC 10 ----

huc10_attr <- huc10 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(huc = Huc10Code, huc_name = Huc10Name) %>%
  mutate(huc_level = 10, .before = everything())

huc10_ls <- huc10 %>%
  st_transform(crs = crs(nlcd)) %>%
  mutate(huc = Huc10Code) %>%
  exact_extract(
    x = nlcd, y = .,
    include_cols = "huc",
    coverage_area = T,
    summarize_df = T,
    fun = extractfn
  )


# HUC 12 ----

huc12_attr <- huc12 %>%
  st_set_geometry(NULL) %>%
  dplyr::select(huc = Huc12Code, huc_name = Huc12Name) %>%
  mutate(huc_level = 12, .before = everything())

huc12_ls <- huc12 %>%
  st_transform(crs = crs(nlcd)) %>%
  mutate(huc = Huc12Code) %>%
  exact_extract(
    x = nlcd, y = .,
    include_cols = "huc",
    coverage_area = T,
    summarize_df = T,
    fun = extractfn
  )


# Join data ----

nlcd_classes <- read_csv("land/nlcd_classes.csv") %>%
  mutate(across(where(is.character), fct_inorder)) %>%
  mutate(hex = gplots::col2hex(color))

landscape_data <- bind_rows(
  left_join(huc8_attr, huc8_ls),
  left_join(huc10_attr, huc10_ls),
  left_join(huc12_attr, huc12_ls)
) %>%
  mutate(class = case_when(class == 0 ~ 11, T ~ class))


# Export/Import ----

landscape_data %>% write_csv("_clean/landcover.csv")
# landscape_data <- read_csv("land/landcover.csv.gz")
landscape_data %>% saveRDS("../WAV Dashboard/data/landcover")
nlcd_classes %>% saveRDS("../WAV Dashboard/data/nlcd_classes")



# Exploratory plots ----

landscape_data %>%
  filter(huc_level == 8) %>%
  ggplot(aes(x = pct_area, y = paste(huc, huc_name), fill = paste(class, class_name))) +
  geom_col(width = 1) +
  scale_y_discrete(expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  scale_fill_viridis_d()

landscape_data %>%
  filter(huc_level == 10) %>%
  ggplot(aes(x = pct_area, y = paste(huc_code, huc_name), fill = paste(class, class_name))) +
  geom_col(width = 1) +
  scale_y_discrete(expand = expansion()) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  scale_fill_viridis_d() +
  theme_void()

landscape_data %>%
  filter(huc_level == 12) %>%
  filter(huc_code %in% sample(.$huc_code, 100)) %>%
  ggplot(aes(y = pct_area, x = paste(huc_code, huc_name), fill = paste(class, class_name))) +
  geom_col(width = 1) +
  scale_x_discrete(expand = expansion()) +
  scale_y_continuous(labels = scales::percent, expand = expansion()) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none")



# Exploration ----

landscape_data %>%
  group_by(huc_level, huc_code) %>%
  summarize(
    mean_area = mean(total_area),
    .groups = "drop_last") %>%
  summarize(
    mean_area = mean(mean_area),
    n = n()) %>%
  mutate(sq_km = mean_area / 1e6, sq_mi = sq_km * 0.3861)

nlcd_classes

test_landscape <- landscape_data %>%
  left_join(nlcd_classes, "class") %>%
  filter(huc_level == 10) %>%
  filter(huc == sample(.$huc, 1)) %>%
  arrange(desc(pct_area)) %>%
  droplevels()

test_landscape <- landscape_data %>%
  left_join(nlcd_classes, "class") %>%
  filter(huc_level == 12) %>%
  filter(huc == "070900020803") %>%
  arrange(desc(pct_area)) %>%
  droplevels()

test_landscape_all <- landscape_data %>%
  left_join(nlcd_classes) %>%
  filter(huc_level == 12) %>%
  summarize(across(pct_area, mean), .by = class_name:hex) %>%
  arrange(desc(pct_area)) %>%
  droplevels()

# pie chart
test_landscape_all %>%
  plot_ly() %>%
  add_trace(
    type = "pie",
    labels = ~class_name,
    values = ~pct_area,
    marker = list(
      colors = ~hex,
      line = list(color = "#ffffff", width = 1)
    ),
    textposition = "inside",
    texttemplate = "<b>%{label}</b><br>%{percent}",
    hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>",
    sort = F
  ) %>%
  layout(
    showlegend = F,
    margin = list(l = 0, r = 0),
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  )


# difference bars
test_landscape_diff <- test_landscape_all %>%
  left_join(select(test_landscape, class_name, pct_area2 = pct_area), by = "class_name") %>%
  replace_na(list(pct_area2 = 0)) %>%
  mutate(diff = pct_area2 - pct_area) %>%
  mutate(label = scales::percent(diff, .1)) %>%
  mutate(label = if_else(substr(label, 1, 1) == "-", label, paste0("+", label))) %>%
  mutate(label_pos = -1 * sign(diff) * .00001) %>%
  mutate(hovertext = paste0(
    "Current watershed: ",
    scales::percent(pct_area2, .1),
    "<br>State average: ",
    scales::percent(pct_area, .1),
    "<br>Difference: ",
    label))

test_landscape_diff %>%
  plot_ly() %>%
  add_bars(
    y = ~class_name,
    x = ~label_pos,
    marker = list(
      opacity = 0
    ),
    text = ~class_name,
    textposition = "outside",
    texttemplate = "<b>%{text}</b>",
    hoverinfo = "none"
  ) %>%
  add_bars(
    y = ~class_name,
    x = ~diff,
    text = ~label,
    marker = list(
      opacity = 0
    ),
    textposition = "outside",
    texttemplate = "<b>%{text}</b>"
  ) %>%
  add_bars(
    y = ~class_name,
    x = ~diff,
    text = ~hovertext,
    marker = list(
      color = ~hex,
      line = list(color = "#000", width = 1)
    ),
    textposition = "none",
    hovertemplate = "<b>%{y}<br></b>%{text}<extra></extra>"
  ) %>%
  layout(
    barmode = "overlay",
    xaxis = list(
      title = "Difference from state average",
      tickformat = ",.0%",
      fixedrange = T,
      range = with(test_landscape_diff, c(min(diff) * 1.25, max(diff) * 1.25)),
      zerolinewidth = 2
    ),
    yaxis = list(
      visible = F,
      fixedrange = T
    ),
    showlegend = F,
    margin = list(l = 10, r = 10),
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  ) %>%
  config(displayModeBar = F)

plotly::schema()

