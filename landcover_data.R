# Landcover fractions for HUC 8/10/12 watersheds

library(tidyverse)
library(sf)
library(raster)
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
nlcd <- raster("D:/GIS/raster/NLCD_WI/wi_nlcd_raw.tif")
crs(nlcd)
plot(nlcd)

# CONUS NLCD
nlcd <- raster("D:/GIS/raster/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
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
    x = nlcd,
    y = .,
    include_cols = "huc_code",
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
    x = nlcd,
    y = .,
    include_cols = "huc_code",
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
    x = nlcd,
    y = .,
    include_cols = "huc_code",
    coverage_area = T,
    summarize_df = T,
    fun = extractfn
  )


# Join data ----

nlcd_classes <- read_csv("land/nlcd_classes.csv") %>%
  mutate(across(where(is.character), fct_inorder)) %>%
  mutate(hex = gplots::col2hex(color))

landscape_data <- bind_rows(
  {huc8_attr %>% left_join(huc8_ls)},
  {huc10_attr %>% left_join(huc10_ls)},
  {huc12_attr %>% left_join(huc12_ls)}
) %>%
  mutate(class = case_when(class == 0 ~ 11, T ~ class))


# Export/Import ----

landscape_data %>% write_csv("land/landcover.csv.gz")
landscape_data %>% write_csv("../Dashboard/data/landcover.csv.gz")
nlcd_classes %>% write_csv("../Dashboard/data/nlcd_classes.csv")

# landscape_data <- read_csv("land/landcover.csv.gz")



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


df <- landscape_data %>%
  filter(pct_area > 0) %>%
  filter(huc == sample(.$huc)) %>%
  arrange(desc(pct_area)) %>%
  droplevels()

df %>%
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
