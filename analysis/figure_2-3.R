library(GeoPressureR)
library(tidyverse)
library(maps)
library(terra)
library(dplyr)
library(ggplot2)
library(scales)


dist2ref <- read_csv("data/dist2ref.csv", show_col_types = FALSE) %>%
  mutate(
    stap_id = as.integer(stap_id),
    j = as.integer(j),
  ) %>%
  filter(j==0) %>%
  # filter(n_mag>1) %>%
  # filter(duration > 0.5) %>%
  filter(if_all(everything(), ~ !is.na(.)))

dist2ref %>%
  group_by(sp) %>%
  summarise(
    n=n(),
    ntag = n_distinct(tag_id)
  )



## Subplot 1

# Get world map
world_map <- map_data("world")
lon_range <- c(-20, 120)    # west to east: covers Europe, Africa, Middle East, Asia
lat_range <- c(-40, 55)     # south to north: covers southern Africa to Estonia

p1 <- ggplot() +
  # country borders
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "#6d6d6b",
    color = "white",
    size = 0.1
  ) +
  # bird trajectories
  geom_path(
    data = dist2ref,
    aes(x = lon, y = lat, group = tag_id, color = sp),
    size = 0.8,
    #alpha = 0.7
  ) +
  geom_point(
    data = dist2ref,
    aes(x = lon, y = lat, color = sp),
    size = 1,
    #alpha = 0.5
  ) +
  # equipment site circles
  geom_point(
    data = dist2ref %>%
      filter(stap_id == 1) %>%
      distinct(sp, lat, lon, .keep_all = TRUE),
    aes(x = lon, y = lat, fill = sp),
    size = 4,
    color="white",
    shape = 21,
    stroke = 1.5
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  coord_quickmap(xlim = lon_range, ylim = lat_range) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#1d1f2a", color = NA),  # water color
    legend.position = "none"
  )

ggsave("figures/figure_2_1.eps", width = 8, height = 5)



## Subplot 2

dist2ref_long <- dist2ref %>%
  pivot_longer(
    cols = c(dist_inc,dist_int, dist_light_lon, dist_light_lat),
    names_to = "dist_type",
    values_to = "distance"
  ) %>%
  mutate(
    dur_group = case_when(
      duration < 1 ~ "<1d",
      duration >= 1 & duration < 7 ~ "1–7d",
      duration >= 7 & duration < 30 ~ "7–30d",
      duration >= 30 ~ ">30d"
    )
  ) %>%
  mutate(dur_group = factor(dur_group, levels = c("<1d", "1–7d", "7–30d", ">30d")))

# Report metric for paper
dist2ref_long %>%
  group_by(dist_type) %>%
  summarise(
    med = median(distance),
    q_low = quantile(distance, 0.25),
    q_high = quantile(distance, 0.75),
    .groups = "drop"
  )

dist2ref_long %>%
  group_by(dist_type, sp) %>%
  summarise(distance = median(distance), .groups = "drop") %>%
  pivot_wider(
    names_from = sp,
    values_from = distance
  )

wilcox.test(dist2ref$dist_inc, dist2ref$dist_int, paired = TRUE, alternative = "less")

# dist2ref_long %>% select(tag_id, stap_id, dur_group) %>% unique() %>% count(dur_group)

dist2ref_long %>%
  mutate(distance=pmin(distance,3000))%>%
  mutate(distance=pmax(distance,10)) %>%
  ggplot(aes(y = dist_type, x = distance, fill = dist_type)) +
  geom_violin(
    position = position_dodge(width = 0.8),
    # alpha = 0.7,
    color = "gray80",
    # size = 0.5
  ) +
  stat_summary(
    fun.data = function(x) {
      q <- quantile(x, c(0.25, 0.75))
      data.frame(ymin = q[1], ymax = q[2], y = median(x))
    },
    geom = "crossbar",
    fatten = 0,
    width = 0.03,
    color = NA,
    fill = "black",
    position = position_dodge(width = 0.8)
  ) +
  # Median as a large dot
  stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 3,
    color = "black",
    fill = "white",
    position = position_dodge(width = 0.8)
  ) +
  # Median text
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.0f", after_stat(x))),
    vjust = -1,  # place text above the dot
    color = "white",
    size = 3,
    position = position_dodge(width = 0.8)
  ) +
  scale_x_log10(
    breaks = c(10, 50, 100, 200, 500, 1000, 3000),
    # limits = c(10, 2000)
  ) +
  scale_y_discrete(labels = c(
    dist_inc = "Magnetic Inclination",
    dist_int = "Magnetic Intensity",
    dist_light_lat = "Light Latitude",
    dist_light_lon = "Light Longitude"
  )) +
  scale_fill_manual(
    values = c(
      dist_light_lat = "#FDB863",  # light orange
      dist_light_lon = "#E08214",  # darker orange
      dist_inc       = "#80B1D3",  # light blue
      dist_int       = "#08519C"   # dark blue
    ),
    name = "Stopover duration"
  ) +
  scale_color_manual(
    values = c(
      dist_light_lat = "#FDB863",
      dist_light_lon = "#E08214",
      dist_inc       = "#80B1D3",
      dist_int       = "#08519C"
    ),
    guide = "none"
  ) +
  labs(
    x = "Distance to reference (km, log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.grid.major.x = element_line(color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray90"),
    axis.title = element_text(color = "gray90"),
    legend.position = "none",
    legend.background = element_rect(fill = "#1b1e2a", color = NA),
    legend.text = element_text(color = "gray90"),
    legend.title = element_text(color = "gray90", face = "bold")
  )


ggsave("figures/figure_2_2.eps", width = 8, height = 5)








### subplot 3

id <- "16AQ"
load(here::here(glue::glue("data/interim/{id}.Rdata")))
istap <- 3


# Convert rasters to data frames and add type + fill color
light_df <- as.data.frame(rast.map(tag$map_light)[[istap]], xy = TRUE, na.rm = TRUE) %>%
  rename(value = 3) %>%
  mutate(
    value = scales::rescale(value, to = c(0,1)),
    type = "Light likelihood",
    fill_col = "#EF9D3C"
  )

mag_incl_df <- as.data.frame(rast.map(tag$map_magnetic_inclination)[[istap]], xy = TRUE, na.rm = TRUE) %>%
  rename(value = 3) %>%
  mutate(
    value = scales::rescale(value, to = c(0,1)),
    type = "Magnetic inclination likelihood",
    fill_col = "#80B1D3"
  )

mag_int_df <- as.data.frame(rast.map(tag$map_magnetic_intensity)[[istap]], xy = TRUE, na.rm = TRUE) %>%
  rename(value = 3) %>%
  mutate(
    value = scales::rescale(value, to = c(0,1)),
    type = "Magnetic intensity likelihood",
    fill_col = "#08519C"
  )

# Combine all
raster_df <- bind_rows(light_df, mag_incl_df, mag_int_df)

# Most likely for light: single point
ml_light <- raster_df %>%
  filter(type == "Light likelihood") %>%
  slice_max(value, n = 1, with_ties = FALSE) %>%
  select(x, y, type)

# Most likely line for magnetic rasters
ml_mag <- raster_df %>%
  filter(type %in% c("Magnetic inclination likelihood", "Magnetic intensity likelihood")) %>%
  group_by(type, x) %>%
  slice_max(value, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(x, y, type)

# Bird points for this tag/stap
points_df <- dist2ref %>% filter(tag_id == id & stap_id==istap)

dilation_km <- 1000
lat_center <- mean(points_df$lat)
lon_range <- range(points_df$lon) + c(-1,1) * dilation_km / (111 * cos(lat_center * pi/180))
lat_range <- range(points_df$lat) + c(-1,1) * dilation_km / 111

ggplot() +
  # countries
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "#6d6d6b",
    color = "white",
    size = 0.3
  ) +
  # raster likelihood maps
  # geom_raster(
  #   data = raster_df,
  #   aes(x = x, y = y, alpha = value),
  #   fill = raster_df$fill_col
  # ) +
  scale_alpha(range = c(0,1)) +
  # bird points
  geom_point(
    data = points_df,
    aes(x = lon, y = lat),
    color = "black",
    size = 3
  ) +
  geom_point(
    data = ml_light,
    aes(x = x, y = y),
    color = "white",
    size = 4,
    shape = 21,
    stroke = 1.5
  ) +
  # most likely line for magnetic
  geom_smooth(
    data = ml_mag,
    aes(x = x, y = y),
    method = "lm",
    formula = y ~ poly(x, 4),
    se = FALSE,       # no confidence interval
    color = "white",
    size = 1
  ) +
  facet_wrap(~type, ncol=1) +
  coord_quickmap(xlim = lon_range, ylim = lat_range, expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#1d1f2a", color = NA),
    legend.position = "none"
  )


ggsave("figures/figure_2_3.eps", width = 3, height = 5)

facet_vals <- unique(raster_df$type)
for (f in facet_vals) {
  p <- ggplot() +
    geom_raster(
      data = subset(raster_df, type == f),
      aes(x = x, y = y, alpha = value),
      fill = subset(raster_df, type == f)$fill_col
    ) +
    scale_alpha(range = c(0,1)) +
    coord_quickmap(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    theme_void() +   # removes axis, ticks, labels, grid
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = NA, color = NA),
      plot.background = element_rect(fill = NA, color = NA)
    )

  ggsave(
    filename = paste0("figures/figure_2_3_", f, ".png"),
    plot = p,
    width = 6, height = 6, dpi = 300, bg = "transparent"
  )
}

ggsave("figures/figure_2_3.png", width = 3, height = 5, dpi = 300, bg = "transparent")





## Supp mat.


dist2ref_long %>%
  mutate(distance=pmin(distance,3000))%>%
  mutate(distance=pmax(distance,10)) %>%
  ggplot(aes(y = dist_type, x = distance, fill = dur_group)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    # alpha = 0.7,
    color = "gray80",
    # size = 0.5
  ) +
  scale_x_log10(
    breaks = c(10, 50, 100, 200, 500, 1000, 3000),
    # limits = c(10, 2000)
  )  +
  scale_y_discrete(labels = c(
    dist_inc = "Magnetic Inclination",
    dist_int = "Magnetic Intensity",
    dist_light_lat = "Light Latitude",
    dist_light_lon = "Light Longitude"
  )) +
  scale_fill_manual(
    values = c("#B0C4DE", "#4682B4", "#27408B", "#191970"),
    name = "Stopover duration"
  ) +
  scale_color_manual(
    values = c("#B0C4DE", "#4682B4", "#27408B", "#191970"),
    guide = "none"
  ) +
  labs(
    x = "Distance to reference (km, log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.grid.major.x = element_line(color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray90"),
    axis.title = element_text(color = "gray90"),
    # legend.position = "none",
    legend.background = element_rect(fill = "#1b1e2a", color = NA),
    legend.text = element_text(color = "gray90"),
    legend.title = element_text(color = "gray90", face = "bold")
  )

ggsave("figures/suppl_mat_1.eps", width = 6, height = 3)


dist2ref_long %>%
  mutate(distance=pmin(distance,3000))%>%
  mutate(distance=pmax(distance,10)) %>%
  ggplot(aes(y = dist_type, x = distance, fill = sp)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    # alpha = 0.7,
    color = "gray80",
    # size = 0.5
  ) +
  scale_x_log10(
    breaks = c(10, 50, 100, 200, 500, 1000, 3000),
    # limits = c(10, 2000)
  )  +
  scale_y_discrete(labels = c(
    dist_inc = "Magnetic Inclination",
    dist_int = "Magnetic Intensity",
    dist_light_lat = "Light Latitude",
    dist_light_lon = "Light Longitude"
  )) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Distance to reference (km, log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.grid.major.x = element_line(color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray90"),
    axis.title = element_text(color = "gray90"),
   # legend.position = "none",
    legend.background = element_rect(fill = "#1b1e2a", color = NA),
    legend.text = element_text(color = "gray90"),
    legend.title = element_text(color = "gray90", face = "bold")
  )

ggsave("figures/suppl_mat_2.eps", width = 6, height = 3)

# Compute correlation between distance and duration for each distance type
dist2ref_long %>%
  group_by(dist_type) %>%
  summarise(
    cor = cor(distance, duration, use = "complete.obs"),
    .groups = "drop"
  )
