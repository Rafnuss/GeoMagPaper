library(GeoPressureR)

library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(terra)

library(viridisLite)
library(scico)
library(rnaturalearth)


invisible(sapply(
  list.files("/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/GeoMag/R", pattern = "\\.R$", full.names = TRUE),
  function(f) source(f, local = globalenv(), echo = FALSE)
))

load(file = "data/tag_mag.RData")

tag <- tags[[14]]
path <- paths[[14]]

## Subplot1 ----

tag$magnetic %>%
  filter(date < min(date) + months(2)) %>%
  select(date:acceleration_z) %>%
  pivot_longer(
    cols = magnetic_x:acceleration_z,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable_type = sub("_(x|y|z)$", "", variable),
    axis = sub("^(magnetic|acceleration)_", "", variable)
  ) %>%
  select(-variable) %>%
  ggplot(aes(x = date, y = value, color = axis)) +
  geom_point(linewidth = 0.3, alpha = 0.8) +
  facet_wrap(~variable_type, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  theme_bw(base_size = 10) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.title = element_blank(),
    panel.grid.minor = element_blank()
  )

tag$magnetic %>%
  head(n = 10) %>%
  select(date:acceleration_z) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


## Subplot 2 ----

xyz_rng <- c(-1, 1)
coords <- list(
  list(x = xyz_rng, y = c(0, 0), z = c(0, 0)),
  list(x = c(0, 0), y = xyz_rng, z = c(0, 0)),
  list(x = c(0, 0), y = c(0, 0), z = xyz_rng)
)
a_xis <- list(showline = F, showgrid = F, zeroline = F, showticklabels = F, title = "")

p <- plot_mag(tag, "calib")

p <- p %>% add_markers(
  x = tag$param$mag_calib$offset[1], y = tag$param$mag_calib$offset[2], z = tag$param$mag_calib$offset[3],
  marker = list(
    color = "#e74c3c",
    size = 6, # larger than default
    symbol = "x" # or "x" / "diamond"
    # line = list(width = 10, color = "white")  # thin white border
  ), showlegend = FALSE
)
p <- p %>% add_trace(
  type = "scatter3d", mode = "lines",
  x = c(0, tag$param$mag_calib$offset[1]),
  y = c(0, tag$param$mag_calib$offset[2]),
  z = c(0, tag$param$mag_calib$offset[3]),
  line = list(color = "#e74c3c", width = 4),
  showlegend = FALSE
)

for (a in coords) {
  p <- add_trace(p,
    type = "scatter3d", mode = "lines", x = a$x, y = a$y, z = a$z,
    line = list(color = "black", width = 2), showlegend = FALSE
  )
}

p <- p %>%
  layout(scene = list(xaxis = a_xis, yaxis = a_xis, zaxis = a_xis))

p


## Subplot 3 ----

mag <- geomag_clean(tag) %>%
  mutate(I = I / pi * 180) %>%
  group_by(stap_id) %>%
  mutate(
    mean_F = mean(F),
    mean_I = mean(I)
  ) %>%
  ungroup()

ids <- unique(tag$mag_calib$stap_id)
cols <- scico::scico(length(ids), palette = "romaO")
cols <- c(tail(cols, -length(cols) %/% 2), head(cols, length(cols) %/% 2))
cols <- setNames(cols, ids)

p1 <- ggplot() +
  geom_point(data = mag, aes(x = date, y = F, color = factor(stap_id)), alpha = 1, shape = 16) +
  geom_line(data = mag, aes(x = date, y = mean_F, color = factor(stap_id)), linewidth = 1, alpha = 1) +
  geom_step(data = rbind(path, tail(path, 1) %>% mutate(start = end)), aes(x = start, y = intensity), color = "#e74c3c", linewidth = 0.5, alpha = 1) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  theme_linedraw() +
  scale_x_datetime(expand = c(0, 0), name = "Date") +
  ylab("Intensity (Gauss)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.3, 0.55))

p2 <- ggplot() +
  geom_point(data = mag, aes(x = date, y = I, color = factor(stap_id)), alpha = 1, shape = 16) +
  geom_line(data = mag, aes(x = date, y = mean_I, color = factor(stap_id)), linewidth = 1, alpha = 1) +
  geom_step(data = rbind(path, tail(path, 1) %>% mutate(start = end)), aes(x = start, y = inclinaison), color = "#e74c3c", linewidth = 0.5, alpha = 1) +
  scale_color_manual(values = cols) +
  guides(color = "none") +
  theme_linedraw() +
  scale_x_datetime(expand = c(0, 0), name = "Date") +
  ylab("Inclinaison (°)") +
  scale_y_continuous(expand = c(0, 0), limits = c(-10, 80))

p1 / p2

postscript("figure_1_2.eps", width = 7, height = 5)
print(p1 / p2)
dev.off()


## Subplot 4 ----

theme_map <- theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#1d1f2a", color = NA),
    plot.background = element_rect(fill = "#1d1f2a", color = NA),
    # legend.position = "none"
  )

world <- map_data("world")
tag_ref <- tag
tag_ref$param$tag_set_map$extent <- c(-180, 180, -90, 90)
tag_ref$param$tag_set_map$scale <- 1
ref_world <- geomag_map_ref(tag_ref)
ref_world_df <- as.data.frame(ref_world, xy = TRUE)


p_intensity <- ggplot() +
  geom_polygon(
    data = world, aes(x = long, y = lat, group = group),
    fill = "#6d6d6b", color = "white", linewidth = 0.2
  ) +
  geom_contour(data = ref_world_df, aes(x = x, y = y, z = intensity, color = after_stat(level))) +
  coord_fixed(ratio = 1.5, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_color_gradientn(colors = colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(256)) +
  theme_map +
  labs(fill = "Intensity", title = "Magnetic Intensity")

p_inclination <- ggplot() +
  geom_polygon(
    data = world, aes(x = long, y = lat, group = group),
    fill = "#6d6d6b", color = "white", linewidth = 0.2
  ) +
  geom_contour(data = ref_world_df, aes(x = x, y = y, z = inclinaison, color = after_stat(level))) +
  coord_fixed(ratio = 1.5, xlim = c(-180, 180), ylim = c(-90, 90)) +
  scale_color_gradientn(colors = colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(256)) +
  theme_map +
  labs(fill = "Inclinasion", title = "Magnetic Inclinasion")

p_intensity / p_inclination


postscript("figure_1_4.eps", width = 2.5, height = 3.5)
print(p_intensity / p_inclination)
dev.off()

## Subplot 5 ----

path$duration <- stap2duration(path)
path$color <- cols[path$stap_id]

pad <- 3
bbox <- list(
  min_lon = min(path$lon, na.rm = TRUE) - pad,
  max_lon = max(path$lon, na.rm = TRUE) + pad,
  min_lat = min(path$lat, na.rm = TRUE) - pad,
  max_lat = max(path$lat, na.rm = TRUE) + pad
)

intersecting_countries <- unique(world[
  world$long >= bbox$min_lon &
    world$long <= bbox$max_lon &
    world$lat >= bbox$min_lat &
    world$lat <= bbox$max_lat,
  "region"
])
map_data_countries <- map_data("world", region = intersecting_countries)

p_map <- ggplot() +
  geom_polygon(
    data = map_data_countries, aes(x = long, y = lat, group = group),
    fill = "#6d6d6b", color = "white", linewidth = 0.1
  ) +
  scale_fill_manual(values = cols) +
  guides(fill = "none", size = "none") +
  coord_fixed(
    ratio = 1.2,
    xlim = c(bbox$min_lon, bbox$max_lon),
    ylim = c(bbox$min_lat, bbox$max_lat)
  ) +
  theme_map

postscript("figure_1_p_path.eps", width = 2.5, height = 3.5)
print(p_map)
dev.off()

# r <- rast.map(tag$map_light)
r <- rast.map(tag$map_magnetic_intensity)
df_r <- as.data.frame(r, xy = TRUE)
df_r[, 3:ncol(df_r)] <- df_r[, 3:ncol(df_r)]^5 # scale
df_r[, 3:ncol(df_r)] <- lapply(df_r[, 3:ncol(df_r)], function(x) {
  x / max(x, na.rm = TRUE)
})


p <- ggplot() + scale_alpha(range = c(0, 1))

for (istap in c(1, 6, 10, 11, 17, 27, 29, 31, 33)) {
  col_name_i <- names(df_r)[istap + 2]
  df_r_plot <- df_r[!is.na(df_r[[col_name_i]]), ]
  p <- p +
    geom_raster(
      data = df_r_plot,
      aes(x = x, y = y, alpha = .data[[col_name_i]]), fill = path$color[istap], show.legend = F
    )
}

p <- p + coord_fixed(
  ratio = 1.2,
  xlim = c(bbox$min_lon, bbox$max_lon),
  ylim = c(bbox$min_lat, bbox$max_lat)
) +
  theme_void()

p

png("figure_1_6.png", width = 2.5, height = 3.5, res = 300, units = "in", bg = "transparent")
print(p)
dev.off()
