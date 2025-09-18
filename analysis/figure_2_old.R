library(GeoPressureR)

library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(terra)
library(geosphere)

library(viridisLite)
library(scico)
library(rnaturalearth)

invisible(sapply(
  list.files("/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/GeoMag/R", pattern = "\\.R$", full.names = TRUE),
  function(f) source(f, local = globalenv(), echo = FALSE)
))
source("./R/zzz.R", echo = FALSE)

load(file = "data/tag_mag.RData")

tag <- tags[[1]]


colors <- c(
  "#6929c4", # Purple 70
  "#1192e8", # Cyan 50
  "#005d5d", # Teal 70
  "#9f1853", # Magenta 70
  "#fa4d56", # Red 50
  "#570408", # Red 90
  "#198038", # Green 60
  "#002d9c", # Blue 80
  "#ee538b", # Magenta 50
  "#b28600", # Yellow 50
  "#009d9a", # Teal 50
  "#012749", # Cyan 90
  "#8a3800", # Orange 70
  "#a56eff"  # Purple 50
)
colors <- kelly.colors()
colors <- colors[-c(1,2,9,20,22)]

ids <- unique(tag$mag_calib$stap_id)
cols <- scico::scico(length(ids), palette = "romaO")
cols <- c(tail(cols, -length(cols) %/% 2), head(cols, length(cols) %/% 2))
cols <- setNames(cols, ids)

tag_ref <- tag
pad <- 3
tag_ref$param$tag_set_map$extent <- tag_ref$param$tag_set_map$extent + c(-pad, pad, -pad, pad)
tag_ref$param$tag_set_map$scale <- 10
map_ref<- geomag_map_ref(tag_ref)
map_ref_df <- as.data.frame(map_ref, xy = TRUE)


bbox <- list(
  min_lon = tag$param$tag_set_map$extent[1],
  max_lon = tag$param$tag_set_map$extent[2],
  min_lat = tag$param$tag_set_map$extent[3],
  max_lat = tag$param$tag_set_map$extent[4]
)

pad <- 3
world <- map_data("world")
intersecting_countries <- unique(world[
  world$long >= bbox$min_lon - pad&
    world$long <= bbox$max_lon + pad&
    world$lat >= bbox$min_lat - pad&
    world$lat <= bbox$max_lat + pad,
  "region"
])
map_data_countries <- map_data("world", region = intersecting_countries)

p_map <- ggplot() +
  geom_polygon(
    data = map_data_countries, aes(x = long, y = lat, group = group),
    fill = "#6d6d6b", color = "white", linewidth = 0.1
  ) +
  #scale_fill_manual(values = cols) +
  guides(fill = "none", size = "none") +
  coord_fixed(
    ratio = 1.2,
    xlim = c(bbox$min_lon+2, bbox$max_lon-8),
    ylim = c(bbox$min_lat+10, bbox$max_lat)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#1d1f2a", color = NA),
    plot.background = element_rect(fill = "#1d1f2a", color = NA),
    # legend.position = "none"
  )

pp <- list()
for (i in seq_len(length(tags))){
  tag <- tags[[i]]
  path <- paths[[i]]

  path$duration <- stap2duration(path)
  path_light <- tag2path(tag, "map_light", use_known = F)

  mag <- geomag_clean(tag) %>% mutate(I = I / pi * 180)

  # Find non-breeding
  n_rm = 6
  i_winter <- path %>%
    slice((n_rm + 1):(nrow(.) - n_rm)) %>%
    filter(duration == max(duration, na.rm = TRUE)) %>%
    pull(stap_id)

  # Select stap_id which are (1) longer than 2 days and separated by at least 20 hours
  stap_dur_min <- .5 # days
  flight_dur_min <- 10 # hours

  for (i_season in c(1, 2)){
    if (i_season==1){
      path_season <- path %>% filter( stap_id<=i_winter)
      flight <- stap2flight(path, include_stap_id = which(path$duration>stap_dur_min&path$stap_id<=i_winter))
    } else {
      path_season <- path %>% filter( stap_id>=i_winter)
      flight <- stap2flight(path, include_stap_id = which(path$duration>stap_dur_min&path$stap_id>=i_winter))
    }
    i_flight <- c(1)  # Always start with the first flight
    cum_dur <- 0
    for (j in 2:length(flight$duration)) {
      cum_dur <- cum_dur + flight$duration[j - 1]
      if (cum_dur >= flight_dur_min) {
        i_flight <- c(i_flight, j)
        cum_dur <- 0
      }
    }
    istap <- flight$stap_s[i_flight]

    path_f <- inner_join(
      path_season %>% filter(stap_id %in% istap) %>% select(stap_id, lon_true = lon, lat_true = lat, duration),
      path_light %>% filter(stap_id %in% istap) %>% select(stap_id, lon_light = lon, lat_light = lat),
      by = "stap_id"
    )

    cols <- colors[seq_len(length(istap))]# kelly.colors(length(istap)+3)
    cols <- setNames(cols, istap)
    path_f$color <- cols[as.character(path_f$stap_id)]

    p <- p_map
    for (u in istap) {
      p <- p + geom_contour(
        data = map_ref_df,
        aes(x = x, y = y, z = inclinaison),
        breaks = mean(mag$I[u == mag$stap_id]),
        color = cols[as.character(u)],
        linetype = "dashed",
        # color = "blue",
        linewidth = 1,
        show.legend = F
      ) + geom_contour(
        data = map_ref_df,
        aes(x = x, y = y, z = intensity),
        breaks = mean(mag$F[u == mag$stap_id]),
        linetype = "dotted",
        color = cols[as.character(u)],
        # color = "blue",
        linewidth = 1,
        show.legend = F
      )
    }

    pp[[i]] <- p +
      geom_path(data = path_season, aes(x = lon, y = lat), color = "white", linewidth = 0.5) +
      # geom_segment(data = path_f, aes( x = lon_true, y = lat_true, xend = lon_light, yend = lat_true, color = color), linewidth = 0.5      ) +
      # geom_segment(data = path_f, aes( x = lon_light, y = lat_true, xend = lon_light, yend = lat_light, color = color), linewidth = 0.5      ) +
      geom_segment(data = path_f, aes( x = lon_light, y = lat_light, xend = lon_true, yend = lat_true, color = color), linewidth = 0.25      ) +
      geom_point( data = path_f, aes(x = lon_true, y = lat_true, size = log(duration), fill = color),  color = "white", shape = 21) +
      geom_point(data = path_f, aes(x = lon_light, y = lat_light, size = log(duration), fill = color), color = "white", shape = 23) +
      scale_size_continuous(range = c(1.5, 3)) +
      scale_fill_identity() +
      scale_color_identity()
  }
}

wrap_plots(pp[1:16], nrow = 3)




postscript("figure_2_1.eps", width = 3.5, height = 3)
print(pp[[i]])
dev.off()




## Compute distance

df_results <- purrr::map2_dfr(tags, paths, function(tag, path) {
  mag <- geomag_clean(tag) %>%
    dplyr::mutate(I = I / pi * 180)

  path$duration <- stap2duration(path)

  path_light <- tag2path(tag, "map_light", use_known = F)

  n_stap <- nrow(tag$stap)

  purrr::map_dfr(seq_len(n_stap), function(istap) {

    target <- c(path$lon[istap], path$lat[istap])


    idx <- mag$stap_id == istap

    n_mag <- sum(idx, na.rm = TRUE)

    dist_mag_I <- NA_real_
    dist_mag_F <- NA_real_
    if (n_mag > 0) {
      tmp <- map_ref_df %>%
        # filter(abs(y-target[2])<.1) %>%
        dplyr::filter(abs(inclinaison - mean(mag$I[idx])) < 0.1)
      if (nrow(tmp)>1){
        dist_mag_I <-  tmp %>%
          dplyr::mutate(d = geosphere::distHaversine(cbind(x, y), target)) %>%
          dplyr::summarise(min_dist = min(d, na.rm = TRUE)/1000) %>%
          dplyr::pull(min_dist)
      }
      tmp <- map_ref_df %>%
        # filter(abs(y-target[2])<.1) %>%
        dplyr::filter(abs(intensity - mean(mag$F[idx])) < 0.0005)
      # tmp %>% ggplot() + geom_point(aes(x=x, y=y))
      if (nrow(tmp)>1){
        dist_mag_F <-  tmp %>%
          dplyr::mutate(d = geosphere::distHaversine(cbind(x, y), target)) %>%
          dplyr::summarise(min_dist = min(d, na.rm = TRUE)/1000) %>%
          dplyr::pull(min_dist)
      }
    }

    path_light_i <- c(path_light$lon[istap], path_light$lat[istap])

    tibble::tibble(
      tag_id = tag$param$id,
      stap_id = istap,
      dist_mag_I = dist_mag_I,
      dist_mag_F = dist_mag_F,
      dist_light_lat = distHaversine(c(path_light_i[1], path_light_i[2]), c(path_light_i[1], target[2])) / 1000,
      dist_light_lon = distHaversine(c(path_light_i[1], path_light_i[2]), c(target[1], path_light_i[2])) / 1000,
      n_mag = n_mag,
      duration = path$duration[istap]
    )
  })
})

df_results %>%
  summarise(
    n = n(),
    mag_I_median = median(dist_mag_I, na.rm = TRUE),
    mag_I_q90 = quantile(dist_mag_I, 0.9, na.rm = TRUE),
    mag_F_median = median(dist_mag_F, na.rm = TRUE),
    mag_F_q90 = quantile(dist_mag_F, 0.9, na.rm = TRUE),
    light_lat_median = median(dist_light_lat, na.rm = TRUE),
    light_lat_q90 = quantile(dist_light_lat, 0.9, na.rm = TRUE),
    light_lon_median = median(dist_light_lon, na.rm = TRUE),
    light_lon_q90 = quantile(dist_light_lon, 0.9, na.rm = TRUE)
  )

df_results %>%
  summarise(
    cor_mag_I = cor(dist_mag_I, duration, use = "complete.obs"),
    cor_mag_F = cor(dist_mag_F, duration, use = "complete.obs"),
    cor_light_lat = cor(dist_light_lat, duration, use = "complete.obs"),
    cor_light_lon = cor(dist_light_lon, duration, use = "complete.obs")
  )

df_results_long <- df_results %>%
  # filter(n_mag>1) %>%
  # filter(duration > 0.5) %>%
  pivot_longer(
    cols = c(dist_mag_I,dist_mag_F, dist_light_lon, dist_light_lat),
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
  mutate(dur_group = factor(dur_group, levels = c("<1d", "1–7d", "7–30d", ">30d"))) %>%
  mutate(distance=pmin(distance,2000))%>%
  mutate(distance=pmax(distance,10))

df_results_long %>% select(tag_id, stap_id, dur_group) %>% unique() %>% count(dur_group)

df_results_long %>%
  ggplot(aes(x = dist_type, y = distance, fill = dur_group)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    # alpha = 0.7,
    color = "gray80",
    size = 0.5
  ) +
  geom_jitter(
    aes(color = dur_group),
    size = 1,
    # alpha = 0.3,
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)
  ) +
  scale_y_log10(
    breaks = c(10, 50, 100, 200, 500, 1000, 2000),
    limits = c(10, 2000)
  ) +
  scale_x_discrete(labels = c(
    dist_mag_I = "Magnetic Inclination",
    dist_mag_F = "Magnetic Intensity",
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
    y = "Distance to reference (km, log scale)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.background = element_rect(fill = "#1b1e2a", color = NA),
    panel.grid.major = element_line(color = "gray40"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "gray90"),
    axis.title = element_text(color = "gray90"),
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
    legend.position = "left",
    legend.background = element_rect(fill = "#1b1e2a", color = NA),
    legend.text = element_text(color = "gray90"),
    legend.title = element_text(color = "gray90", face = "bold")
  )


df_results_long %>%
  ggplot(aes(x = log(duration), y = distance, group=dist_type, color=dist_type)) +
  geom_point() +
  geom_smooth(method = 'loess', span=.75)+#method = "lm", formula = y ~ poly(x, 2), fullrange=F) +
  # facet_wrap(~ dist_type, ncol=1) +
  theme_light() + # + guides(color = "none") +
  scale_x_continuous(
    breaks = log(c(0.25, 0.5, 1, 7, 30, 30*6)),
    labels = c("6 h", "12 h", "1 d", "7 d", "1 mo", "6 mo"),
    limits = log(c(0.5, 30*7)), expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = seq(0,1000, by=100),
    limits = c(0,1000), expand = c(0,0)
  ) +
  labs(x = "Stationary period duration (days)", y = "Distance to reference (km)",
       title = "Position Error vs. Duration") +
  scale_color_manual(values = c(
    "#0077BB", "#33BBEE", "#CC3311", "#EE7733"
  ))

ggsave("figure_2_2.eps", width = 8, height = 5)

postscript("figure_2_2.eps", width = 6, height = 3)
print(p)
dev.off()

