library(GeoPressureR)
library(GeoLocatoR)
library(ggplot2)
library(plotly)
library(tidyverse)
library(terra)
library(patchwork)
library(rnaturalearth)
library(GeoMag)
source("./R/graph_create_flight.R", echo = FALSE)


list_id <- c("14OI", "12GE") # bee-eater, alpine swift


# Process file
for (id in list_id) {
  tag <- tag_create(
    id,
    crop_start = config::get("tag_create", id)$crop_start,
    crop_end = config::get("tag_create", id)$crop_end
  )

  tag <- twilight_create(tag)

  if (FALSE){
     twilight_label_write()
     twilight_label_read() %>%
     geomag_calib()

     tag <- geomag_calib(tag)

    p1 <- plot(tag, "pressure")
    p2 <- plot(tag, "twilight", plot_plotly = T)
    p3 <- plot(tag, "actogram", plot_plotly = T)
    p4 <- ggplotly(
      tag$magnetic %>%
        filter(is_static) %>%
        ggplot(aes(x = date, y = I / pi * 180)) +
        geom_line(color = "grey") +
        theme_bw() +
        scale_y_continuous(name = "Inclination (°)"),
      dynamicTicks = TRUE
    )
    subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleY = TRUE)

     geolightviz(tag)

  }


  ### Define staps
  known <- config::get("tag_set_map", id)$known
  known$known_lon <- as.numeric(lapply(known$known_lon, function(x) if (is.null(x)) NA else x))
  known$known_lat <- as.numeric(lapply(known$known_lat, function(x) if (is.null(x)) NA else x))
  known <- as.data.frame(known)

  stap <- known
  stap$start <- as.POSIXct(stap$start, tz = "UTC")
  stap$end <- as.POSIXct(stap$end, tz = "UTC")

  days <- unlist(
    lapply(seq_len(nrow(stap) - 1), function(i) {
      seq(from = stap$end[i], to = stap$start[i + 1], by = "1 day")
    })
  )
  stap <- rbind(
    stap[c(1, nrow(stap)), ],
    data.frame(
      stap_id = NA_integer_,
      start = head(days, -1),
      end = tail(days, -1),
      known_lon = NA_real_,
      known_lat = NA_real_
    )
  )
  stap <- stap[order(stap$start), ]
  rownames(stap) <- NULL
  stap$stap_id <- seq_len(nrow(stap))

  tag$stap <- stap

  for (sensor_df in c("pressure", "acceleration", "light", "twilight", "magnetic")) {
    if (assertthat::has_name(tag, sensor_df)) {
      assertthat::assert_that(is.data.frame(tag[[sensor_df]]))
      if ("date" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$date
      } else if ("twilight" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$twilight
      } else {
        cli::cli_abort(c("{.field {sensor_df}} needs to have a column {.field date} or \\
                         {.field twilight}."))
      }
      tag[[sensor_df]]$stap_id <- GeoPressureR:::find_stap(tag$stap, date)
    }
  }

  ### Compute likelihood map

  tag <- tag_set_map(tag,
    extent = config::get("tag_set_map", id)$extent,
    scale = config::get("tag_set_map", id)$scale,
    known = tag$stap[!is.na(tag$stap$known_lat), ]
  )

  tag <- tag %>%
    twilight_create() %>%
    #  twilight_label_write()
    twilight_label_read() %>%
    geolight_map()

  tag <- tag %>%
    geomag_calib() %>%
    geomag_map(
      sd_e_f = 0.009,
      sd_e_i = 2.6,
      sd_m_f = 0.014,
      sd_m_i = 3.5
    )

  # Save
  save(tag, file = here::here(glue::glue("data/interim/{id}.RData")))
}











### Analyse/plot
world <- map_data("world")

id <- "14OI" # bee-eater
id <- "12GE" # alpine swift

load(here::here(glue::glue("data/interim/{id}.RData")))

paths <- imap_dfr(
  list(
    "Light"      = tag2path(tag, "map_light"),
    "Intensity"  = tag2path(tag, "map_magnetic_intensity"),
    "Inclination"= tag2path(tag, "map_magnetic_inclination"),
    # path_light_I = tag2path(tag, c("map_light", "map_magnetic_inclination")),
    # path_light_F = tag2path(tag, c("map_light", "map_magnetic_intensity")),
    "Combined"   = tag2path(tag, c("map_light", "map_magnetic_inclination", "map_magnetic_intensity"))
  ),
  ~mutate(.x, duration = stap2duration(.x), type = .y)
) %>%
  mutate(
    type = factor(type, levels = c("Light", "Intensity", "Inclination", "Combined"))
  )

bbox <- list(
  min_lon = tag$param$tag_set_map$extent[1],
  max_lon = tag$param$tag_set_map$extent[2],
  min_lat = tag$param$tag_set_map$extent[3],
  max_lat = tag$param$tag_set_map$extent[4]
)

pad <- 3
intersecting_countries <- unique(world[
  world$long >= bbox$min_lon - pad&
    world$long <= bbox$max_lon + pad&
    world$lat >= bbox$min_lat - pad&
    world$lat <= bbox$max_lat + pad,
  "region"
])

p_map <- ggplot() +
  geom_polygon(
    data = map_data("world", region = intersecting_countries), aes(x = long, y = lat, group = group),
    fill = "#6d6d6b", color = "white", linewidth = 0.1
  ) +
  #scale_fill_manual(values = cols) +
  guides(fill = "none", size = "none") +
  coord_fixed(
    ratio = 1.2,
    xlim = c(bbox$min_lon, bbox$max_lon),
    ylim = c(bbox$min_lat, bbox$max_lat)
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

p_map +
  geom_path(
    data = paths,
    aes(x = lon, y = lat, group = type), # Use id if available, else just group
    color = "white", linewidth = 0.5
  ) +
  geom_point(
    data = paths,
    aes(x = lon, y = lat, size = log(duration)),
    color = "white", shape = 21, fill="white"
  ) +
  facet_wrap(~type, nrow = 1) +
  theme(
    strip.background = element_rect(fill = "#1d1f2a", color = NA),
    strip.text = element_text(color = "white")
  )


ggsave(here::here(glue::glue("figure_3_{id}.eps")), width = 10, height = 6)





# Uncertainty map

istap = 6

f_x = \(x) as.data.frame(rast.map(tag2map(tag, x))[[istap]], xy=TRUE)

maps <- imap_dfr(
  list(
    Light       = f_x("map_light"),
    Intensity   = f_x("map_magnetic_intensity"),
    Inclination = f_x("map_magnetic_inclination"),
    Combined    = f_x(c("map_light", "map_magnetic_inclination", "map_magnetic_intensity"))
  ),
  ~ tibble(
    x     = .x[[1]],
    y     = .x[[2]],
    value = .x[[3]],   # take 3rd column safely
    type  = .y
  )
) %>%
  mutate(
    type  = factor(type, levels = c("Light", "Intensity", "Inclination", "Combined")),
    value = (value - min(value, na.rm = TRUE)) /
      (max(value, na.rm = TRUE) - min(value, na.rm = TRUE)),
    .by   = type
  )

p_map +
  geom_raster(
    data = maps,
    aes(x = x, y = y, alpha = value, fill = type), , show.legend = F
  ) +
  facet_wrap(~type, nrow = 1) +
  scale_fill_manual(
    values = c(
      Light       = "#EF9D3C",
      Intensity   = "#0E64AA",
      Inclination = "#81B1D3",
      Combined    = "#8E6BBE"
    )
  ) +
  scale_alpha(range = c(0, 1)) +
  theme(
    strip.background = element_rect(fill = "#1d1f2a", color = NA),
    strip.text = element_text(color = "white")
  )


plots <- lapply(split(maps, maps$type), function(df) {
  ggplot(df) +
    geom_raster(aes(x = x, y = y, alpha = value, fill = type), show.legend = FALSE) +
    scale_fill_manual(values = c(
      Light       = "#EF9D3C",
      Intensity   = "#0E64AA",
      Inclination = "#81B1D3",
      Combined    = "#8E6BBE"
    )) +
    scale_alpha(range = c(0, 1)) +
    theme_void()
})

# Export each plot
names(plots) <- unique(maps$type)
for (nm in names(plots)) {
  ggsave(
    filename = paste0("figures/figure_4_2_", nm, ".png"),
    plot = plots[[nm]],
    width = 2.5, height = 3.5, dpi = 300, bg = "transparent"
  )
}
















  ### Movement model NOT USED
for (id in list_id) {
  load(here::here(glue::glue("data/interim/{id}.RData")))

  daily_act <- tag$acceleration %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    summarise(next_flight_act = sum(value, na.rm = TRUE)) %>%
    mutate(is_migration = day %in% as.Date(tag$stap$start))


  # create stacked histogram
  daily_act %>%  ggplot() +
    geom_histogram(aes(x = next_flight_act, fill = is_migration), position = "stack")
  # geom_point(aes(x = day, y = next_flight_act, color = is_migration))


  stap_flight <- tag$stap %>%
    mutate(
      day = as.Date(tag$stap$end)
    ) %>%
    merge(daily_act, by = "day")

  # No flight at the end
  stap_flight$next_flight_act[nrow(stap_flight)] <- NA

  basic_rate = 500
  tmp <- pmax(stap_flight$next_flight_act-basic_rate, 0)
  stap_flight$dur <- tmp/sum(tmp, na.rm=T) * 6500 / 50 # 6500km at 50km/h
  stap_flight$dur <- pmax(stap_flight$dur, 0.25) # min of 30min

  path_light_IF$dur <- stap_flight$dur

  path_light_IF %>%
    ggplot(aes(x=lon, y = lat, color = dur)) +
    geom_path()+
    geom_point(aes(size=dur)) +
    # scale size so that 0 is not visible
    scale_size_continuous(range = c(0.1, 10))


  ggplot(daily_act, aes(x = day, y = daily_accel_sum)) +
    geom_line() +
    labs(title = "Daily Acceleration Sum", x = "Day", y = "Acceleration Sum") +
    theme_minimal()

  plot(tag,"actogram")


  graph <- graph_create_flight(tag,
                               likelihood = c("map_light", "map_magnetic_intensity", "map_magnetic_inclination"),
                               flight_duration = head(stap_flight$dur,-1),
                               thr_gs = 150,
  )

  graph <- graph_set_movement(graph,
                              method = "gamma",
                              shape = 7,
                              scale = 8,
                              low_speed_fix = 20,
                              zero_speed_ratio = 4)

  plot_graph_movement(graph)

  path_most_likely <- graph_most_likely(graph)

  plot_path(path_most_likely)
}
