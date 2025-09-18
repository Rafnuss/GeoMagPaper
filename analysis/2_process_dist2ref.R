library(GeoPressureR)
library(ggplot2)
library(plotly)
library(tidyverse)
library(terra)
library(cli)
library(glue)
library(GeoMag)

species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA")
dist2ref <- data.frame()
for (sp in species_list) {

  list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)

  for (id in list_id) {

    save_list <- load_interim(id)

    if (!("path_most_likely" %in% save_list)){
      print(id)
      next
    }

    path <- path_most_likely %>%
      mutate(j=0) %>%
      rbind(path_simulation) %>%
      mutate(
        tag_id = tag$param$id,
        sp = sp,
        duration = stap2duration(.)
      )

    path <- path %>% left_join(
      geomag_clean(tag) %>%
        mutate(I = I / pi * 180) %>%
        group_by(stap_id) %>%
        summarise(
          tag_F = median(F, na.rm = TRUE),
          tag_I = median(I, na.rm = TRUE),
          tag_n = n()
        ),
      by = "stap_id")


    ## Light
    map_light <- as.data.frame(rast.map(tag$map_light), xy = TRUE) %>% pivot_longer(
      cols = starts_with("#"),
      names_to = "stap_id",
      names_pattern = "#(\\d+)",  # remove the "#"
      names_transform = list(stap_id = as.integer),
      values_to = "value"
    )

    light_ml <- map_light %>%
      group_by(stap_id) %>%
      slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        invalid_lon = x==min(map_light$x) | x==max(map_light$x),
        invalid_lat = y==min(map_light$y) | y==max(map_light$y),
      ) %>% select(-value)

    path <- path %>%
      left_join(light_ml, by = "stap_id") %>%
      mutate(
        dist_light = geosphere::distHaversine(cbind(x, y), cbind(lon, lat))/1000,
        dist_light_lon = geosphere::distHaversine(cbind(x, lat), cbind(lon, lat))/1000,
        dist_light_lat = geosphere::distHaversine(cbind(lon, y), cbind(lon, lat))/1000,
      ) %>%
      mutate(
        dist_light = ifelse(invalid_lon | invalid_lat, NA_real_, dist_light),
        dist_light_lon = ifelse(invalid_lon, NA_real_, dist_light_lon),
        dist_light_lat = ifelse(invalid_lat, NA_real_, dist_light_lat)
      ) %>%
      select(-c(x, y, invalid_lat, invalid_lon))


    ## Intensity
    map_int <- as.data.frame(rast.map(tag$map_magnetic_intensity), xy = TRUE) %>%
      pivot_longer(
      cols = starts_with("#"),
      names_to = "stap_id",
      names_pattern = "#(\\d+)",  # remove the "#"
      names_transform = list(stap_id = as.integer),
      values_to = "value"
    )

    int_ml <- map_int %>%
      group_by(stap_id, x) %>%
      slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        invalid_lon = x==min(map_int$x) | x==max(map_int$x),
        invalid_lat = y==min(map_int$y) | y==max(map_int$y),
      ) %>% select(-value)

    path <- path %>%
      left_join(int_ml, by = "stap_id", relationship="many-to-many") %>%
      mutate(
        dist_int = geosphere::distHaversine(cbind(x, y), cbind(lon, lat))/1000,
      ) %>%
      group_by(stap_id, j) %>%
      slice_min(order_by = dist_int, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        dist_int = ifelse(invalid_lon | invalid_lat, NA_real_, dist_int),
      ) %>%
      select(-c(x, y, invalid_lat, invalid_lon))

    ## Inclinaison
    map_inc <- as.data.frame(rast.map(tag$map_magnetic_inclination), xy = TRUE) %>% pivot_longer(
      cols = starts_with("#"),
      names_to = "stap_id",
      names_pattern = "#(\\d+)",  # remove the "#"
      names_transform = list(stap_id = as.integer),
      values_to = "value"
    )

    inc_ml <- map_inc %>%
      group_by(stap_id, x) %>%
      slice_max(order_by = value, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        invalid_lon = x==min(map_inc$x) | x==max(map_inc$x),
        invalid_lat = y==min(map_inc$y) | y==max(map_inc$y),
      ) %>% select(-value)

    path <- path %>%
      left_join(inc_ml, by = "stap_id", relationship="many-to-many") %>%
      mutate(
        dist_inc = geosphere::distHaversine(cbind(x, y), cbind(lon, lat))/1000,
      ) %>%
      group_by(stap_id, j) %>%
      slice_min(order_by = dist_inc, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        dist_inc = ifelse(invalid_lon | invalid_lat, NA_real_, dist_inc),
      ) %>%
      select(-c(x, y, invalid_lat, invalid_lon))

    dist2ref <- rbind(dist2ref, path %>% select(
        lat,lon, start, end, duration,
        sp, tag_id, stap_id, j,
        tag_F, tag_I, tag_n,
        dist_light, dist_light_lon, dist_light_lat,
        dist_int, dist_inc
      ))
  }
}

# Add known value of inclinaison and intensity
time <- as.POSIXct(rowMeans(cbind(dist2ref$start, dist2ref$end)))
dist2ref[c("wmm_F", "wmm_I")] <- t(vapply(
  seq_len(nrow(dist2ref)),
  \(i) {
    out <- wmm::GetMagneticFieldWMM(dist2ref$lon[i], dist2ref$lat[i], 0, time[i])
    c(out$f/ 100000, out$i)
  },
  numeric(2)
))


# Write file
write_csv(dist2ref, "data/dist2ref.csv")


