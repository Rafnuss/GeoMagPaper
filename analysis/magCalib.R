
library(GeoLocatoR)
library(GeoMag)
library(tidyverse)
library(GeoPressureR)
library(ggplot2)
library(htmlwidgets)
library(htmltools)
setwd(here::here())

# Function to render Quarto document for each element in the list
 list_id = c("16HV", "16QO", "20FU", "22CE", "22GV", "16KD", "16LA", "18JF", "24SH", "24SI","14TS", "18CG", "18JB", "18JF", "18JG", "18JN", "16NG", "16OH", "16SO", "16ST", "16KD", "16FY", "16HV", "16HY", "16MS", "16OS", "16PU", "16PY", "16QO", "16QR", "18CL", "20EJ", "20EX", "20FB", "20GY", "20HE", "22CE", "22EJ", "22EP", "22GV", "22HD", "24LE", "24LH", "24MU", "24MY", "24SE", "24SH", "24SI", "24SZ", "26SL", "26SN", "26SR", "26SS", "26SY", "26TA", "26TC", "26TJ", "26TK", "26TP", "26TQ", "26TR", "26TX", "26TZ", "26UD", "26UE", "26UO", "26UQ", "26US", "26UU", "26UX", "26UY", "26WH", "26XA", "16KS", "16KT", "16LA", "20CE", "16GW", "16OP", "20FU", "26VC", "26VD", "26WG", "26XE", "26YA", "26YE", "16AJ", "16BY", "16EG")

gdl <- read_gdl('/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/40 Database/GDL_Data.accdb') %>%
  filter(GDL_ID %in% list_id) %>%
  GeoLocatoR:::add_gldp_soi_directory(directory_data="/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/10 Raw data/") %>%
  filter(!grepl("KE", OrderName))

plots <- list()  # collect plots
offsets <- tibble(id = character(), type = character(),
                  x = numeric(), y = numeric(), z = numeric())

for (id in gdl$GDL_ID) {
  tag <- tag_create(id, directory = gdl %>% filter(GDL_ID == id) %>% pull(directory))
  tag_situ <- NULL
  tag_vitro <- NULL
  tag_situ <- geomag_calib(tag = tag, calib_data = FALSE)
  tag_vitro <- geomag_calib(tag = tag, calib_data = TRUE)

  offsets <- bind_rows(
    offsets,
    tibble(
      id = id,
      type = "situ",
      x = tag_situ$param$geomag_calib$offset[1],
      y = tag_situ$param$geomag_calib$offset[2],
      z = tag_situ$param$geomag_calib$offset[3]
    ),
    tibble(
      id = id,
      type = "vitro",
      x = tag_vitro$param$geomag_calib$offset[1],
      y = tag_vitro$param$geomag_calib$offset[2],
      z = tag_vitro$param$geomag_calib$offset[3]
    )
  )

  if (FALSE){
    p1=plot_mag(tag_situ, "calib")
    p2=plot_mag(tag_vitro, "calib")

    p1$x$attrs[[2]]$colors[1]="#578072"
    p1$x$attrs[[3]]$color[1]="#578072"
    p1$x$attrs[[3]]$colorscale[[1]]="#578072"
    p1$x$attrs[[3]]$colorscale[[2]]="#578072"

    p <- subplot(p1, p2, shareX = TRUE, shareY = TRUE)

    plots[[length(plots) + 1]] <- p
  }
}

#combined <- tagList(plots)  # wrap all plots at once
#saveWidget(combined, "combined_plots.html", selfcontained = TRUE)

dist_pairs <- offsets %>%
  mutate(dist = sqrt(x^2 + y^2 + z^2)) %>%
  tidyr::pivot_wider(names_from = type, values_from = c(x, y, z, dist)) %>%
  mutate(dist_situ_vitro = sqrt((x_situ - x_vitro)^2 +
                                  (y_situ - y_vitro)^2 +
                                  (z_situ - z_vitro)^2))

dist_pairs %>%
  select(id, dist_situ, dist_vitro, dist_situ_vitro) %>%
  pivot_longer(-id, names_to = "measure", values_to = "distance") %>%
  ggplot(aes(x = distance, fill = measure)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Offset distances",
       x = "Distance",
       y = "Count") +
  theme_minimal()

dist_pairs %>%
ggplot(aes(x = dist_situ_vitro)) +
  geom_histogram(bins = 100) +
  labs(title = "Distances between situ and vitro calibration",
       x = "Distance (G)",
       y = "Count") +
  theme_minimal()
