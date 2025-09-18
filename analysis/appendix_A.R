library(GeoPressureR)

library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(terra)
library(GeoMag)
library(viridisLite)
library(scico)
library(rnaturalearth)

plot_mag(tag, type = "acceleration")
plot_mag(tag, type = "acceleration_p")
plot_mag(tag, type = "magnetic")


list_id <- c("14TS", "14DM", "20OA", "22NO", "12GE", "14OI")
tt <- c("Great Reed Warbler", "Eurasian Hoopoe", "Ring Ouzel", "Woodland Kingfisher", "Alpine Swift", "European Bee-eater")
####

xyz_rng <- c(-1, 1)
coords <- list(
  list(x = xyz_rng, y = c(0, 0), z = c(0, 0)),
  list(x = c(0, 0), y = xyz_rng, z = c(0, 0)),
  list(x = c(0, 0), y = c(0, 0), z = xyz_rng)
)
a_xis <- list(showline = F, showgrid = F, zeroline = F, showticklabels = F, title = "")


tags <- list()
for (i in seq_len(length(list_id))) {
  load(here::here(glue::glue("data/interim/{list_id[i]}.RData")))
  tags[[i]] <- tag
}

## Figure
pp <- list()
for (i in seq_len(length(tags))) {
  tag <- tags[[i]]

  p <- plot_mag(tag, "calib")

  p <- p %>% add_markers(
    x = tag$param$geomag_calib$offset[1], y = tag$param$geomag_calib$offset[2], z = tag$param$geomag_calib$offset[3],
    marker = list(
      color = "#e74c3c",
      size = 6, # larger than default
      symbol = "x" # or "x" / "diamond"
      # line = list(width = 10, color = "white")  # thin white border
    ), showlegend = FALSE
  )
  p <- p %>% add_trace(
    type = "scatter3d", mode = "lines",
    x = c(0, tag$param$geomag_calib$offset[1]),
    y = c(0, tag$param$geomag_calib$offset[2]),
    z = c(0, tag$param$geomag_calib$offset[3]),
    line = list(color = "#e74c3c", width = 4),
    showlegend = FALSE
  )

  for (a in coords) {
    p <- add_trace(p,
      type = "scatter3d", mode = "lines", x = a$x, y = a$y, z = a$z,
      line = list(color = "black", width = 2), showlegend = FALSE
    )
  }

  pp[[i]] <- p %>%
    layout(scene = list(xaxis = a_xis, yaxis = a_xis, zaxis = a_xis)) %>%
    layout(title = glue::glue("{tt[i]} ({tag$param$id})"))
}


## Error summary

species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA")
error_summary <- data.frame()

for (sp in species_list) {
  list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)

  for (id in list_id) {
    save_list <- load_interim(id)

    offset <- mean(sqrt(sum(tag$param$geomag_calib$offset^2)))

    radius_shape <- tag$param$geomag_calib$radius_shape
    a <- max(radius_shape)
    b <- median(radius_shape)
    c <- min(radius_shape)

    # Eccentricity
    ecc_ab <- sqrt(1 - (b^2 / a^2))
    ecc_ac <- sqrt(1 - (c^2 / a^2))
    eccentricity <- mean(c(ecc_ab, ecc_ac))

    # Sphericity
    sphericity <- c / a

    # Collect
    error_summary <- rbind(
      error_summary,
      data.frame(
        id = id,
        sp = sp,
        offset = offset,
        sphericity = sphericity,
        eccentricity = eccentricity
      )
    )
  }
}
