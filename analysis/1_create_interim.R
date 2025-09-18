library(GeoPressureR)
setwd(here::here())

source("R/geopressuretemplate_tag_mag.R")

invisible(sapply(
  list.files("/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/GeoMag/R", pattern = "\\.R$", full.names = TRUE),
  function(f) source(f, local = globalenv(), echo = FALSE)
))


species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA")

for (sp in species_list) {
job::job({
  list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)

  for (id in list_id) {
    if (FALSE){
      geopressuretemplate_tag_mag(
        id,
        config = config::get(config = id, file = glue::glue("config_{sp}.yml")),
        # file = glue::glue("/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/geomag_paper/data/interim/{id}.RData"),
        geomag_calib = list(calib_data = FALSE, calib_method = "ellipse_stap"),
        geomag_map = list(),
        geopressuretemplate = list(likelihood = c("map_pressure", "map_light", "map_magnetic"))
      )
    } else {
      load(glue::glue("data/interim_origin/{id}.RData"))

      config = config::get(config = id, file = glue::glue("config_{sp}.yml"))

      # Recompute light to include known location (compute_known: TRUE)
      tag <- do.call(geolight_map, c(
        list(tag = tag),
        config$geolight_map
      ))

      tag <- do.call(geomag_calib, c(
        list(tag = tag),
        config$geomag_calib
      ))

      tag <- do.call(geomag_map, c(
        list(tag = tag),
        config$geomag_map
      ))

      param <- tag$param

      # Save the processed tag object to a file if requested
      save(
        tag,
        param,
        file = glue::glue("./data/interim/{id}.RData")
      )

      graph <- geopressuretemplate_graph(id, config = config)
    }
  }
})
}

# Get sensor resolution
df_res <- data.frame()
for (sp in species_list) {
  list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)
  for (id in list_id) {
    load_interim(id)
    df_res <- rbind(df_res, data.frame(
      species = sp,
      id = id,
      pressure = median(diff(tag$pressure$date)),
      light = median(diff(tag$light$date)),
      acceleration = median(diff(tag$acceleration$date)),
      magnetic = median(diff(tag$magnetic$date))
    ))
  }
}

