library(GeoPressureR)
library(leaflet)

library(GeoMag)


species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA")


for (sp in species_list) {

  list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)

  for (id in list_id) {

    load(glue::glue("data/interim_origin/{id}.RData"))

    pml <- path_most_likely
    eml <- edge_most_likely

    load_interim(id)

    # Update stap_id as it was not build correctly before
    tag$magnetic$stap_id <- GeoPressureR:::find_stap(tag$stap, tag$magnetic$date)

    # Only during flight
    magf <- tag$magnetic[tag$magnetic$stap_id != round(tag$magnetic$stap_id),]

    # Compute lat lon
    magf$lat <- stats::approx(pml$stap_id, pml$lat, magf$stap_id, rule = 1)$y
    magf$lon <- stats::approx(pml$stap_id, pml$lon, magf$stap_id, rule = 1)$y

    # Compute declination
    height <- -(288.15 / -0.0065) * (1 - ((stats::median(tag$pressure$value) / 1013.25)^(1 / 5.2561)))
    magf$D <- mapply(function(lon, lat, date) {
      wmm::GetMagneticFieldWMM(lon, lat, height, date)$d # deg
    }, magf$lon, magf$lat, magf$date)


    magf$H <- atan2(magf$magnetic_y, magf$magnetic_x) - pi
    # magf$H <- atan2(magf$magnetic_xc, magf$magnetic_yc)
    #magf$H <- atan2(magf$magnetic_ycp, magf$magnetic_xcp)



    # Magnetic heading as bearing
    magf$bearing_magnetic <- (magf$H %% (2*pi)) * 180/pi # convert radian to bearin 0 North, clockwise

    # Convert to geographic heading
    magf$bearing <- (magf$bearing_magnetic + magf$D) %% 360


    ggplot(magf, aes(x = date, y = bearing)) + geom_point()


    # plot
    m <- plot_path(pml)


    dest <- geosphere::destPoint(p = cbind(magf$lon, magf$lat),
                                 b = magf$bearing,  # degrees, clockwise from North
                                 d = rep(100000, nrow(magf)))         # meters

    magf$lon_end <- dest[, "lon"]
    magf$lat_end <- dest[, "lat"]

    ggplot(magf, aes(x = date, y = lat_end-lat)) + geom_point()

    for (i in seq_len(nrow(magf))) {
      m <- addPolylines(
        m,
        lng = c(magf$lon[i], magf$lon_end[i]),
        lat = c(magf$lat[i], magf$lat_end[i]),
        color = "red",
        weight = 5
      )
    }


    # compute line segment endpoints
    eml <- eml %>%
      mutate(
        lat_start = (lat_s + lat_t)/2,
        lon_start = (lon_s + lon_t)/2,
        # displacement in km
        dx = Re(ws) * duration,  # km east
        dy = Im(ws) * duration,  # km north
        # convert km → degrees
        lat_end = lat_start + dy / 111,
        lon_end = lon_start + dx / (111 * cos(lat_start * pi/180))
      )

    for (i in seq_len(nrow(eml))) {
      m <- addPolylines(
        m,
        lng = c(eml$lon_start[i], eml$lon_end[i]),
        lat = c(eml$lat_start[i], eml$lat_end[i]),
        color = "blue",
        weight = 5
      )
    }

    m


  }
}
