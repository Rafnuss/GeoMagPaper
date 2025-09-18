library(GeoLocatoR)
library(GeoPressureR)
library(tidyverse)

# Woodland Kingfisher
pkg1 <- read_gldp("https://zenodo.org/records/15259676/files/datapackage.json")

# Hoopoes
pkg2 <- read_gldp("https://zenodo.org/records/15260024/files/datapackage.json")

# Ring Ouzel
pkg3 <- read_gldp("https://zenodo.org/records/15259763/files/datapackage.json")

pkg0 <- merge_gldp(pkg1, pkg2)
pkg0 <- merge_gldp(pkg0, pkg3)

tag_keep <- measurements(pkg0) %>%
  filter(sensor=="magnetic_x") %>%
  pull(tag_id) %>% unique()

pkg_keep <- pkg0
tags(pkg_keep) <- tags(pkg_keep) %>% filter(tag_id %in% tag_keep)
observations(pkg_keep) <- observations(pkg_keep) %>% filter(tag_id %in% tag_keep)
measurements(pkg_keep) <- measurements(pkg_keep) %>% filter(tag_id %in% tag_keep)
twilights(pkg_keep) <- twilights(pkg_keep) %>% filter(tag_id %in% tag_keep)
staps(pkg_keep) <- staps(pkg_keep) %>% filter(tag_id %in% tag_keep)
paths(pkg_keep) <- paths(pkg_keep) %>% filter(tag_id %in% tag_keep)
edges(pkg_keep) <- edges(pkg_keep) %>% filter(tag_id %in% tag_keep)
pressurepaths(pkg_keep) <- pressurepaths(pkg_keep) %>% filter(tag_id %in% tag_keep)

tmp_dir <- tempfile()  # Generate a temporary file path
dir.create(tmp_dir)
project_dir <- create_geopressuretemplate(tmp_dir, pkg_keep, open=F)

# Copy mannually the content of data into current project
system2("open", project_dir)

# Copy config
# Only done once. Now parameters have changed!
# download.file("https://raw.githubusercontent.com/Rafnuss/WoodlandKingfisher/refs/heads/main/config.yml", destfile = "config_WOKI.yml", mode = "wb")
# download.file("https://raw.githubusercontent.com/Rafnuss/SwissHoopoe/refs/heads/main/config.yml", destfile = "config_EUHO.yml", mode = "wb")
# download.file("https://raw.githubusercontent.com/Rafnuss/migration-route-of-swiss-ring-ouzels/refs/heads/main/config.yml", destfile = "config_RIOU.yml", mode = "wb")

# Import interim manually in interim_origin


## Unpublished

Great Reed Warbler


