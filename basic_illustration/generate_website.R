# =========================================
# Load required libraries
# =========================================
library(ggplot2)
library(plotly)
library(terra)
library(tidyverse)
library(GeoPressureR)
library(glue)
library(fs)
library(here)
library(yaml)
library(config)

# =========================================
# Set working directory to project root
# =========================================
setwd(here::here())

# =========================================
# Generate list of tag IDs for each species
# =========================================
species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA")
list_id <- list()  # initialize

for (sp in species_list){
  config_file <- glue("config_{sp}.yml")

  # Read YAML and get all tag IDs (skip first key if needed)
  tags <- tail(names(yaml::yaml.load_file(config_file, eval.expr = FALSE)), -1)

  list_id[[config_file]] <- tags
}

# =========================================
# Move to Quarto project folder
# =========================================
setwd("basic_illustration")

# =========================================
# Build _quarto.yml for website navbar
# =========================================
navbar_left <- list()

for (i in seq_along(list_id)) {
  # Extract scientific name from config
  sp_name <- config::get("bird_create", file = names(list_id)[i])$scientific_name
  ids <- list_id[[i]]

  # Create menu items for all tag IDs of this species
  menu_items <- lapply(ids, function(id) {
    list(text = id, href = paste0(id, ".html"))
  })

  # Add species entry to navbar
  navbar_left[[i]] <- list(
    text = sp_name,
    menu = menu_items
  )
}

quarto_yaml <- list(
  project = list(type = "website"),
  website = list(navbar = list(left = navbar_left))
)

# Write the _quarto.yml **once**
yaml::write_yaml(quarto_yaml, "_quarto.yml")

# =========================================
# Generate QMD files from template
# =========================================
template_file <- "basic_illustration.qmd"  # path to template
dir_create("qmd_pages")                    # output folder for generated QMDs

for (i in seq_along(list_id)) {
  sp_name <- config::get("bird_create", file = names(list_id)[i])$scientific_name
  ids <- list_id[[i]]

  for (id in ids) {
    # Read template QMD
    qmd_lines <- readLines(template_file)

    # Update YAML params dynamically
    qmd_lines <- gsub('id: ".*"', glue('id: "{id}"'), qmd_lines)
    qmd_lines <- gsub('sp: ".*"', glue('sp: "{sp_name}"'), qmd_lines)

    # Update the title
    qmd_lines <- gsub(
      "title: .*",
      glue('title: "`r glue::glue(\'{sp_name} ({id})\')`"'),
      qmd_lines
    )

    # Write new QMD in qmd_pages folder
    writeLines(qmd_lines, path(paste0(id, ".qmd")))
  }
}

# =========================================
# Render the full website **once**
# =========================================
# Important: Quarto will now build all pages listed in _quarto.yml
quarto::quarto_render(".")

# =========================================
# Return to project root
# =========================================
setwd(here::here())
