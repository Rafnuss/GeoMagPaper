library(fs)
library(stringr)
library(readxl)
library(tidyverse)
library(GeoLocatoR)

gld <- read_gdl('/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/40 Database/GDL_Data.accdb', filter_col = F) %>%
  mutate(
    hw_version=as.double(sub("v", "", HardwareVersion))
  ) %>%
  filter(!is.na(OrderName) & OrderName != "") %>%
  filter(!is.na(GDL_ID) & GDL_ID != "") %>%
  GeoLocatoR:::add_gldp_soi_directory(directory_data = "/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/10 Raw data/")

file_w_mag <- dir_ls("/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/10 Raw data/", regexp = "\\.magnetic$", recurse = TRUE)

mag_data <- tibble(
  path = file_w_mag,
  magCalib = grepl("magCalib", file_w_mag),
  # Extract the matches
  OrderName = str_remove_all(str_extract(file_w_mag, "/([A-Za-z]{8}[0-9]{2})/"), "/"),
  # Remove the surrounding slashes
  GDL_ID = str_remove_all(str_remove_all(str_extract(file_w_mag, "/([0-9]{2}[A-Za-z]{2})_"), "/"), "_")
) %>%
  group_by(OrderName, GDL_ID) %>%
  summarize(
    path = path[!magCalib][1],
    magCalib = any(magCalib)
  ) %>%
  ungroup() %>%
  inner_join(
    gld,
    by = join_by(OrderName, GDL_ID)
  ) %>%
  mutate(Species = if_else(grepl("TacMel", OrderName) & is.na(Species), "Tachymarptis melba", Species)) %>%
  mutate(Species = if_else(grepl("CapEur", OrderName) & is.na(Species), "Caprimulgus europaeus", Species)) %>%
  mutate(Species = if_else(grepl("LasCin", OrderName) & is.na(Species), "Lasiurus cinereus", Species)) %>%
  mutate(Species = if_else(grepl("TaMeRo", OrderName) & is.na(Species), "Tachymarptis melba", Species))


alsw_dat <- mag_data %>%
  filter(GDL_Type=="GDL3pam") %>%
  filter(Species=="Tachymarptis melba") %>%
  # filter(hw_version==2.2) %>%
  filter(!is.na(LatitudeAttached)) %>%
  # filter(magCalib) %>%
  # filter(Species=="Merops apiaster") %>%
  filter(SiteAttached=="Sofia") %>%
   print(n=100)

# trim alpine swift data to 20 rows
alsw_dat <- alsw_dat %>%
  slice_head(n=20)


setwd("alpine_swift")
# Create the YAML content
quarto_yaml <- '
project:
  type: website

render:
  - alpine_swift.qmd

profiles:
  default:
    execute:
      enabled: false

website:
  navbar:
    left:
'
for (i in seq_len(nrow(alsw_dat))){
  quarto_yaml <- paste0(quarto_yaml,
"    - text: '", alsw_dat$GDL_ID[i], "'\n",
"      href: '", alsw_dat$GDL_ID[i], ".html'\n")
}
# Write to _quarto.yml
writeLines(quarto_yaml, "_quarto.yml")


for (i in seq(8,nrow(alsw_dat))){
  # Render the Quarto document to HTML
  tryCatch({
    quarto::quarto_render("alpine_swift.qmd",
                          output_file = glue::glue("{alsw_dat$GDL_ID[i]}.html"),
                          execute_params = list(id = alsw_dat$GDL_ID[i],
                                                directory = alsw_dat$directory[i])
    )
  }, error = function(e) {
    cli::cli_alert_warning("Failed to render {.strong {alsw_dat$GDL_ID[i]}}: {e$message}")
  })
}

setwd(here::here())
