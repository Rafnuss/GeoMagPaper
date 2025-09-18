library(GeoPressureR)
library(tidyverse)
library(GeoMag)


dist2ref <- read_csv("data/dist2ref.csv", show_col_types = FALSE) %>%
  mutate(
    stap_id = as.integer(stap_id),
    j = as.integer(j),
  ) %>%
  filter(j==0)
  # filter(if_all(everything(), ~ !is.na(.)))

dist2ref %>%
  mutate(
    err_I = tag_I - wmm_I,
    err_F = tag_F - wmm_F
  ) %>%
  group_by(sp) %>%
  filter(tag_n > 1) %>%
  filter(
    abs(err_I - mean(err_I, na.rm = TRUE)) < 3 * sd(err_I, na.rm = TRUE) & abs(err_F - mean(err_F, na.rm = TRUE)) < 3 * sd(err_F, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_histogram(aes(x=err_I), bins=50) +
  facet_wrap(~sp, scales = "free_y")


mags <- data.frame()
species_list <- c("EUHO", "WOKI", "RIOU", "MONI", "GRWA" )
for (sp in species_list) {
    list_id <- tail(names(yaml::yaml.load_file(glue::glue("config_{sp}.yml"), eval.expr = FALSE)), -1)
    for (id in list_id) {
      load(here::here(glue::glue("data/interim/{id}.RData")))

      path <- dist2ref %>%
        filter(j==0) %>%
        filter(tag_id == id) %>%
        select(stap_id, wmm_I, wmm_F, sp, tag_id)

      mag <- GeoMag::geomag_clean(tag) %>%
        mutate(I = I / pi * 180) %>%
        select(date, F, I, stap_id) %>%
        left_join(path, by = "stap_id") %>%
        mutate(
          err_F = F - wmm_F,
          err_I = I - wmm_I,
          tag_id = tag$param$id
        )
      mags <- rbind(mags, mag)
    }
}

p1 <- mags %>%
  ggplot(aes(x = date)) +
  facet_wrap(~paste(sp, tag_id, sep="-"), scales = "free") +
  geom_point(aes(y = F)) +
  geom_line(aes(y = wmm_F), color = "red") +
  theme_minimal()
ggsave("figures/err_IF_intensity.png", p1, dpi = 300, width = 10, height = 6)

# Plot 2
p2 <- mags %>%
  ggplot(aes(x = date)) +
  facet_wrap(~paste(sp, tag_id, sep="-"), scales = "free") +
  geom_point(aes(y = I)) +
  geom_line(aes(y = wmm_I), color = "red") +
  theme_minimal()
ggsave("figures/err_IF_I_inclinaison.png", p2, dpi = 300, width = 10, height = 6)

# Plot 3
p3 <- mags %>%
  ggplot(aes(x = date)) +
  facet_wrap(~paste(sp, tag_id, sep="-"), scales = "free_x") +
  geom_point(aes(y = err_F)) +
  geom_line(aes(y = 0), color = "red") +
  theme_minimal()
ggsave("figures/err_IF_err_F.png", p3, dpi = 300, width = 10, height = 6)

# Plot 4
p4 <- mags %>%
  ggplot(aes(x = date)) +
  facet_wrap(~paste(sp, tag_id, sep="-"), scales = "free_x") +
  geom_point(aes(y = err_I)) +
  geom_line(aes(y = 0), color = "red") +
  theme_minimal()
ggsave("figures/err_IF_err_I.png", p4, dpi = 300, width = 10, height = 6)


plot_err <- function(x1) {
  mu1 <- mean(x1, na.rm = TRUE)
  sd1 <- sd(x1, na.rm = TRUE)
  hist(x1, prob = TRUE, col = "grey80", border = "black", breaks=30)
  curve(dnorm(x, mean = mu1, sd = sd1), col = "red", lwd = 2, add = TRUE)
  mtext(sprintf("μ = %.4f°, σ = %.4f°", mu1, sd1), side = 3, line = -1, adj = 1, cex = 0.8)
}

par(mfrow = c(1, 2))
plot_err(mags$err_I)
plot_err(mags$err_F)

mags_stap <- mags %>%
  filter(!is.na(err_F) & !is.na(err_I)) %>%
  filter(
    abs(err_I - mean(err_I, na.rm = TRUE)) < 3 * sd(err_I, na.rm = TRUE) &
      abs(err_F - mean(err_F, na.rm = TRUE)) < 3 * sd(err_F, na.rm = TRUE)
  ) %>%
  group_by(stap_id, tag_id, sp) %>%
  summarise(
    err_I_mean_stap = mean(err_I, na.rm = TRUE),
    err_F_mean_stap = mean(err_F, na.rm = TRUE),
    err_I_var_stap = var(err_I, na.rm = TRUE),
    err_F_var_stap = var(err_F, na.rm = TRUE),
    weight = n() -1,
    .groups = "drop"
  )

plot_err(mags_stap$err_I_mean_stap[mags_stap$sp=="EUHO"])
plot_err(mags_stap$err_F_mean_stap[mags_stap$sp=="EUHO"])


mags_stap %>%
  filter(weight>5) %>%
  filter(
    abs(err_I_var_stap - mean(err_I_var_stap, na.rm = TRUE)) < 3 * sd(err_I_var_stap, na.rm = TRUE) &
      abs(err_F_var_stap - mean(err_F_var_stap, na.rm = TRUE)) < 3 * sd(err_F_var_stap, na.rm = TRUE)
  ) %>%
  # group_by(sp) %>%
  summarise(
    sd_m_i = sd(err_I_mean_stap, na.rm = TRUE),
    sd_m_f = sd(err_F_mean_stap, na.rm = TRUE),
    sd_e_i = sqrt(sum(weight * err_I_var_stap, na.rm = TRUE) / sum(weight, na.rm = TRUE)),
    sd_e_f = sqrt(sum(weight * err_F_var_stap, na.rm = TRUE) / sum(weight, na.rm = TRUE))
  )
