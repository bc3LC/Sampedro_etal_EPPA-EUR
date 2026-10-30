# EPPA sector price index (EU), ensembles compared: median (+ p10-p90 band) per sector, Ref dashed.
# Standalone: only needs readxl, dplyr, tidyr, ggplot2. Reads data/eppa_outputs/<ens>/*.xlsx
# (sheet pol_EUR; Ref: ref_EUR). Rows = scenarios, columns = years, 2020 = 1.
suppressPackageStartupMessages({library(readxl); library(dplyr); library(tidyr); library(ggplot2)})
if (file.exists("config.R")) source("config.R")

eppa_dir <- if (exists("path_data")) file.path(path_data, "eppa_outputs") else "data/eppa_outputs"
out_dir  <- if (exists("path_fig"))  file.path(path_fig, "0_eppa_prices") else "outputs/figures/0_eppa_prices"
tab_dir  <- if (exists("path_tab"))  path_tab else "outputs/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE); dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

ens  <- c("2C", "1p5C", "PF", "Ref")
yrs  <- c(2020, 2025, 2030, 2035, 2040)
secs <- c(elec="Electricity", gas="Gas", coal="Coal", roil="Refined oil",   # crude oil ("oil") left out: it is not used by MEDUSA

          tran="Transport", serv="Services", eint="Energy-intensive industry",
          othr="Other industry", food="Food", crop="Crops", live="Livestock", fors="Forestry")

read_one <- function(f, e) {
  sh <- if (e == "Ref") "ref_EUR" else "pol_EUR"
  x  <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE))
  y  <- as.integer(unlist(x[1, -(1:4)]))
  m  <- as.data.frame(x[-1, -(1:4)]); m[] <- lapply(m, as.numeric); names(m) <- y
  m$scenario <- as.integer(unlist(x[-1, 4]))
  pivot_longer(m, -scenario, names_to = "year", values_to = "price") |>
    mutate(year = as.integer(year))
}

dat <- bind_rows(lapply(ens, function(e) {
  fs <- list.files(file.path(eppa_dir, e), pattern = "^.*price_.*\\.xlsx$", full.names = TRUE)
  fs <- fs[!grepl("^~\\$", basename(fs))]
  bind_rows(lapply(fs, function(f) {
    s <- sub(".*price_(\\w+?)_price.*", "\\1", basename(f))
    if (!s %in% names(secs)) return(NULL)
    read_one(f, e) |> mutate(ensemble = e, sector = s)
  }))
}))

summ <- dat |> filter(year %in% yrs) |>
  group_by(ensemble, sector, year) |>
  summarise(p10 = quantile(price, .1, na.rm = TRUE), med = median(price, na.rm = TRUE),
            p90 = quantile(price, .9, na.rm = TRUE), .groups = "drop") |>
  mutate(sector = factor(sector, names(secs), secs[names(secs)]),
         ensemble = factor(ensemble, ens))

cols <- c("2C" = "#2a78d6", "1p5C" = "#eb6834", "PF" = "#1baf7a", "Ref" = "#0b0b0b")
p <- ggplot(summ, aes(year)) +
  geom_ribbon(data = filter(summ, ensemble != "Ref"),
              aes(ymin = p10, ymax = p90, fill = ensemble), alpha = .12) +
  geom_line(data = filter(summ, ensemble != "Ref"), aes(y = med, colour = ensemble), linewidth = 1) +
  geom_line(data = filter(summ, ensemble == "Ref"), aes(y = med, colour = ensemble),
            linewidth = .7, linetype = "dashed") +
  geom_hline(yintercept = 1, linewidth = .3, linetype = "dotted") +
  facet_wrap(~sector, scales = "free_y", ncol = 4) +
  scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = yrs) +
  labs(x = NULL, y = "Price index (2020 = 1)",
       title = "EPPA sector price index, EU, 2020-2040: ensembles compared",
       subtitle = "Median across scenarios; band = 10th-90th percentile (policy ensembles); Ref dashed (median)") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(),
                                        strip.text = element_text(face = "bold", hjust = 0))
ggsave(file.path(out_dir, "eppa_prices_EUR_compare.png"), p, width = 13, height = 7.5, dpi = 200, bg = "white")
write.csv(summ, file.path(tab_dir, "diag_eppa_prices_EUR_compare.csv"), row.names = FALSE)
