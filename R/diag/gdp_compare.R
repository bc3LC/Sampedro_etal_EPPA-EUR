# ==============================================================================
# R/diag/gdp_compare.R
# EPPA GDP trajectories per region and ensemble (same format as eppa_price_compare.R) + a GDP outlier check.
#
# Reads : data/eppa_outputs/GDP/GDP_<Ref|P2C|P15C|PF>.xlsx   one sheet per region (ref_<REG> / pol_<REG>),
#         row 1 = years, rows 2-401 = scenarios (column 2 = run id), columns = 2010-2100, billion USD2007
# Writes: outputs/figures/0_gdp/gdp_levels_compare.png       GDP per region: median + p10-p90 band, Ref dashed
#         outputs/figures/0_gdp/gdp_relative_to_Ref.png      policy / Ref - 1, same run (paired), median + p10-p90 band
#         outputs/figures/0_outliers/gdp_outliers.png        GLB and EUR trajectories, runs with a GDP collapse in red
#         outputs/tables/diag_gdp_summary.csv, diag_gdp_flags.csv
#         data/processed/flagged_gdp.rds   runs with a GDP collapse (ensemble, run_id); load_flagged() adds them to the flagged list,
#                                          so scripts 02 / 03 / di_decomposition drop them when drop_flagged_scenarios <- TRUE
# Run from the project root. Standalone: only needs readxl, dplyr, tidyr, ggplot2.
# ==============================================================================
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork) })
if (file.exists("config.R")) source("config.R")

# ---- settings ------------------------------------------------------------------------------
gdp_dir  <- if (exists("path_data")) file.path(path_data, "eppa_outputs", "GDP") else "data/eppa_outputs/GDP"
fig_root <- if (exists("path_fig")) path_fig else "outputs/figures"
tab_dir  <- if (exists("path_tab")) path_tab else "outputs/tables"
proc_dir <- if (exists("path_proc")) path_proc else "data/processed"
files <- c("2C" = "GDP_P2C.xlsx", "1p5C" = "GDP_P15C.xlsx", "PF" = "GDP_PF.xlsx", "Ref" = "GDP_Ref.xlsx")
ens   <- names(files)
yr_max <- 2040                                 # last year shown and screened (the files go to 2100; set 2100 to show everything)
yrs   <- seq(2020, yr_max, by = 5)
yr_breaks <- if (yr_max <= 2050) yrs else seq(2020, yr_max, by = 20)
check_regions   <- c("GLB", "EUR")             # regions of the outlier figure
drop_threshold  <- -0.15                       # flag a run if GDP falls by more than 15% between two consecutive 5-year steps (GLB or EUR)
for (d in c(file.path(fig_root, c("0_gdp", "0_outliers")), tab_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- read -----------------------------------------------------------------------------------
read_sheet <- function(f, sh) {
  x <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE))
  y <- suppressWarnings(as.integer(unlist(x[1, ]))); cols <- which(!is.na(y) & y >= 2000 & y <= 2100)
  id <- suppressWarnings(as.numeric(unlist(x[, 2]))); rows <- which(!is.na(id) & id == round(id) & seq_along(id) > 1)
  m <- as.matrix(x[rows, cols]); mode(m) <- "numeric"
  data.frame(run_id = rep(as.character(id[rows]), times = length(cols)), year = rep(y[cols], each = length(rows)), gdp = as.vector(m))
}
dat <- bind_rows(lapply(ens, function(e) {
  f <- file.path(gdp_dir, files[[e]]); stopifnot("GDP file not found" = file.exists(f))
  shs <- excel_sheets(f)
  bind_rows(lapply(shs, function(sh) read_sheet(f, sh) |> mutate(ensemble = e, region = sub("^(pol|ref)_", "", sh))))
}))
message("GDP: ", n_distinct(dat$region), " regions, ", paste(sprintf("%s: %d runs", ens, sapply(ens, function(e) n_distinct(dat$run_id[dat$ensemble == e]))), collapse = ", "))
regions <- c("GLB", "EUR", setdiff(unique(dat$region), c("EUR", "GLB")))

# ---- (1) levels: median and p10-p90 band per region -----------------------------------------
summ <- dat |> filter(year %in% yrs) |> group_by(ensemble, region, year) |>
  summarise(p10 = quantile(gdp, .1, na.rm = TRUE), med = median(gdp, na.rm = TRUE), p90 = quantile(gdp, .9, na.rm = TRUE), .groups = "drop") |>
  mutate(region = factor(region, regions), ensemble = factor(ensemble, ens))
write.csv(summ, file.path(tab_dir, "diag_gdp_summary.csv"), row.names = FALSE)
cols <- c("2C" = "#2a78d6", "1p5C" = "#eb6834", "PF" = "#1baf7a", "Ref" = "#0b0b0b")
theme_g <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
lev_plot <- function(d, free = TRUE) ggplot(d, aes(year)) +
  geom_ribbon(data = filter(d, ensemble != "Ref"), aes(ymin = p10, ymax = p90, fill = ensemble), alpha = .12) +
  geom_line(data = filter(d, ensemble != "Ref"), aes(y = med, colour = ensemble), linewidth = 1) +
  geom_line(data = filter(d, ensemble == "Ref"), aes(y = med, colour = ensemble), linewidth = .7, linetype = "dashed") +
  scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = yr_breaks) + scale_y_continuous(labels = scales::comma) + labs(x = NULL, y = "GDP (billion USD2007)") + theme_g
# big global panel on top, the other regions smaller below
big_small <- function(glb, reg, title, subtitle, ncol = 6, heights = c(1.6, 2)) {
  (glb + labs(title = title, subtitle = subtitle) + theme(plot.title = element_text(face = "bold", size = 13))) /
    (reg + facet_wrap(~region, scales = "free_y", ncol = ncol)) +
    plot_layout(heights = heights, guides = "collect") & theme(legend.position = "bottom")
}
p1 <- big_small(lev_plot(filter(summ, region == "GLB")) + facet_wrap(~region) , lev_plot(filter(summ, region != "GLB")),
                "EPPA GDP, 2020-2040: ensembles compared", "Median across scenarios; band = 10th-90th percentile (policy ensembles); Ref dashed (median). Top: global; below: regions")
ggsave(file.path(fig_root, "0_gdp", "gdp_levels_compare.png"), p1, width = 15, height = 12, dpi = 200, bg = "white")

# ---- (2) policy relative to Ref (same run) ----------------------------------------------------
rel <- dat |> filter(year %in% yrs) |>
  inner_join(dat |> filter(ensemble == "Ref", year %in% yrs) |> select(run_id, region, year, ref = gdp), by = c("run_id", "region", "year")) |>
  filter(ensemble != "Ref") |> mutate(rel = 100 * (gdp / ref - 1)) |>
  group_by(ensemble, region, year) |>
  summarise(p10 = quantile(rel, .1, na.rm = TRUE), med = median(rel, na.rm = TRUE), p90 = quantile(rel, .9, na.rm = TRUE), .groups = "drop") |>
  mutate(region = factor(region, regions), ensemble = factor(ensemble, ens))
rel_plot <- function(d) ggplot(d, aes(year)) + geom_hline(yintercept = 0, linewidth = .3, linetype = "dotted") +
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = ensemble), alpha = .12) + geom_line(aes(y = med, colour = ensemble), linewidth = 1) +
  scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = yr_breaks) + labs(x = NULL, y = "GDP relative to Ref (%)") + theme_g
p2 <- big_small(rel_plot(filter(rel, region == "GLB")) + facet_wrap(~region), rel_plot(filter(rel, region != "GLB")),
                "EPPA GDP: policy ensembles relative to Ref (same run), 2020-2040", "Median across scenarios; band = 10th-90th percentile. Top: global; below: regions")
ggsave(file.path(fig_root, "0_gdp", "gdp_relative_to_Ref.png"), p2, width = 15, height = 12, dpi = 200, bg = "white")

# ---- (3) outliers: GDP collapses between consecutive 5-year steps (all years of the file) --------
steps <- dat |> filter(region %in% check_regions) |> arrange(ensemble, region, run_id, year) |>
  group_by(ensemble, region, run_id) |> mutate(change = gdp / lag(gdp) - 1) |> ungroup()
flags <- steps |> filter(!is.na(change), change < drop_threshold, year <= yr_max) |> select(ensemble, region, run_id, year, change)
write.csv(flags, file.path(tab_dir, "diag_gdp_flags.csv"), row.names = FALSE)
dir.create(proc_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(distinct(flags, ensemble, run_id) |> as.data.frame(), file.path(proc_dir, "flagged_gdp.rds"))   # read by load_flagged()
cat("\nRuns with a GDP drop below ", 100 * drop_threshold, "% between two consecutive steps (", paste(check_regions, collapse = "/"), "):\n", sep = "")
print(flags |> group_by(ensemble) |> summarise(n_runs = n_distinct(run_id), runs = paste(head(sort(unique(as.integer(run_id))), 15), collapse = ","), .groups = "drop"))
fl <- distinct(flags, ensemble, run_id) |> mutate(flag = TRUE)
tr <- dat |> filter(region %in% check_regions, year <= yr_max) |> left_join(fl, by = c("ensemble", "run_id")) |>
  mutate(flag = !is.na(flag), ensemble = factor(ensemble, ens), region = factor(region, check_regions))
p3 <- ggplot(tr, aes(year, gdp, group = run_id)) + geom_line(data = filter(tr, !flag), colour = "grey70", alpha = .4, linewidth = .3) +
  geom_line(data = filter(tr, flag), colour = "#b2182b", linewidth = .5) +
  facet_grid(region ~ ensemble, scales = "free_y") + scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = "GDP (billion USD2007)", title = "GDP trajectories: runs with a collapse in red",
       subtitle = sprintf("Red: GDP falls by more than %d%% between two consecutive 5-year steps up to %d (GLB or EUR). Grey: all other runs", round(100 * -drop_threshold), yr_max)) + theme_g
ggsave(file.path(fig_root, "0_outliers", "gdp_outliers.png"), p3, width = 13, height = 6, dpi = 200, bg = "white")
message("gdp_compare.R finished: figures in ", file.path(fig_root, "0_gdp"), " and ", file.path(fig_root, "0_outliers"))
