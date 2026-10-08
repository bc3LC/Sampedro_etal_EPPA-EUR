# ==============================================================================
# 02_figures_results.R
# Distribution results and other non-scenario-discovery figures
#
# Reads : data/processed/medusa_di_results.rds   (from 01_run_medusa.R)
#         data/design/design_matrix.csv
# Writes: outputs/figures/02_*.pdf / .png
#
# Mirrors the "exploration" figures of JP-Data-Viz:
#   time series of the ensemble, histograms / distributions, input-output scatter.
#
# PRELIMINARY: edit the schema block and the selections to match the real
# MEDUSA output.
# ==============================================================================

# ---- Setup -------------------------------------------------------------------
source(here::here("config.R"))                       # paths and global settings
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)

if (!exists("path_design")) path_design <- file.path(root, "data", "design")
if (!exists("path_fig"))    path_fig    <- file.path(root, "outputs", "figures")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(scales)
})

# ---- Load data ---------------------------------------------------------------
results <- readRDS(file.path(path_proc, "medusa_di_results.rds"))
design  <- read.csv(file.path(path_design, "design_matrix.csv"), check.names = FALSE)

# Make sure run ids have the same type in both tables (EPPA workbooks store them as text)
design$run_id  <- as.character(design$run_id)
results$run_id <- as.character(results$run_id)

# Expected long format (rename here if MEDUSA's columns are called differently):
#   run_id | region | year | group | indicator | value
# e.g. group = income decile or household type; indicator = % welfare change,
# energy burden, energy-poverty share, ...
# results <- results |> rename(run_id = scenario, group = decile)   # <- example

stopifnot(all(c("run_id", "year", "group", "indicator", "value") %in% names(results)))
if (!"region" %in% names(results)) results$region <- "EUR"

# ---- Selections (edit) ---------------------------------------------------------
sel_region    <- "EUR"
sel_indicator <- unique(results$indicator)[1]
sel_year      <- 2050
sel_groups    <- sort(unique(results$group))

res <- results |> filter(region == sel_region, indicator == sel_indicator)

# ==============================================================================
# Fig 1. Ensemble time series  (JP-Data-Viz: output time series)
#   One thin line per run, median and 10-90 % band highlighted.
# ==============================================================================
ts_summary <- res |>
  group_by(group, year) |>
  summarise(p10 = quantile(value, 0.10), median = median(value),
            p90 = quantile(value, 0.90), .groups = "drop")

p_ts <- ggplot() +
  geom_line(data = res, aes(year, value, group = run_id), colour = "grey70",
            linewidth = 0.2, alpha = 0.35) +
  geom_ribbon(data = ts_summary, aes(year, ymin = p10, ymax = p90), fill = "#0072B2", alpha = 0.25) +
  geom_line(data = ts_summary, aes(year, median), colour = "#0072B2", linewidth = 0.9) +
  facet_wrap(~ group) +
  labs(title = "Distributional impacts across the scenario ensemble",
       subtitle = paste0(sel_indicator, " - ", sel_region),
       x = NULL, y = sel_indicator,
       caption = "Grey: individual runs. Blue: median and 10-90th percentile range.") +
  theme_paper()

save_fig(p_ts, "02_timeseries_ensemble", width = 9, height = 6)

# ==============================================================================
# Fig 2. Distribution of impacts by group in one year  (JP-Data-Viz: histograms)
# ==============================================================================
res_y <- res |> filter(year == sel_year)

p_box <- ggplot(res_y, aes(x = fct_inorder(as.character(group)), y = value)) +
  geom_boxplot(fill = "#56B4E9", alpha = 0.7, outlier.size = 0.8) +
  labs(title = paste0("Impact distribution by group in ", sel_year),
       x = "Household group", y = sel_indicator) +
  theme_paper()

save_fig(p_box, "02_distribution_by_group", width = 7, height = 4.5)

p_hist <- ggplot(res_y, aes(value)) +
  geom_histogram(bins = 30, fill = "#0072B2", colour = "white", alpha = 0.85) +
  facet_wrap(~ group, scales = "free_y") +
  labs(title = paste0("Spread of outcomes across runs, ", sel_year),
       x = sel_indicator, y = "Number of runs") +
  theme_paper()

save_fig(p_hist, "02_histograms_by_group", width = 9, height = 6)

# ==============================================================================
# Fig 3. Outcome vs. each varying parameter  (quick input-output look)
#   Uses one group (the most affected, edit) to keep the figure readable.
# ==============================================================================
sel_group <- sel_groups[1]

io <- design |>
  inner_join(res_y |> filter(group == sel_group) |> select(run_id, value), by = "run_id") |>
  pivot_longer(-c(run_id, value), names_to = "parameter", values_to = "parameter_value")

p_io <- ggplot(io, aes(parameter_value, value)) +
  geom_point(alpha = 0.35, size = 0.9, colour = "grey30") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D55E00") +
  facet_wrap(~ parameter, scales = "free_x") +
  labs(title = paste0("Outcome vs. varying parameters (group ", sel_group, ", ", sel_year, ")"),
       x = "Parameter value", y = sel_indicator) +
  theme_paper()

save_fig(p_io, "02_outcome_vs_parameters", width = 9, height = 6)

# ==============================================================================
# Fig 4. Platform results (scenariodiscovery.mit.edu)
#   Placeholder: once you have the files or tables you want to show next to the
#   MEDUSA results (e.g. EPPA prices, emissions, energy mix), read them here and
#   add one block per figure, using the same helpers (theme_paper, save_fig).
# ==============================================================================
# platform <- read.csv(file.path(root, "data", "external", "platform_results.csv"))

message("02_figures_results.R finished: figures saved to ", path_fig)
