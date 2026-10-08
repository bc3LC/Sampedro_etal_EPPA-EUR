# ==============================================================================
# 03_scenario_discovery.R
# Which uncertain drivers explain the largest distributional impacts?
#
# Reproduces the JP-Data-Viz (Python) scenario-discovery workflow in R:
#   1. CART tree                         (figures/tree.py)
#   2. Random-forest importance          (analysis/mappings.py)
#   3. Permutation importance            (figures/permutation.py)
#   4. Importance heatmap over groups x years  (figures/heatmaps.py,
#                                          multi_region_rf_heatmap.py)
#   5. Time-series clustering + drivers of each cluster (figures/clustering.py,
#                                          cluster_output_parcoords.py)
#   6. (optional) PRIM boxes             (not in JP-Data-Viz, common in the field)
#
# Reads : data/design/design_matrix.csv, data/processed/medusa_di_results.rds
# Writes: outputs/figures/03_*.pdf / .png, outputs/tables/03_*.csv
#
# Settings (70th-percentile target, 100 trees, depth 4, k = 3, seed 0) live in
# R/functions/sd_helpers.R, copied from the app so results are comparable.
#
# PRELIMINARY: needs a real design matrix and MEDUSA outputs to run.
# ==============================================================================

# ---- Setup -------------------------------------------------------------------
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)

if (!exists("path_design")) path_design <- file.path(root, "data", "design")
if (!exists("path_fig"))    path_fig    <- file.path(root, "outputs", "figures")
if (!exists("path_tab"))    path_tab    <- file.path(root, "outputs", "tables")
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(rpart.plot)
  library(GGally)
})

# ---- Load data ---------------------------------------------------------------
# Design matrix: run_id + one column per varying parameter. Use syntactic column
# names (no spaces), e.g. renew_cost, cdr_avail, biomass, nuclear, efficiency, fossil.
design  <- read.csv(file.path(path_design, "design_matrix.csv"))
results <- readRDS(file.path(path_proc, "medusa_di_results.rds"))
if (!"region" %in% names(results)) results$region <- "EUR"

# Make sure run ids have the same type in both tables (EPPA workbooks store them as text)
design$run_id  <- as.character(design$run_id)
results$run_id <- as.character(results$run_id)

# Optional readable labels for plots (edit to your parameters)
param_labels <- c(
  renew_cost = "Renewable technology costs",
  cdr_avail  = "CDR cost / availability",
  biomass    = "Biomass constraint",
  nuclear    = "Nuclear availability",
  efficiency = "Energy efficiency improvements",
  fossil     = "Fossil fuel availability"
)
lab <- function(x) ifelse(x %in% names(param_labels), unname(param_labels[x]), x)

# ---- Selections (edit) -------------------------------------------------------
sel_region    <- "EUR"
sel_indicator <- unique(results$indicator)[1]
sel_year      <- 2050
sel_group     <- sort(unique(results$group))[1]     # e.g. lowest income decile
sel_groups    <- sort(unique(results$group))
sel_years     <- sort(unique(results$year))

outcome_for <- function(group, year, indicator = sel_indicator, region = sel_region) {
  results |>
    filter(region == !!region, indicator == !!indicator, group == !!group, year == !!year) |>
    select(run_id, value)
}

# ---- 0. Sanity check of the sampled design -------------------------------------
stopifnot("run_id" %in% names(design))
message("Runs in design: ", nrow(design), " | parameters: ", ncol(design) - 1)

d <- build_sd_table(design, outcome_for(sel_group, sel_year))
message("Runs used: ", nrow(d), " | 'high impact' runs (> P", SD_THRESHOLD, "): ", sum(d$target))

# ==============================================================================
# 1. CART tree: combinations of parameter ranges that lead to high-impact runs
# ==============================================================================
cart <- fit_cart(d)

pdf(file.path(path_fig, "03_cart_tree.pdf"), width = 9, height = 6)
rpart.plot(cart, type = 4, extra = 104, under = TRUE, fallen.leaves = TRUE,
           box.palette = "Blues", main = paste0("CART: drivers of high impacts (", sel_group, ", ", sel_year, ")"))
dev.off()
png(file.path(path_fig, "03_cart_tree.png"), width = 9, height = 6, units = "in", res = 300)
rpart.plot(cart, type = 4, extra = 104, under = TRUE, fallen.leaves = TRUE,
           box.palette = "Blues", main = paste0("CART: drivers of high impacts (", sel_group, ", ", sel_year, ")"))
dev.off()

# ==============================================================================
# 2. Random-forest importance (impurity), top inputs
# ==============================================================================
rf     <- fit_rf(d)
imp_rf <- rf_importance(rf, top_n = NULL) |> mutate(label = lab(input))
write.csv(imp_rf, file.path(path_tab, "03_rf_importance.csv"), row.names = FALSE)

p_rf <- ggplot(imp_rf, aes(importance, fct_reorder(label, importance))) +
  geom_col(fill = "#0072B2") +
  labs(title = "Random-forest importance of the varying parameters",
       subtitle = paste0("Target: ", sel_indicator, " above P", SD_THRESHOLD, " (", sel_group, ", ", sel_year, ")"),
       x = "Mean decrease in impurity (normalised)", y = NULL) +
  theme_paper()
save_fig(p_rf, "03_rf_importance", width = 7, height = 4)

# ==============================================================================
# 3. Permutation importance (as in the app: 10 repeats, keep mean - 3 sd > 0)
# ==============================================================================
imp_perm <- perm_importance(d, keep_significant = FALSE) |>
  mutate(label = lab(input), significant = (mean - 3 * sd) > 0)
write.csv(imp_perm, file.path(path_tab, "03_permutation_importance.csv"), row.names = FALSE)

p_perm <- ggplot(imp_perm, aes(mean, fct_reorder(label, mean), fill = significant)) +
  geom_col() +
  geom_errorbar(aes(xmin = mean - sd, xmax = mean + sd), width = 0.25) +
  scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey75"),
                    labels = c(`TRUE` = "mean - 3 sd > 0", `FALSE` = "not significant"), name = NULL) +
  labs(title = "Permutation importance",
       x = "Drop in accuracy when the parameter is shuffled", y = NULL) +
  theme_paper()
save_fig(p_perm, "03_permutation_importance", width = 7, height = 4)

# ==============================================================================
# 4. Importance heatmap: parameters x years, one panel per group
#    (JP-Data-Viz: regional / multi-region RF heatmaps, with groups as panels)
# ==============================================================================
grid <- expand.grid(group = sel_groups, year = sel_years, stringsAsFactors = FALSE)

heat <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid$group[i]; y <- grid$year[i]
  dd <- build_sd_table(design, outcome_for(g, y))
  if (nrow(dd) < 30 || length(unique(dd$target)) < 2) return(NULL)
  rf_importance(fit_rf(dd), top_n = NULL) |> mutate(group = g, year = y)
}))

top_inputs <- heat |> group_by(input) |> summarise(total = sum(importance), .groups = "drop") |>
  slice_max(total, n = 6) |> pull(input)

p_heat <- heat |>
  filter(input %in% top_inputs) |>
  mutate(label = lab(input)) |>
  ggplot(aes(factor(year), fct_reorder(label, importance, .fun = sum), fill = importance)) +
  geom_tile(colour = "white") +
  facet_wrap(~ group) +
  scale_fill_importance(name = "RF importance") +
  labs(title = "Which drivers matter, for whom and when",
       subtitle = paste0(sel_indicator, " - ", sel_region, "; top drivers by summed importance"),
       x = NULL, y = NULL) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_heat, "03_importance_heatmap", width = 10, height = 6)
write.csv(heat, file.path(path_tab, "03_importance_by_group_year.csv"), row.names = FALSE)

# ==============================================================================
# 5. Trajectory clustering + drivers of each cluster
# ==============================================================================
traj <- results |>
  filter(region == sel_region, indicator == sel_indicator, group == sel_group) |>
  select(run_id, year, value)

K <- 3
clusters <- cluster_runs(traj, k = K)

traj_c <- traj |> inner_join(clusters, by = "run_id") |> mutate(cluster = factor(cluster))
centres <- traj_c |> group_by(cluster, year) |> summarise(value = mean(value), .groups = "drop")

p_clu <- ggplot() +
  geom_line(data = traj_c, aes(year, value, group = run_id, colour = cluster), linewidth = 0.2, alpha = 0.3) +
  geom_line(data = centres, aes(year, value, colour = cluster), linewidth = 1.3) +
  scale_colour_manual(values = pal_cluster, name = "Cluster") +
  labs(title = "Clusters of impact trajectories", subtitle = paste0(sel_indicator, ", ", sel_group),
       x = NULL, y = sel_indicator) +
  theme_paper()
save_fig(p_clu, "03_clusters_timeseries", width = 7, height = 4.5)

cl_imp <- bind_rows(lapply(seq_len(K), function(k) cluster_importance(design, clusters, k, top_n = NULL))) |>
  mutate(label = lab(input), cluster = factor(cluster))
write.csv(cl_imp, file.path(path_tab, "03_cluster_importance.csv"), row.names = FALSE)

p_clu_imp <- ggplot(cl_imp, aes(importance, fct_reorder(label, importance), fill = cluster)) +
  geom_col() +
  facet_wrap(~ cluster, nrow = 1) +
  scale_fill_manual(values = pal_cluster, guide = "none") +
  labs(title = "Drivers of cluster membership (one-vs-rest random forest)", x = "Importance", y = NULL) +
  theme_paper()
save_fig(p_clu_imp, "03_cluster_drivers", width = 10, height = 4)

# Parallel coordinates: parameter values of the runs in each cluster
top_cl <- cl_imp |> group_by(input) |> summarise(t = sum(importance)) |> slice_max(t, n = 5) |> pull(input)
pc <- design |> inner_join(clusters, by = "run_id") |> mutate(cluster = factor(cluster))
p_pc <- ggparcoord(pc, columns = match(top_cl, names(pc)), groupColumn = "cluster",
                   scale = "uniminmax", alphaLines = 0.35) +
  scale_colour_manual(values = pal_cluster, name = "Cluster") +
  scale_x_discrete(labels = lab(top_cl)) +
  labs(title = "Parameter values by cluster", x = NULL, y = "Scaled value (0-1)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
save_fig(p_pc, "03_cluster_parcoords", width = 9, height = 4.5)

# ==============================================================================
# 6. (Optional) PRIM: boxes in parameter space with high share of high-impact runs
#    Not part of the app; widely used in scenario discovery. Check ?prim::prim.box
#    for the exact arguments before running.
# ==============================================================================
# library(prim)
# X <- d[, input_cols(d)]
# box <- prim.box(x = X, y = d$target, threshold = 0.8, threshold.type = 1)
# summary(box)
# plot(box)

message("03_scenario_discovery.R finished: figures in ", path_fig, ", tables in ", path_tab)
