# ==============================================================================
# 03_scenario_discovery.R
# Which EPPA uncertainties / prices drive the worst household outcomes?  (R port of JP-Data-Viz)
#
# Blocks (switch each on/off in section 0):
#   A  Input-output mapping : EPPA uncertain inputs -> "worst 30% of runs" (RF, CART, permutation importance)
#   B  Regional heatmap     : RF importance of inputs over years (panels: indicator x ensemble)
#   C  Output-output mapping: EPPA sector prices (and optional extra outputs) -> "worst 30% of runs"
#   D  Time-series clustering: k-means of 2020-2040 trajectories, binary RF per cluster, outputs by cluster
#
# Reads : data/processed/medusa_long.rds (written by 02), the EPPA design file (path_inputs), EPPA price files
# Writes: outputs/tables/03_*.csv, outputs/figures/03_* (subfolders per block)
# ==============================================================================
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

# ---- 0. Settings ---------------------------------------------------------------------
run_A <- TRUE; run_B <- TRUE; run_C <- TRUE; run_D <- TRUE

# path_inputs (EPPA design file: 'Run #' + one column per uncertain input) is defined in config.R
sc_region   <- "EUR"          # region used to filter TFP / Pop / AEEI columns of the design file
sc_year     <- 2030           # main year for blocks A, C and the cluster outputs
sc_years    <- years_calc     # years of the heatmap (2020 is the base: no variance)
adverse_pct <- 70             # class 1 = worst (100 - 70)% of runs
n_trees     <- 100; rf_depth <- 4; cart_depth <- 4; top_n <- 5; rf_seed <- 0
k_clusters  <- 3; cl_indicator <- "di"; cl_highlight <- 1   # cluster 1 = most adverse
sc_ens      <- ensembles  # ensembles analysed (use setdiff(ensembles, "Ref") to skip the reference)
ind_lab <- c(di = "Distributional impact", ep = "Energy poverty", tp = "Transport poverty")
cls_lab <- function(ind) c("Other runs", paste0("Worst ", 100 - adverse_pct, "% (", tolower(ind_lab[[ind]]), ")"))
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)

# ---- MEDUSA outcomes (from 02) and design file ------------------------------------------
res <- readRDS(file.path(path_proc, "medusa_long.rds")) |>
  filter(year <= max(years_calc)) |> drop_flagged() |> mutate(run_id = as.character(run_id))
stopifnot("add path_inputs to config.R" = exists("path_inputs"), file.exists(path_inputs))
inputs <- sd_read_inputs(path_inputs, sc_region)
message("Design file: ", nrow(inputs), " runs, ", ncol(inputs) - 1, " inputs (region ", sc_region, ")")

# outcome vector at one year (run_id, value) and aligned [inputs | target] table
outcome_at <- function(e, ind, yr) res |> filter(ensemble == e, indicator == ind, year == yr) |> select(run_id, value)
aligned <- function(e, ind, yr) {
  o <- outcome_at(e, ind, yr)
  d <- inner_join(o, inputs, by = "run_id") |> filter(!is.na(value))
  list(X = d[, setdiff(names(d), c("run_id", "value")), drop = FALSE], value = d$value, run_id = d$run_id,
       y = sd_target(ind, d$value, adverse_pct))
}

# ==============================================================================
# A. Input-output mapping at sc_year
# ==============================================================================
if (run_A) {
  imp_A <- list(); cart_A <- list()
  for (e in sc_ens) for (ind in names(ind_lab)) {
    a <- aligned(e, ind, sc_year); if (nrow(a$X) < 50 || sum(a$y) < 5) next
    imp  <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
    perm <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed, perm = TRUE)
    cart <- sd_cart(a$X, a$y, cart_depth)
    imp_A[[paste(e, ind)]]  <- data.frame(ensemble = e, indicator = ind, year = sc_year, input = names(imp),
                                          importance = as.numeric(imp), perm_importance = as.numeric(perm[names(imp)]))
    cart_A[[paste(e, ind)]] <- cbind(ensemble = e, indicator = ind, year = sc_year, cart$leaves)
    p <- sd_plot_parcoords(a$X, a$y, a$value, imp, ind_lab[[ind]], cls_lab(ind), top_n,
                           title = sprintf("%s, %s, %d: input-output mapping (n = %d runs)", ind_lab[[ind]], e, sc_year, nrow(a$X)))
    save_fig(p, sprintf("03_io_%s_%s_%d", e, ind, sc_year), sub = "03_input_output", width = 11, height = 4.8)
    png(file.path(path_fig, sprintf("03_input_output/03_cart_%s_%s_%d.png", e, ind, sc_year)), 1800, 1000, res = 200)
    plot(partykit::as.party(cart$fit), main = sprintf("CART: %s, %s, %d (class 1 = worst runs)", ind_lab[[ind]], e, sc_year))
    dev.off()
  }
  write.csv(bind_rows(imp_A),  file.path(path_tab, "03_importance_inputs.csv"), row.names = FALSE)
  write.csv(bind_rows(cart_A), file.path(path_tab, "03_cart_leaves.csv"), row.names = FALSE)
}

# ==============================================================================
# B. Heatmap: input importance over time (top inputs by summed importance in each panel)
# ==============================================================================
if (run_B) {
  hb <- bind_rows(lapply(sc_ens, function(e) bind_rows(lapply(names(ind_lab), function(ind)
    bind_rows(lapply(sc_years[sc_years > 2020], function(yr) {
      a <- aligned(e, ind, yr); if (nrow(a$X) < 50 || sum(a$y) < 5) return(NULL)
      imp <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
      data.frame(ensemble = e, indicator = ind, year = yr, input = names(imp), importance = as.numeric(imp))
    }))))))
  write.csv(hb, file.path(path_tab, "03_importance_inputs_by_year.csv"), row.names = FALSE)
  # rows: union over ensembles of each ensemble's top inputs (so every panel of a row shows the same inputs)
  top <- hb |> group_by(ensemble, indicator, input) |> summarise(s = sum(importance), .groups = "drop") |>
    group_by(ensemble, indicator) |> slice_max(s, n = top_n, with_ties = FALSE) |> ungroup() |> distinct(indicator, input)
  hp <- inner_join(hb, top, by = c("indicator", "input")) |>
    mutate(ensemble = factor(ensemble, sc_ens), indicator = factor(indicator, names(ind_lab), ind_lab))
  p <- ggplot(hp, aes(factor(year), input, fill = importance)) + geom_tile(colour = "white") +
    facet_grid(indicator ~ ensemble, scales = "free_y") + scale_fill_importance(name = "Avg. feature\nimportance") +
    labs(x = NULL, y = NULL, title = sprintf("RF feature importance of EPPA inputs, %s", sc_region)) +
    theme_paper(base_size = 9) + theme(panel.grid = element_blank(), legend.position = "right")
  save_fig(p, "03_heatmap_inputs", sub = "03_heatmaps", width = 12, height = 8.5)
}

# ==============================================================================
# C. Output-output mapping: EPPA sector prices at sc_year -> worst runs
#    (to add other outputs, e.g. GDP or carbon price, join them on run_id in `price_wide`)
# ==============================================================================
if (run_C) {
  imp_C <- list()
  for (e in sc_ens) {
    pw <- sd_read_eppa_prices(e, years = sc_year) |> mutate(sector = sd_eppa_secs[sector]) |>
      select(-year) |> pivot_wider(names_from = sector, values_from = price) |> mutate(run_id = as.character(run_id))
    pw <- drop_flagged(mutate(pw, ensemble = e))
    for (ind in names(ind_lab)) {
      d <- inner_join(outcome_at(e, ind, sc_year), pw, by = "run_id") |> filter(!is.na(value))
      X <- d[, intersect(sd_eppa_secs, names(d)), drop = FALSE]; y <- sd_target(ind, d$value, adverse_pct)
      if (nrow(X) < 50 || sum(y) < 5) next
      imp <- sd_rf(X, y, n_trees, rf_depth, rf_seed)
      imp_C[[paste(e, ind)]] <- data.frame(ensemble = e, indicator = ind, year = sc_year, output = names(imp), importance = as.numeric(imp))
      p <- sd_plot_parcoords(X, y, d$value, imp, ind_lab[[ind]], cls_lab(ind), top_n,
                             title = sprintf("%s, %s, %d: output-output mapping (EPPA price index)", ind_lab[[ind]], e, sc_year),
                             fill = "#6929c4")
      save_fig(p, sprintf("03_oo_%s_%s_%d", e, ind, sc_year), sub = "03_output_output", width = 11, height = 4.8)
    }
  }
  write.csv(bind_rows(imp_C), file.path(path_tab, "03_importance_outputs.csv"), row.names = FALSE)
}

# ==============================================================================
# D. Time-series clustering of cl_indicator (2020-2040), binary RF for cluster `cl_highlight`
# ==============================================================================
if (run_D) {
  ind <- cl_indicator; cl_all <- list(); imp_D <- list()
  for (e in sc_ens) {
    W <- res |> filter(ensemble == e, indicator == ind) |> select(run_id, year, value) |>
      pivot_wider(names_from = year, values_from = value) |> filter(complete.cases(pick(everything())))
    M <- as.matrix(W[, -1]); rownames(M) <- W$run_id
    cl <- sd_cluster(M, ind, k_clusters, rf_seed)
    yrs <- as.integer(colnames(M))
    cl_all[[e]] <- data.frame(ensemble = e, indicator = ind, run_id = names(cl$cluster), cluster = as.integer(cl$cluster))
    # trajectories coloured by cluster, centres in bold
    long <- data.frame(run_id = rep(rownames(M), ncol(M)), year = rep(yrs, each = nrow(M)), value = as.vector(M),
                       cluster = factor(rep(cl$cluster, ncol(M)), 1:k_clusters, paste("Cluster", 1:k_clusters)))
    cen <- data.frame(year = rep(yrs, each = k_clusters), value = as.vector(cl$centers),
                      cluster = factor(rep(1:k_clusters, ncol(M)), 1:k_clusters, paste("Cluster", 1:k_clusters)))
    p <- ggplot() + geom_line(data = long, aes(year, value, group = run_id, colour = cluster), linewidth = .25, alpha = .25) +
      geom_line(data = cen, aes(year, value, colour = cluster), linewidth = 1.4) + geom_point(data = cen, aes(year, value, colour = cluster), size = 2) +
      scale_colour_manual(values = pal_cluster, name = NULL) +
      labs(x = NULL, y = ind_lab[[ind]], title = sprintf("%s, %s: time-series clusters (k = %d)", ind_lab[[ind]], e, k_clusters)) + theme_paper(base_size = 10)
    save_fig(p, sprintf("03_clusters_%s_%s", e, ind), sub = "03_clusters", width = 7, height = 4.5)

    # which inputs separate the highlighted cluster from the rest?
    d <- inner_join(data.frame(run_id = names(cl$cluster), cluster = cl$cluster), inputs, by = "run_id")
    X <- d[, setdiff(names(d), c("run_id", "cluster")), drop = FALSE]; yb <- as.integer(d$cluster == cl_highlight)
    if (sum(yb) >= 5) {
      imp <- sd_rf(X, yb, n_trees, rf_depth, rf_seed)
      imp_D[[e]] <- data.frame(ensemble = e, indicator = ind, cluster = cl_highlight, input = names(imp), importance = as.numeric(imp))
      tp <- names(imp)[seq_len(min(top_n, length(imp)))]
      pb <- ggplot(data.frame(v = factor(tp, rev(tp)), i = imp[tp]), aes(i, v)) + geom_col(fill = pal_cluster[cl_highlight], width = .7) +
        labs(x = "Feature importance", y = NULL, subtitle = sprintf("Membership of Cluster %d vs rest", cl_highlight)) + theme_paper(base_size = 10)
      hl <- long |> mutate(inn = cluster == paste("Cluster", cl_highlight))
      pt <- ggplot(hl, aes(year, value, group = run_id, colour = inn, linewidth = inn, alpha = inn)) + geom_line() +
        scale_colour_manual(values = c("grey60", pal_cluster[cl_highlight]), guide = "none") +
        scale_linewidth_manual(values = c(.25, .5), guide = "none") + scale_alpha_manual(values = c(.3, .8), guide = "none") +
        labs(x = NULL, y = ind_lab[[ind]]) + theme_paper(base_size = 10)
      save_fig(patchwork::wrap_plots(pb, pt, widths = c(.42, .58)) + patchwork::plot_annotation(title = sprintf("%s, %s", ind_lab[[ind]], e)),
               sprintf("03_cluster%d_inputs_%s_%s", cl_highlight, e, ind), sub = "03_clusters", width = 10, height = 4.8)
    }

    # outputs of each cluster at sc_year: the three indicators + key EPPA prices (parallel axes)
    oi <- res |> filter(ensemble == e, year == sc_year) |> select(run_id, indicator, value) |>
      pivot_wider(names_from = indicator, values_from = value)
    names(oi)[match(names(ind_lab), names(oi))] <- unname(ind_lab)
    pr <- sd_read_eppa_prices(e, years = sc_year) |> filter(sector %in% c("elec", "gas", "roil", "tran", "serv")) |>
      mutate(sector = paste(sd_eppa_secs[sector], "price"), run_id = as.character(run_id)) |> select(-year) |>
      pivot_wider(names_from = sector, values_from = price)
    po <- inner_join(oi, pr, by = "run_id") |> inner_join(data.frame(run_id = names(cl$cluster), cluster = cl$cluster), by = "run_id") |> na.omit()
    if (nrow(po) > 20) {
      pp <- GGally::ggparcoord(mutate(po, cluster = factor(cluster, 1:k_clusters, paste("Cluster", 1:k_clusters))),
                               columns = setdiff(seq_along(po), c(1, match("cluster", names(po)))), groupColumn = "cluster",
                               scale = "uniminmax", alphaLines = .3) +
        scale_colour_manual(values = pal_cluster, name = NULL) +
        labs(x = NULL, y = "Scaled value (min-max)", title = sprintf("%s, %s, %d: outputs by cluster", ind_lab[[ind]], e, sc_year)) +
        theme_paper(base_size = 10) + theme(axis.text.x = element_text(angle = 25, hjust = 1))
      save_fig(pp, sprintf("03_cluster_outputs_%s_%s_%d", e, ind, sc_year), sub = "03_clusters", width = 10, height = 5)
    }
  }
  write.csv(bind_rows(cl_all), file.path(path_tab, "03_cluster_membership.csv"), row.names = FALSE)
  write.csv(bind_rows(imp_D),  file.path(path_tab, "03_cluster_importance_inputs.csv"), row.names = FALSE)
}
message("03 done. Figures: ", path_fig, "  Tables: ", path_tab)
