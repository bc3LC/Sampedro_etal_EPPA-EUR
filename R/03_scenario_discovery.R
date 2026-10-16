# ==============================================================================
# 03_scenario_discovery.R
# Which EPPA uncertainties / prices drive the worst household outcomes?  (R port of JP-Data-Viz)
#
# Blocks (switch each on/off in section 0):
#   A  Input-output mapping : EPPA uncertain inputs -> "worst 30% of runs" (RF, CART, permutation importance)
#   B  Regional heatmap     : RF importance of inputs over years (panels: indicator x ensemble)
#   C  Output-output mapping: EPPA sector prices (and optional extra outputs) -> "worst 30% of runs"
#   D  Time-series clustering: k-means of 2020-2040 trajectories, binary RF per cluster, outputs by cluster
#   E  Direction and shape of the input effects: Spearman sign heatmap + partial dependence plots (1-D and 2-D)
#   F  Extreme scenarios: the BEST (least harm for households) and/or WORST runs, and the input values that characterise them
#
# Reads : data/processed/medusa_long.rds (written by 02), the EPPA design file (path_inputs), EPPA price files
# Writes: outputs/tables/03_*.csv, outputs/figures/03_* (subfolders per block)
# ==============================================================================
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

# ---- 0. Settings ---------------------------------------------------------------------
run_A <- TRUE; run_B <- TRUE; run_C <- TRUE; run_D <- TRUE; run_E <- TRUE; run_F <- TRUE

# path_inputs (EPPA design file: 'Run #' + one column per uncertain input) is defined in config.R
sc_region   <- "EUR"          # region used to filter TFP / Pop / AEEI columns of the design file
sc_year     <- 2030           # main year for blocks A, C and the cluster outputs
sc_years    <- years_calc     # years of the heatmap (2020 is the base: no variance)
adverse_pct <- 70             # class 1 = worst (100 - 70)% of runs
n_trees     <- 100; rf_depth <- 4; cart_depth <- 4; cart_minbucket <- 10; top_n <- 5; rf_seed <- 0
k_clusters  <- 3; cl_indicator <- "di"; cl_highlight <- 1   # cluster 1 = most adverse
sc_ens      <- ensembles  # ensembles analysed (use setdiff(ensembles, "Ref") to skip the reference)
# Target of blocks A, B, D:  "level" = outcome of each ensemble;  "diff" = policy minus Ref, same run (what the policy
# changes; Ref is skipped);  c("level", "diff") runs both. Files of "diff" get the suffix _diff and column target = "diff".
target_mode <- c("level", "diff")
case_type <- c("best", "worst")   # block F: which extreme runs ("best" = smallest DI loss / lowest EP and TP; "worst" = the opposite)
extreme_n <- 15               # block F: runs listed per indicator in the heatmap
extreme_pct <- 10             # block F: extreme group for the input profile = best / worst extreme_pct % of runs
worst_inputs <- 8             # block F: inputs shown (top by RF importance)
pdp_top <- 3                  # block E: inputs per indicator in the partial dependence plots (union over ensembles)
cl_min_share <- 0.05          # block D warns if a cluster holds less than this share of the runs (outlier cluster)
ind_lab <- c(di = "Distributional impact", ep = "Energy poverty", tp = "Transport poverty")
cls_lab <- function(ind) c("Other runs", paste0("Worst ", 100 - adverse_pct, "% (", tolower(ind_lab[[ind]]), ")"))
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)
if (!exists("sd_pdp2")) stop("R/functions/scenario_discovery.R is an old version (block E needs sd_direction / sd_pdp): replace it with the latest one")
if (!exists("sd_oob_auc")) warning("R/functions/scenario_discovery.R is an old version (no sd_oob_auc): replace it with the latest one; oob_auc will be NA")
`%||%` <- function(a, b) if (is.null(a)) b else a
stopifnot(all(target_mode %in% c("level", "diff")))

# ---- MEDUSA outcomes (from 02) and design file ------------------------------------------
res <- readRDS(file.path(path_proc, "medusa_long.rds")) |>
  filter(year <= max(years_calc)) |> drop_flagged() |> mutate(run_id = as.character(run_id))
# policy minus Ref for the same run (negative DI = worse than Ref; positive EP/TP = worse than Ref)
res_diff <- function(r) {
  k <- intersect(c("run_id", "region", "group", "indicator", "year"), names(r))
  ref <- r |> filter(ensemble == "Ref") |> select(all_of(k), ref = value)
  r |> filter(ensemble != "Ref") |> inner_join(ref, by = k) |> mutate(value = value - ref) |> select(-ref)
}
res_by <- list(level = res)
if ("diff" %in% target_mode) res_by$diff <- res_diff(res)
mode_ens <- function(md) if (md == "diff") setdiff(sc_ens, "Ref") else sc_ens
mode_sfx <- function(md) if (md == "diff") "_diff" else ""
mode_lab <- function(md) if (md == "diff") " (policy - Ref)" else ""
stopifnot("add path_inputs to config.R" = exists("path_inputs"), file.exists(path_inputs))
inputs <- sd_read_inputs(path_inputs, sc_region)
message("Design file: ", nrow(inputs), " runs, ", ncol(inputs) - 1, " inputs (region ", sc_region, ")")

# outcome vector at one year (run_id, value) and aligned [inputs | target] table
outcome_at <- function(e, ind, yr, R = res) R |> filter(ensemble == e, indicator == ind, year == yr) |> select(run_id, value)
aligned <- function(e, ind, yr, R = res) {
  o <- outcome_at(e, ind, yr, R)
  d <- inner_join(o, inputs, by = "run_id") |> filter(!is.na(value))
  list(X = d[, setdiff(names(d), c("run_id", "value")), drop = FALSE], value = d$value, run_id = d$run_id,
       y = sd_target(ind, d$value, adverse_pct))
}

# ==============================================================================
# A. Input-output mapping at sc_year
# ==============================================================================
if (run_A) {
  imp_A <- list(); cart_A <- list()
  for (md in target_mode) for (e in mode_ens(md)) for (ind in names(ind_lab)) {
    a <- aligned(e, ind, sc_year, res_by[[md]]); if (nrow(a$X) < 50 || sum(a$y) < 5) next
    imp  <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
    perm <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed, perm = TRUE)
    cart <- sd_cart(a$X, a$y, cart_depth, cart_minbucket)
    key <- paste(md, e, ind)
    imp_A[[key]]  <- data.frame(target = md, ensemble = e, indicator = ind, year = sc_year, input = names(imp),
                                importance = as.numeric(imp), perm_importance = as.numeric(perm[names(imp)]),
                                oob_error = attr(imp, "oob_error") %||% NA_real_, oob_auc = attr(imp, "oob_auc") %||% NA_real_)
    cart_A[[key]] <- cbind(target = md, ensemble = e, indicator = ind, year = sc_year, cart$leaves)
    p <- sd_plot_parcoords(a$X, a$y, a$value, imp, paste0(ind_lab[[ind]], mode_lab(md)), cls_lab(ind), top_n,
                           title = sprintf("%s%s, %s, %d: input-output mapping (n = %d runs)", ind_lab[[ind]], mode_lab(md), e, sc_year, nrow(a$X)))
    save_fig(p, sprintf("03_io_%s_%s_%d%s", e, ind, sc_year, mode_sfx(md)), sub = "03_input_output", width = 11, height = 4.8)
    png(file.path(path_fig, sprintf("03_input_output/03_cart_%s_%s_%d%s.png", e, ind, sc_year, mode_sfx(md))), 3600, 1800, res = 250)
    plot(partykit::as.party(cart$fit), main = sprintf("CART: %s%s, %s, %d (class 1 = worst runs)", ind_lab[[ind]], mode_lab(md), e, sc_year),
         tp_args = list(id = FALSE), ip_args = list(id = FALSE), gp = grid::gpar(fontsize = 9))
    dev.off()
  }
  write.csv(bind_rows(imp_A),  file.path(path_tab, "03_importance_inputs.csv"), row.names = FALSE)
  write.csv(bind_rows(cart_A), file.path(path_tab, "03_cart_leaves.csv"), row.names = FALSE)
}

# ==============================================================================
# B. Heatmap: input importance over time (top inputs by summed importance in each panel)
# ==============================================================================
if (run_B) {
  hb_all <- list()
  for (md in target_mode) {
    ens <- mode_ens(md)
    hb <- bind_rows(lapply(ens, function(e) bind_rows(lapply(names(ind_lab), function(ind)
      bind_rows(lapply(sc_years[sc_years > 2020], function(yr) {
        a <- aligned(e, ind, yr, res_by[[md]]); if (nrow(a$X) < 50 || sum(a$y) < 5) return(NULL)
        imp <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
        data.frame(target = md, ensemble = e, indicator = ind, year = yr, input = names(imp), importance = as.numeric(imp))
      }))))))
    hb_all[[md]] <- hb
    # rows: union over ensembles of each ensemble's top inputs (so every panel of a row shows the same inputs)
    top <- hb |> group_by(ensemble, indicator, input) |> summarise(s = sum(importance), .groups = "drop") |>
      group_by(ensemble, indicator) |> slice_max(s, n = top_n, with_ties = FALSE) |> ungroup() |> distinct(indicator, input)
    hp <- inner_join(hb, top, by = c("indicator", "input")) |>
      mutate(ensemble = factor(ensemble, ens), indicator = factor(indicator, names(ind_lab), ind_lab))
    p <- ggplot(hp, aes(factor(year), input, fill = importance)) + geom_tile(colour = "white") +
      facet_grid(indicator ~ ensemble, scales = "free_y") + scale_fill_importance(name = "Avg. feature\nimportance") +
      labs(x = NULL, y = NULL, title = sprintf("RF feature importance of EPPA inputs, %s%s", sc_region, mode_lab(md))) +
      theme_paper(base_size = 9) + theme(panel.grid = element_blank(), legend.position = "right")
    save_fig(p, paste0("03_heatmap_inputs", mode_sfx(md)), sub = "03_heatmaps", width = if (md == "diff") 9.5 else 12, height = 8.5)
  }
  write.csv(bind_rows(hb_all), file.path(path_tab, "03_importance_inputs_by_year.csv"), row.names = FALSE)
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
  for (md in target_mode) for (e in mode_ens(md)) {
    sfx <- mode_sfx(md); key <- paste(md, e)
    W <- res_by[[md]] |> filter(ensemble == e, indicator == ind) |> select(run_id, year, value) |>
      pivot_wider(names_from = year, values_from = value) |> filter(complete.cases(pick(everything())))
    M <- as.matrix(W[, -1]); rownames(M) <- W$run_id
    cl <- sd_cluster(M, ind, k_clusters, rf_seed)
    yrs <- as.integer(colnames(M))
    share <- table(cl$cluster) / length(cl$cluster)
    if (length(share) < k_clusters || any(share < cl_min_share))
      warning(sprintf("Block D (%s, %s): cluster sizes %s - a cluster holds < %.0f%% of the runs (outlier trajectories?); check before interpreting",
                      md, e, paste(table(cl$cluster), collapse = "/"), 100 * cl_min_share))
    cl_all[[key]] <- data.frame(target = md, ensemble = e, indicator = ind, run_id = names(cl$cluster), cluster = as.integer(cl$cluster))
    # trajectories coloured by cluster, centres in bold
    long <- data.frame(run_id = rep(rownames(M), ncol(M)), year = rep(yrs, each = nrow(M)), value = as.vector(M),
                       cluster = factor(rep(cl$cluster, ncol(M)), 1:k_clusters, paste("Cluster", 1:k_clusters)))
    cen <- data.frame(year = rep(yrs, each = k_clusters), value = as.vector(cl$centers),
                      cluster = factor(rep(1:k_clusters, ncol(M)), 1:k_clusters, paste("Cluster", 1:k_clusters)))
    p <- ggplot() + geom_line(data = long, aes(year, value, group = run_id, colour = cluster), linewidth = .25, alpha = .25) +
      geom_line(data = cen, aes(year, value, colour = cluster), linewidth = 1.4) + geom_point(data = cen, aes(year, value, colour = cluster), size = 2) +
      scale_colour_manual(values = pal_cluster, name = NULL) +
      labs(x = NULL, y = paste0(ind_lab[[ind]], mode_lab(md)), title = sprintf("%s%s, %s: time-series clusters (k = %d)", ind_lab[[ind]], mode_lab(md), e, k_clusters)) + theme_paper(base_size = 10)
    save_fig(p, sprintf("03_clusters_%s_%s%s", e, ind, sfx), sub = "03_clusters", width = 7, height = 4.5)

    # which inputs separate the highlighted cluster from the rest?
    d <- inner_join(data.frame(run_id = names(cl$cluster), cluster = cl$cluster), inputs, by = "run_id")
    X <- d[, setdiff(names(d), c("run_id", "cluster")), drop = FALSE]; yb <- as.integer(d$cluster == cl_highlight)
    if (sum(yb) >= 5) {
      imp <- sd_rf(X, yb, n_trees, rf_depth, rf_seed)
      imp_D[[key]] <- data.frame(target = md, ensemble = e, indicator = ind, cluster = cl_highlight, n_cluster = sum(yb), input = names(imp),
                                 importance = as.numeric(imp), oob_error = attr(imp, "oob_error") %||% NA_real_, oob_auc = attr(imp, "oob_auc") %||% NA_real_)
      tp <- names(imp)[seq_len(min(top_n, length(imp)))]
      pb <- ggplot(data.frame(v = factor(tp, rev(tp)), i = imp[tp]), aes(i, v)) + geom_col(fill = pal_cluster[cl_highlight], width = .7) +
        labs(x = "Feature importance", y = NULL, subtitle = sprintf("Membership of Cluster %d vs rest", cl_highlight)) + theme_paper(base_size = 10)
      hl <- long |> mutate(inn = cluster == paste("Cluster", cl_highlight))
      pt <- ggplot(hl, aes(year, value, group = run_id, colour = inn, linewidth = inn, alpha = inn)) + geom_line() +
        scale_colour_manual(values = c("grey60", pal_cluster[cl_highlight]), guide = "none") +
        scale_linewidth_manual(values = c(.25, .5), guide = "none") + scale_alpha_manual(values = c(.3, .8), guide = "none") +
        labs(x = NULL, y = paste0(ind_lab[[ind]], mode_lab(md))) + theme_paper(base_size = 10)
      save_fig(patchwork::wrap_plots(pb, pt, widths = c(.42, .58)) + patchwork::plot_annotation(title = sprintf("%s%s, %s", ind_lab[[ind]], mode_lab(md), e)),
               sprintf("03_cluster%d_inputs_%s_%s%s", cl_highlight, e, ind, sfx), sub = "03_clusters", width = 10, height = 4.8)
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
        labs(x = NULL, y = "Scaled value (min-max)", title = sprintf("%s%s, %s, %d: outputs by cluster (outputs in levels)", ind_lab[[ind]], mode_lab(md), e, sc_year)) +
        theme_paper(base_size = 10) + theme(axis.text.x = element_text(angle = 25, hjust = 1))
      save_fig(pp, sprintf("03_cluster_outputs_%s_%s_%d%s", e, ind, sc_year, sfx), sub = "03_clusters", width = 10, height = 5)
    }
  }
  write.csv(bind_rows(cl_all), file.path(path_tab, "03_cluster_membership.csv"), row.names = FALSE)
  write.csv(bind_rows(imp_D),  file.path(path_tab, "03_cluster_importance_inputs.csv"), row.names = FALSE)
}

# ==============================================================================
# E. Direction (sign) and shape of the input effects at sc_year, for every target mode
#    - Spearman rho of each input with adversity (rho > 0: higher input = worse outcome); complements RF importance, which has no sign
#    - Partial dependence of the outcome (original units) on the top inputs, ensembles overlaid; 2-D for the top two inputs
# ==============================================================================
if (run_E) {
  ens_col <- c(`2C` = "#0072B2", `1p5C` = "#D55E00", PF = "#009E73", Ref = "#000000")
  dir_all <- list(); pdp_all <- list()
  for (md in target_mode) {
    sfx <- mode_sfx(md); ens <- mode_ens(md); fits <- list(); tops <- list()
    for (e in ens) for (ind in names(ind_lab)) {
      a <- aligned(e, ind, sc_year, res_by[[md]]); if (nrow(a$X) < 50 || sum(a$y) < 5) next
      key <- paste(e, ind)
      imp <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
      dd <- sd_direction(a$X, sd_adverse(ind, a$value))
      dd$importance <- as.numeric(imp[dd$input])
      dir_all[[paste(md, key)]] <- cbind(target = md, ensemble = e, indicator = ind, year = sc_year, dd)
      fits[[key]] <- list(fit = sd_rf_reg(a$X, a$value, seed = rf_seed), X = a$X, imp = imp)
    }
    if (!length(fits)) next
    dm <- bind_rows(dir_all[startsWith(names(dir_all), paste0(md, " "))])
    # (1) sign heatmap: inputs = union of each panel's top_n inputs by RF importance
    tp <- dm |> group_by(ensemble, indicator) |> slice_max(importance, n = top_n, with_ties = FALSE) |> ungroup() |> distinct(indicator, input)
    hp <- inner_join(dm, tp, by = c("indicator", "input")) |>
      mutate(ensemble = factor(ensemble, ens), indicator = factor(indicator, names(ind_lab), ind_lab),
             star = ifelse(p < 0.01, "*", ""))
    p1 <- ggplot(hp, aes(ensemble, input, fill = rho)) + geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f%s", rho, star)), size = 2.6) +
      facet_wrap(~indicator, scales = "free_y", nrow = 1) +
      scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1, 1) * max(abs(hp$rho)), name = "Spearman\nrho") +
      labs(x = NULL, y = NULL, title = sprintf("Direction of the input effects, %s, %d%s", sc_region, sc_year, mode_lab(md)),
           subtitle = "Red: a higher input value makes the outcome worse for households (higher poverty / lower DI). * p < 0.01") +
      theme_paper(base_size = 9) + theme(panel.grid = element_blank())
    save_fig(p1, paste0("03_direction_inputs", sfx), sub = "03_direction_pdp", width = 12, height = 5.5)
    # (2) partial dependence of the outcome on the top inputs (union over ensembles), ensembles overlaid
    for (ind in names(ind_lab)) {
      ks <- paste(ens, ind); ks <- ks[ks %in% names(fits)]; if (!length(ks)) next
      vars <- unique(unlist(lapply(ks, function(k) names(fits[[k]]$imp)[seq_len(pdp_top)])))
      pd <- bind_rows(lapply(ks, function(k) cbind(ensemble = sub(" .*", "", k),
                                                  sd_pdp(fits[[k]]$fit, fits[[k]]$X, vars))))
      pdp_all[[paste(md, ind)]] <- cbind(target = md, indicator = ind, year = sc_year, pd)
      p2 <- ggplot(mutate(pd, ensemble = factor(ensemble, ens), input = factor(input, vars)), aes(x, pd, colour = ensemble)) +
        geom_line(linewidth = .9) + facet_wrap(~input, scales = "free_x") +
        scale_colour_manual(values = ens_col, name = NULL) +
        labs(x = "Input value (5th-95th percentile of the design)", y = paste0(ind_lab[[ind]], mode_lab(md)),
             title = sprintf("Partial dependence, %s%s, %d", ind_lab[[ind]], mode_lab(md), sc_year)) + theme_paper(base_size = 10)
      save_fig(p2, sprintf("03_pdp_%s_%d%s", ind, sc_year, sfx), sub = "03_direction_pdp", width = 11, height = 3 + 2.2 * ceiling(length(vars) / 3))
      # (3) 2-D partial dependence of the top two inputs of each ensemble (interaction)
      for (k in ks) {
        v2 <- names(fits[[k]]$imp)[1:2]
        g <- sd_pdp2(fits[[k]]$fit, fits[[k]]$X, v2[1], v2[2])
        p3 <- ggplot(g, aes(x1, x2, fill = pd)) + geom_raster(interpolate = TRUE) +
          scale_fill_viridis_c(name = paste0(ind_lab[[ind]], mode_lab(md))) +
          labs(x = v2[1], y = v2[2], title = sprintf("2-D partial dependence, %s%s, %s, %d", ind_lab[[ind]], mode_lab(md), sub(" .*", "", k), sc_year)) +
          theme_paper(base_size = 10)
        save_fig(p3, sprintf("03_pdp2_%s_%s_%d%s", sub(" .*", "", k), ind, sc_year, sfx), sub = "03_direction_pdp", width = 6.5, height = 5)
      }
    }
  }
  write.csv(bind_rows(dir_all), file.path(path_tab, "03_direction_inputs.csv"), row.names = FALSE)
  write.csv(bind_rows(pdp_all), file.path(path_tab, "03_pdp_inputs.csv"), row.names = FALSE)
}

# ==============================================================================
# F. Extreme scenarios at sc_year: the BEST runs (smallest loss of DI, lowest energy / transport poverty) and/or the WORST runs.
#    Every run of a policy ensemble is a scenario that meets the 2C / 1.5C target by construction, so "best" = least harm
#    to households at the same climate outcome; with target_mode "diff" it is the run where the policy costs households
#    least compared with Ref. Input values are shown as percentile ranks within the ensemble (0 = lowest value of the
#    design, 1 = highest); inputs = the worst_inputs most important in the forest that separates the extreme 30% of runs.
#    Each indicator is ranked separately; the table lists the other indicators in which the same run is also extreme
#    (a run that is extreme in all three has no trade-off: see 03_extreme_overlap.csv).
# ==============================================================================
if (run_F) {
  ens_col <- c(`2C` = "#0072B2", `1p5C` = "#D55E00", PF = "#009E73", Ref = "#000000")
  ext_tab <- list(); ovl_tab <- list()
  for (cs in case_type) {
    sg <- if (cs == "best") -1 else 1                       # sd_adverse() is larger = worse; best = smallest
    ccol <- if (cs == "best") "#1b7837" else "#b2182b"
    for (md in target_mode) {
      sfx <- mode_sfx(md); ens <- mode_ens(md); prof <- list()
      for (e in ens) {
        pan <- list()
        A <- lapply(setNames(names(ind_lab), names(ind_lab)), function(ind) aligned(e, ind, sc_year, res_by[[md]]))
        n_grp <- function(a) ceiling(extreme_pct / 100 * length(a$value))
        grp_ind <- lapply(setNames(names(ind_lab), names(ind_lab)), function(k)
          A[[k]]$run_id[rank(-sg * sd_adverse(k, A[[k]]$value), ties.method = "first") <= n_grp(A[[k]])])
        n_in <- table(factor(unlist(grp_ind), levels = unique(A[[1]]$run_id)))
        all3 <- names(n_in)[n_in == length(ind_lab)]
        ovl_tab[[paste(cs, md, e)]] <- data.frame(case = cs, target = md, ensemble = e, year = sc_year, extreme_pct = extreme_pct,
          n_group = n_grp(A[[1]]), n_in_1 = sum(n_in >= 1), n_in_2 = sum(n_in >= 2), n_in_all = length(all3), runs_in_all = paste(all3, collapse = " "))
        for (ind in names(ind_lab)) {
          a <- A[[ind]]; if (nrow(a$X) < 50) next
          adv <- sg * sd_adverse(ind, a$value)               # larger = more extreme in the chosen direction
          y <- as.integer(adv > quantile(adv, adverse_pct / 100)); if (sum(y) < 5) next
          imp <- sd_rf(a$X, y, n_trees, rf_depth, rf_seed); vars <- names(imp)[seq_len(min(worst_inputs, length(imp)))]
          P <- as.data.frame(lapply(a$X[, vars, drop = FALSE], function(v) rank(v) / (length(v) + 1)), check.names = FALSE)
          ord <- order(adv, decreasing = TRUE)
          top <- ord[seq_len(min(extreme_n, length(ord)))]
          grp <- ord[seq_len(n_grp(a))]
          others <- vapply(a$run_id, function(r) paste(setdiff(names(ind_lab)[vapply(names(ind_lab), function(k) r %in% grp_ind[[k]], logical(1))], ind), collapse = "+"), "")
          lab <- sprintf("run %s  (%s)", a$run_id[top], formatC(a$value[top], digits = 3, format = "g"))
          hd <- bind_rows(lapply(vars, function(v) data.frame(run = factor(lab, rev(lab)), input = factor(v, vars), pct = P[top, v])))
          pan[[ind]] <- ggplot(hd, aes(input, run, fill = pct)) + geom_tile(colour = "white") +
            scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = .5, limits = c(0, 1), name = "Input value\n(percentile)") +
            labs(x = NULL, y = NULL, subtitle = sprintf("%s%s: %d %s runs (value)", ind_lab[[ind]], mode_lab(md), length(top), cs)) +
            theme_paper(base_size = 9) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1))
          prof[[paste(e, ind)]] <- do.call(rbind, lapply(vars, function(v) data.frame(
            ensemble = e, indicator = ind, input = v, importance = as.numeric(imp[v]), n_group = length(grp),
            q25 = quantile(P[grp, v], .25), med = median(P[grp, v]), q75 = quantile(P[grp, v], .75))))
          ext_tab[[paste(cs, md, e, ind)]] <- do.call(rbind, lapply(vars, function(v) data.frame(
            case = cs, target = md, ensemble = e, indicator = ind, year = sc_year, rank = seq_along(top), run_id = a$run_id[top], value = a$value[top],
            also_extreme_in = others[top], input = v, input_value = a$X[top, v], input_pct = P[top, v])))
        }
        if (length(pan))
          save_fig(patchwork::wrap_plots(pan, nrow = 1, guides = "collect") +
                     patchwork::plot_annotation(title = sprintf("%s-case scenarios, %s%s, %d", tools::toTitleCase(cs), e, mode_lab(md), sc_year),
                       subtitle = sprintf("Rows: runs ranked from the %s (%s). Colour: input value as percentile of the ensemble (red = high, blue = low)",
                                          cs, if (cs == "best") "smallest DI loss / lowest poverty" else "largest DI loss / highest poverty")),
                   sprintf("03_%s_runs_%s_%d%s", cs, e, sc_year, sfx), sub = "03_extreme_cases", width = 15, height = 1.6 + 0.3 * extreme_n)
      }
      pf <- bind_rows(prof)
      if (nrow(pf)) {
        keep <- pf |> group_by(indicator, input) |> summarise(m = max(importance), .groups = "drop") |>
          group_by(indicator) |> slice_max(m, n = 6, with_ties = FALSE) |> ungroup()
        pf <- inner_join(pf, keep[, c("indicator", "input")], by = c("indicator", "input")) |>
          mutate(indicator = factor(indicator, names(ind_lab), ind_lab), ensemble = factor(ensemble, ens))
        pf <- left_join(pf, pf |> group_by(indicator, input) |> summarise(m = mean(abs(med - .5)), .groups = "drop"), by = c("indicator", "input"))
        pf$key <- reorder(paste(pf$input, pf$indicator, sep = "___"), pf$m)
        p <- ggplot(pf, aes(med, key, colour = ensemble)) +
          geom_vline(xintercept = .5, linetype = 2, colour = "grey50") +
          geom_pointrange(aes(xmin = q25, xmax = q75), position = position_dodge(width = .6), size = .35) +
          facet_wrap(~indicator, scales = "free_y", nrow = 1) +
          scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
          scale_colour_manual(values = ens_col, name = NULL) + scale_x_continuous(limits = c(0, 1)) +
          labs(x = sprintf("Input value among the %s runs (percentile of the ensemble; point = median, bar = interquartile range)", cs), y = NULL,
               title = sprintf("What the %s %d%% of runs have in common, %s, %d%s", cs, extreme_pct, sc_region, sc_year, mode_lab(md)),
               subtitle = "Dashed line = the whole ensemble (0.5). Points far from it identify the inputs that characterise these scenarios") +
          theme_paper(base_size = 9)
        save_fig(p, sprintf("03_%s_profile%s", cs, sfx), sub = "03_extreme_cases", width = 13, height = 5)
      }
    }
  }
  write.csv(bind_rows(ext_tab), file.path(path_tab, "03_extreme_scenarios.csv"), row.names = FALSE)
  write.csv(bind_rows(ovl_tab), file.path(path_tab, "03_extreme_overlap.csv"), row.names = FALSE)
}
message("03 done. Figures: ", path_fig, "  Tables: ", path_tab)
