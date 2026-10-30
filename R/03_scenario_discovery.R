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
#   G  Filtered input-output mapping (the platform's "filtered mode"): inputs that separate the runs that meet SEVERAL outputs
#      at once (e.g. smallest DI loss AND lowest EP AND lowest TP) from the rest
#
# Reads : data/processed/medusa_long.rds (written by 02), the EPPA design file (path_inputs), EPPA price files
# Writes: outputs/tables/03_*.csv, outputs/figures/03_* (subfolders per block)
# ==============================================================================
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

# ---- 0. Settings ---------------------------------------------------------------------
run_A <- TRUE; run_B <- TRUE; run_C <- TRUE; run_D <- TRUE; run_E <- TRUE; run_F <- TRUE; run_G <- TRUE

# path_inputs (EPPA design file: 'Run #' + one column per uncertain input) is defined in config.R
sc_region   <- "EUR"          # region used to filter TFP / Pop / AEEI columns of the design file
sc_year     <- 2030           # main year for blocks A, C and the cluster outputs
sc_years    <- years_calc     # years of the heatmap (2020 is the base: no variance)
di_loss_is_negative <- TRUE   # DI = welfare change in %: negative = loss, so -4% is worse than -2%. (EP, TP: higher rate = worse)
min_perm_imp <- 0.005   # filtered-mode parallel axes: show only inputs whose permutation importance exceeds this (at least 2 shown)
options(sd.di_loss_is_negative = di_loss_is_negative)
adverse_pct <- 70             # class 1 = worst (100 - 70)% of runs
n_trees     <- 100; rf_depth <- 4; cart_depth <- 4; cart_minbucket <- 10; top_n <- 5; rf_seed <- 0
k_clusters  <- 3; cl_indicator <- c("di", "ep", "tp"); cl_highlight <- 1   # indicators to cluster; cluster 1 = most adverse
sc_ens      <- ensembles  # ensembles analysed (use setdiff(ensembles, "Ref") to skip the reference)
target_mode <- "level"   # only the level of each ensemble is analysed (policy-minus-Ref "diff" mode removed)
case_type <- c("best", "worst")   # block F: which extreme runs ("best" = smallest DI loss / lowest EP and TP; "worst" = the opposite)
extreme_n <- 15               # block F: runs listed per indicator in the heatmap
extreme_pct <- 10             # block F: extreme group for the input profile = best / worst extreme_pct % of runs
worst_inputs <- 8             # block F: inputs shown (top by RF importance)
# Block G (filtered mode). A case selects the runs that satisfy ALL its conditions. Bounds are ADVERSITY PERCENTILES of the ensemble:
# 0 = best run, 1 = worst run (so c(0, .45) = "best 45%" for DI, EP and TP alike). Two ways to define a case:
#   list(side = "best"/"worst", q = "auto" or a number or c(di = .4, ep = .5, tp = .5), ind = optional subset of c("di","ep","tp"))
#        -> every indicator within the best (0, q) / worst (1 - q, 1) fraction (one q for all, or one per indicator);
#           q = "auto" tunes the common q to select about filter_n_target runs
#   list(bounds = list(ep = c(0, .5), tp = c(0, .5)))   -> explicit bounds (as the sliders of the platform)
filter_years    <- unique(c(sc_year, max(sc_years)))   # year(s) of block G: the main year and the last year (2030 and 2040)
filter_n_target <- 100           # runs the "auto" cases aim at
filter_n_range  <- c(80, 150)    # the platform recommends 80-150 selected runs of 400 (warning outside)
filter_cases <- list(
  "Best on all three (small DI loss, low EP, TP)"  = list(side = "best",  q = "auto"),
  "Worst on all three (large DI loss, high EP, TP)" = list(side = "worst", q = "auto"),
  "Low EP and TP (best 45%)" = list(bounds = list(ep = c(0, .45), tp = c(0, .45)))
)
pdp_top <- 3                  # block E: inputs per indicator in the partial dependence plots (union over ensembles)
cl_min_share <- 0.05          # block D warns if a cluster holds less than this share of the runs (outlier cluster)
ind_lab <- c(di = "Distributional impact", ep = "Energy poverty", tp = "Transport poverty")
cls_lab <- function(ind) c("Other runs", paste0("Worst ", 100 - adverse_pct, "% (", tolower(ind_lab[[ind]]), ")"))
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)
if (!exists("sd_filter_select")) stop("R/functions/scenario_discovery.R is an old version (block G needs sd_filter_select): replace it with the latest one")
if (!exists("sd_pdp2")) stop("R/functions/scenario_discovery.R is an old version (block E needs sd_direction / sd_pdp): replace it with the latest one")
if (!exists("sd_oob_auc")) warning("R/functions/scenario_discovery.R is an old version (no sd_oob_auc): replace it with the latest one; oob_auc will be NA")
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- MEDUSA outcomes (from 02) and design file ------------------------------------------
res <- readRDS(file.path(path_proc, "medusa_long.rds")) |>
  filter(year <= max(years_calc)) |> drop_flagged() |> mutate(run_id = as.character(run_id))
# sanity check of the DI sign convention
{ m_di <- median(res$value[res$indicator == "di"], na.rm = TRUE)
  message(sprintf("DI convention: negative DI = loss (median DI in the data = %.2f).", m_di))
  if (di_loss_is_negative && is.finite(m_di) && m_di > 0)
    warning("Median DI is positive while di_loss_is_negative = TRUE: check the sign of DI.", call. = FALSE) }
res_by <- list(level = res)
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
    # one heatmap per ensemble: own top inputs and own colour scale, so the ensemble with the largest importances (e.g. 1p5C) does not wash out the others
    for (e in ens) {
      he <- hb |> filter(ensemble == e)
      tope <- he |> group_by(indicator, input) |> summarise(s = sum(importance), .groups = "drop") |>
        group_by(indicator) |> slice_max(s, n = top_n, with_ties = FALSE) |> ungroup() |> select(indicator, input)
      hpe <- inner_join(he, tope, by = c("indicator", "input")) |> mutate(indicator = factor(indicator, names(ind_lab), ind_lab))
      pe <- ggplot(hpe, aes(factor(year), input, fill = importance)) + geom_tile(colour = "white") +
        geom_text(aes(label = sprintf("%.2f", importance)), size = 2.4) +
        facet_grid(indicator ~ ., scales = "free_y", switch = "y") +
        scale_fill_gradient(low = "white", high = "#0b3d91", limits = c(0, NA), name = "RF\nimportance") +
        labs(x = NULL, y = NULL, title = sprintf("RF feature importance of EPPA inputs, %s, %s%s", sc_region, e, mode_lab(md)),
             subtitle = "Top inputs per metric for this ensemble; colour scale specific to this ensemble") +
        theme_paper(base_size = 9) + theme(panel.grid = element_blank(), legend.position = "right", strip.placement = "outside")
      save_fig(pe, sprintf("03_heatmap_inputs_%s%s", e, mode_sfx(md)), sub = "03_heatmaps", width = 6.5, height = 7.5)
    }
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
  cl_all <- list(); imp_D <- list()
  for (ind in cl_indicator) for (md in target_mode) for (e in mode_ens(md)) {
    sfx <- mode_sfx(md); key <- paste(md, e, ind)
    W <- res_by[[md]] |> filter(ensemble == e, indicator == ind) |> select(run_id, year, value) |>
      pivot_wider(names_from = year, values_from = value) |> filter(complete.cases(pick(everything())))
    M <- as.matrix(W[, -1]); rownames(M) <- W$run_id
    cl <- sd_cluster(M, ind, k_clusters, rf_seed)
    yrs <- as.integer(colnames(M))
    share <- table(cl$cluster) / length(cl$cluster)
    if (length(share) < k_clusters || any(share < cl_min_share))
      warning(sprintf("Block D (%s, %s, %s): cluster sizes %s - a cluster holds < %.0f%% of the runs (outlier trajectories?); check before interpreting",
                      md, e, ind, paste(table(cl$cluster), collapse = "/"), 100 * cl_min_share))
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
      dd <- bind_rows(dd, cbind(metric = "selected", sd_direction(X, y)))     # rho between each input and membership of the selected set (case-specific)
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
           subtitle = "Red: a higher input value makes the outcome worse for households (larger DI loss / higher poverty). * p < 0.01") +
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
#    to households at the same climate outcome
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

# ==============================================================================
# G. Filtered input-output mapping (platform "filtered mode") at filter_years
#    Class 1 = runs that meet ALL the conditions of a case (see filter_cases); RF + CART + permutation importance as in block A.
#    Extra figures (folders 04_filtered_io, _cart, _runs, _direction, _common_drivers), one set per case, year and target mode:
#      - 04_filt_*            importance + parallel axes (inputs and the three outputs), and the CART tree
#      - 04_filt_direction_*  direction of each input's effect on DI, EP, TP and on the combined score (Spearman, as in block E)
#      - 04_filt_runs_*       the selected runs ranked by combined adversity (best first; worst first for "worst" cases), with their
#                             input percentiles and their outcomes (as block F, but joint)
#      - 04_filt_common_*     heatmap of RF importance for DI, EP, TP and each case: inputs that matter for all three metrics
#    Combined score of a run = mean of its three adversity percentiles (0 = best run of the ensemble, 1 = worst).
# ==============================================================================
if (run_G) {
  imp_G <- list(); cart_G <- list(); mem_G <- list(); dir_G <- list(); com_G <- list()
  slug <- function(x) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x))
  met_lab <- c(di = "DI", ep = "EP", tp = "TP", combined = "Combined")
  for (md in target_mode) for (e in mode_ens(md)) for (yr in filter_years) {
    O <- Reduce(function(a, b) inner_join(a, b, by = "run_id"), lapply(names(ind_lab), function(ind)
      outcome_at(e, ind, yr, res_by[[md]]) |> rename(!!ind := value)))
    D <- inner_join(O, inputs, by = "run_id") |> filter(if_all(all_of(names(ind_lab)), ~ !is.na(.x)))
    if (nrow(D) < 50) next
    P <- vapply(names(ind_lab), function(ind) { a <- sd_adverse(ind, D[[ind]]); (rank(a) - 1) / (length(a) - 1) }, numeric(nrow(D)))
    comb <- rowMeans(P)
    X <- D[, setdiff(names(D), c("run_id", names(ind_lab))), drop = FALSE]
    # single-metric forests (worst adverse_pct % of each metric) for the common-drivers heatmap
    for (ind in names(ind_lab)) {
      a <- aligned(e, ind, yr, res_by[[md]]); if (nrow(a$X) < 50 || sum(a$y) < 5) next
      im <- sd_rf(a$X, a$y, n_trees, rf_depth, rf_seed)
      com_G[[paste(md, e, yr, ind)]] <- data.frame(target = md, ensemble = e, year = yr, column = met_lab[[ind]], input = names(im), importance = as.numeric(im))
    }
    for (cs in names(filter_cases)) {
      fc <- filter_cases[[cs]]; worst_first <- identical(fc$side, "worst")
      f <- sd_filter_select(P, fc, filter_n_target); y <- as.integer(f$sel); n_sel <- sum(y)
      message(sprintf("G | %s%s | %d | %s: %d of %d runs selected%s", e, mode_lab(md), yr, cs, n_sel, length(y),
                      if (is.na(f$q)) "" else sprintf(" (q = %.2f)", f$q)))
      if (n_sel < filter_n_range[1] || n_sel > filter_n_range[2])
        warning(sprintf("Block G (%s, %s, %d, '%s'): %d runs selected, outside %d-%d (platform recommendation): over/under-fitting risk",
                        md, e, yr, cs, n_sel, filter_n_range[1], filter_n_range[2]))
      if (n_sel < 20 || n_sel > length(y) - 20) next
      imp  <- sd_rf(X, y, n_trees, rf_depth, rf_seed)
      perm <- sd_rf(X, y, n_trees, rf_depth, rf_seed, perm = TRUE)
      cart <- sd_cart(X, y, cart_depth, cart_minbucket)
      key <- paste(md, e, yr, cs)
      imp_G[[key]]  <- data.frame(target = md, ensemble = e, case = cs, year = yr, n_selected = n_sel, q = f$q, input = names(imp),
                                  importance = as.numeric(imp), perm_importance = as.numeric(perm[names(imp)]),
                                  oob_error = attr(imp, "oob_error") %||% NA_real_, oob_auc = attr(imp, "oob_auc") %||% NA_real_)
      cart_G[[key]] <- cbind(target = md, ensemble = e, case = cs, year = yr, cart$leaves)
      mem_G[[key]]  <- data.frame(target = md, ensemble = e, case = cs, year = yr, run_id = D$run_id, selected = y, combined = comb, D[, names(ind_lab)])
      com_G[[paste(key, "case")]] <- data.frame(target = md, ensemble = e, year = yr, column = cs, input = names(imp), importance = as.numeric(imp))
      # direction of each input on DI, EP, TP and the combined score
      dd <- bind_rows(lapply(names(met_lab), function(k) cbind(metric = k, sd_direction(X, if (k == "combined") comb else sd_adverse(k, D[[k]])))))
      dd <- bind_rows(dd, cbind(metric = "selected", sd_direction(X, y)))     # rho between each input and membership of the selected set (case-specific)
      dd$importance <- as.numeric(imp[dd$input])
      dir_G[[key]] <- cbind(target = md, ensemble = e, case = cs, year = yr, dd)
      # (1) importance + parallel axes, CART
      outs <- D[, names(ind_lab)]
      if (di_loss_is_negative && "di" %in% names(outs)) outs$di <- -outs$di          # DI shown as a loss: higher = worse on all three axes
      names(outs) <- unname(ind_lab[names(ind_lab)])
      if (di_loss_is_negative && "di" %in% names(ind_lab)) names(outs)[names(ind_lab) == "di"] <- "DI loss"
      keep <- names(imp)[!is.na(perm[names(imp)]) & perm[names(imp)] > min_perm_imp]   # only inputs with a non-negligible permutation importance
      if (length(keep) < 2) keep <- names(imp)[1:2]
      p <- sd_plot_parcoords_multi(X, y, outs, imp[keep], c("Other runs", "Selected runs"), top_n,
                                   title = sprintf("Filtered mode: %s, %s%s, %d (%d of %d runs selected)", cs, e, mode_lab(md), yr, n_sel, length(y)),
                                   fill = "#6929c4")
      save_fig(p, sprintf("04_filt_%s_%s_%d%s", slug(cs), e, yr, mode_sfx(md)), sub = "04_filtered_io", width = 11, height = 4.8)
      dir.create(file.path(path_fig, "04_filtered_cart"), showWarnings = FALSE, recursive = TRUE)
      png(file.path(path_fig, sprintf("04_filtered_cart/04_filt_cart_%s_%s_%d%s.png", slug(cs), e, yr, mode_sfx(md))), 3600, 1800, res = 250)
      plot(partykit::as.party(cart$fit), main = sprintf("CART (filtered mode): %s, %s%s, %d (class 1 = selected runs)", cs, e, mode_lab(md), yr),
           tp_args = list(id = FALSE), ip_args = list(id = FALSE), gp = grid::gpar(fontsize = 9))
      dev.off()
      # (2) the selected runs, ranked by combined adversity: input percentiles | outcomes
      sel <- which(y == 1); sel <- sel[order(comb[sel], decreasing = worst_first)]; top <- sel[seq_len(min(extreme_n, length(sel)))]
      vars <- names(imp)[seq_len(min(worst_inputs, length(imp)))]
      lab <- sprintf("run %s", D$run_id[top]); lab <- factor(lab, rev(lab))
      hi <- bind_rows(lapply(vars, function(v) data.frame(run = lab, input = factor(v, vars), pct = rank(X[[v]])[top] / (length(y) + 1))))
      g1 <- ggplot(hi, aes(input, run, fill = pct)) + geom_tile(colour = "white") +
        scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = .5, limits = c(0, 1), name = "Input value\n(percentile)") +
        labs(x = NULL, y = NULL, subtitle = "Inputs (top by importance for the selection)") + theme_paper(base_size = 9) +
        theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1))
      save_fig(g1 + labs(title = sprintf("%s: %d %s selected runs, %s%s, %d", cs, length(top), if (worst_first) "worst" else "best", e, mode_lab(md), yr),
                         subtitle = sprintf("Rows: selected runs, ordered from the %s to the %s overall outcome (DI, EP and TP together). Cells: input value as a percentile of the ensemble",
                                            if (worst_first) "worst" else "best", if (worst_first) "least bad" else "least good")),
               sprintf("04_filt_runs_%s_%s_%d%s", slug(cs), e, yr, mode_sfx(md)), sub = "04_filtered_runs", width = 8, height = 1.6 + 0.3 * extreme_n)
    }
  }
  # (3) direction heatmap, ONE figure per year and mode: panels = metric (DI loss, EP, TP, combined), columns = ensembles.
  #     rho does not depend on the case (it is computed on the whole ensemble), so cases are not faceted; they only choose which inputs to show.
  dir_all <- bind_rows(dir_G)
  for (md in unique(dir_all$target)) for (yr in unique(dir_all$year)) {
    dd <- dir_all |> filter(target == md, year == yr, metric != "selected"); if (!nrow(dd)) next
    ens <- unique(dd$ensemble)
    tp <- dd |> filter(metric == "combined") |> group_by(ensemble, case) |> slice_max(importance, n = top_n, with_ties = FALSE) |> ungroup() |> distinct(input)
    mets <- c(di = "DI loss", ep = "EP", tp = "TP", combined = "Combined")
    hp <- inner_join(dd |> distinct(target, ensemble, year, metric, input, rho, p), tp, by = "input") |>
      mutate(ensemble = factor(ensemble, ens), metric = factor(mets[metric], mets), star = ifelse(p < 0.01, "*", ""))
    ord <- hp |> filter(metric == "Combined") |> group_by(input) |> summarise(m = max(abs(rho)), .groups = "drop")
    hp$input <- factor(hp$input, ord$input[order(ord$m)])
    g <- ggplot(hp, aes(ensemble, input, fill = rho)) + geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f%s", rho, star)), size = 2.6) + facet_wrap(~metric, nrow = 1) +
      scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1, 1) * max(abs(hp$rho)), name = "Spearman\nrho") +
      labs(x = NULL, y = NULL, title = sprintf("Direction of the input effects, %d%s", yr, mode_lab(md)),
           subtitle = "Rows: union of the top inputs by importance across ensembles and cases. Red: a higher input value worsens the outcome (larger DI loss, higher EP, TP); blue: improves it. * p < 0.01") +
      theme_paper(base_size = 9) + theme(panel.grid = element_blank())
    save_fig(g, sprintf("04_filt_direction_metrics_%d%s", yr, mode_sfx(md)), sub = "04_filtered_direction", width = 3 + 1.0 * length(ens) * length(mets), height = 2 + 0.3 * nlevels(hp$input))
  }
  # (3b) direction by case: panels = cases, columns = ensembles; fill = Spearman rho between the input and being in the selected set
  #      (red: a higher input value makes a run MORE likely to be selected; blue: less likely). Best and Worst are roughly mirror images by construction.
  for (md in unique(dir_all$target)) for (yr in unique(dir_all$year)) {
    dd <- dir_all |> filter(target == md, year == yr, metric == "selected"); if (!nrow(dd)) next
    ens <- unique(dd$ensemble); cases <- unique(dd$case)
    tp <- dd |> group_by(ensemble, case) |> slice_max(importance, n = top_n, with_ties = FALSE) |> ungroup() |> distinct(input)
    hp <- inner_join(dd, tp, by = "input") |> mutate(ensemble = factor(ensemble, ens), case = factor(case, cases), star = ifelse(p < 0.01, "*", ""))
    ord <- hp |> group_by(input) |> summarise(m = max(importance), .groups = "drop")
    hp$input <- factor(hp$input, ord$input[order(ord$m)])
    g <- ggplot(hp, aes(ensemble, input, fill = rho)) + geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f%s", rho, star)), size = 2.8) + facet_wrap(~case, nrow = 1) +
      scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1, 1) * max(abs(hp$rho)), name = "Spearman rho\n(with selection)") +
      labs(x = NULL, y = NULL, title = sprintf("Direction of the input effects on being selected, %d%s", yr, mode_lab(md)),
           subtitle = "Rows: union of the top inputs by importance across ensembles and cases. Red: a higher input value makes a run more likely to be in the selected set; blue: less likely. * p < 0.01") +
      theme_paper(base_size = 9) + theme(panel.grid = element_blank())
    save_fig(g, sprintf("04_filt_direction_cases_%d%s", yr, mode_sfx(md)), sub = "04_filtered_direction", width = 3 + 1.1 * length(ens) * length(cases), height = 2 + 0.3 * nlevels(hp$input))
  }
  # (4) inputs that matter for all three metrics: RF importance for DI, EP, TP (worst adverse_pct %) and for each case
  com_all <- bind_rows(com_G)
  for (md in unique(com_all$target)) for (yr in unique(com_all$year)) {
    cc <- com_all |> filter(target == md, year == yr); if (!nrow(cc)) next
    ens <- unique(cc$ensemble); cols <- met_lab[1:3]; cols <- cols[cols %in% cc$column]
    sel <- cc |> filter(column %in% met_lab[1:3]) |> group_by(ensemble, input) |>
      summarise(m = if (n() == 3) min(importance) else 0, .groups = "drop") |>      # affects all three = high importance in the weakest of them
      group_by(ensemble) |> slice_max(m, n = 10, with_ties = FALSE) |> ungroup() |> mutate(rk = ave(-m, ensemble, FUN = rank))
    hp <- inner_join(cc |> filter(column %in% cols), sel, by = c("ensemble", "input")) |>
      mutate(ensemble = factor(ensemble, ens), column = factor(column, cols), key = paste(ensemble, input))
    hp$key <- reorder(hp$key, hp$m)
    g <- ggplot(hp, aes(column, key, fill = importance)) + geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.2f", importance)), size = 2.5) + facet_wrap(~ensemble, nrow = 1, scales = "free_y") +
      scale_y_discrete(labels = function(x) sub("^\\S+ ", "", x)) + scale_fill_importance(name = "RF\nimportance") +
      labs(x = NULL, y = NULL, title = sprintf("Inputs that matter for all three metrics, %s, %d%s", sc_region, yr, mode_lab(md)),
           subtitle = sprintf("Rows: the 10 inputs with the highest importance in the weakest of DI, EP, TP (sorted). Columns: RF importance for being in the worst %d%% of DI, EP and TP", 100 - adverse_pct)) +
      theme_paper(base_size = 9) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))
    save_fig(g, sprintf("04_filt_common_drivers_%d%s", yr, mode_sfx(md)), sub = "04_filtered_common_drivers", width = 3.5 + 3.4 * length(ens), height = 5.5)
  }
  write.csv(bind_rows(imp_G),  file.path(path_tab, "04_filtered_importance.csv"), row.names = FALSE)
  write.csv(bind_rows(cart_G), file.path(path_tab, "04_filtered_cart_leaves.csv"), row.names = FALSE)
  write.csv(bind_rows(mem_G),  file.path(path_tab, "04_filtered_runs.csv"), row.names = FALSE)
  write.csv(dir_all,           file.path(path_tab, "04_filtered_direction.csv"), row.names = FALSE)
  write.csv(com_all,           file.path(path_tab, "04_filtered_common_drivers.csv"), row.names = FALSE)
}
message("03 done. Figures: ", path_fig, "  Tables: ", path_tab)
