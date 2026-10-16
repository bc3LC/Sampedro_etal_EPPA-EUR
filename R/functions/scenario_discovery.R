# R/functions/scenario_discovery.R
# Helpers for 03_scenario_discovery.R. R port of the JP-Data-Viz analysis (eppa_viz/analysis/mappings.py):
#   random forest (100 trees, depth 4) on a binary target = "worst 30% of runs", CART (depth 4),
#   permutation importance, k-means time-series clustering + binary RF per cluster.
# Only definitions here (nothing runs when sourced).

# ---- targets -------------------------------------------------------------------
# "Adversity": larger = worse for households. DI is a welfare change (negative = loss), EP/TP are rates.
sd_adverse <- function(ind, x) if (ind == "di") -x else x

# 1 = run is in the worst (100 - pct)% of the ensemble for this indicator (JP: threshold 70, gt = TRUE)
sd_target <- function(ind, x, pct = 70) {
  a <- sd_adverse(ind, x)
  as.integer(a > quantile(a, pct / 100, na.rm = TRUE))
}

# ---- inputs (EPPA uncertain parameters, one row per run) ----------------------------
# Design file = Inputs_Master.xlsx (sheet "samples": row 3 = input names, column 1 = run number, runs 1-400;
# the percentile rows below the runs are ignored). A csv with 'Run #' + one column per input also works.
# Region filter (as in JP analysis/inputs.py): keep {REG}/Non-{REG} GDP (or TFP) and Pop and AEEI {REG};
# for GLB keep the aggregate GDP/TFP/Pop and all AEEI columns. Global parameters (elasticities, fossil
# resources, technology costs, ...) are always kept.
sd_input_cols <- function(cols, region = "EUR") {
  allowed <- if (region == "GLB") c("TFP", "GDP", "Pop") else
    c(paste(region, c("TFP", "GDP", "Pop")), paste0("Non-", region, " ", c("TFP", "GDP", "Pop")))
  is_reg  <- grepl("(^| )(TFP|GDP|Pop)$", cols)
  is_aeei <- startsWith(cols, "AEEI ")
  drop <- (is_reg & !cols %in% allowed) | (is_aeei & region != "GLB" & cols != paste("AEEI", region))
  cols[!drop]
}

sd_read_inputs <- function(path, region = "EUR", sheet = "samples") {
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    x <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE))
    nm <- as.character(unlist(x[3, ])); x <- x[-(1:3), ]
    names(x) <- c("run_id", nm[-1])
    x <- x[!is.na(suppressWarnings(as.numeric(x$run_id))) & as.numeric(x$run_id) == round(as.numeric(x$run_id)), ]
    x <- x[, !is.na(names(x)) & names(x) != "NA", drop = FALSE]
  } else {
    x <- utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
    names(x)[1] <- "run_id"
  }
  x$run_id <- as.character(as.integer(x$run_id))
  x[-1] <- lapply(x[-1], function(v) suppressWarnings(as.numeric(v)))
  x <- as.data.frame(x[, c("run_id", sd_input_cols(names(x)[-1], region))])
  x <- x[, c(TRUE, vapply(x[-1], function(v) length(unique(v[!is.na(v)])) > 1, logical(1))), drop = FALSE]
  x[stats::complete.cases(x), ]
}

# ---- EPPA sector prices (outputs used for output-output mapping) --------------------
sd_eppa_secs <- c(elec = "Electricity", gas = "Gas", coal = "Coal", roil = "Refined oil", tran = "Transport",
                  serv = "Services", eint = "Energy-intensive ind.", othr = "Other industry", food = "Food",
                  crop = "Crops", live = "Livestock", fors = "Forestry")   # crude oil is dropped, as in MEDUSA

# long: run_id | year | sector | price   (sheet pol_EUR; Ref: ref_EUR)
sd_read_eppa_prices <- function(ens, dir = path_eppa, years = c(2020, 2025, 2030, 2035, 2040)) {
  sh <- if (ens == "Ref") "ref_EUR" else "pol_EUR"
  fs <- list.files(file.path(dir, ens), pattern = "price_.*\\.xlsx$", full.names = TRUE)
  fs <- fs[!grepl("^~\\$", basename(fs))]
  do.call(rbind, lapply(fs, function(f) {
    s <- sub(".*price_(\\w+?)_price.*", "\\1", basename(f))
    if (!s %in% names(sd_eppa_secs)) return(NULL)
    x <- suppressMessages(readxl::read_excel(f, sheet = sh, col_names = FALSE))
    y <- as.integer(unlist(x[1, -(1:4)])); v <- as.matrix(x[-1, -(1:4)]); mode(v) <- "numeric"
    d <- data.frame(run_id = as.character(as.integer(unlist(x[-1, 4]))), year = rep(y, each = nrow(v)),
                    sector = s, price = as.vector(v), stringsAsFactors = FALSE)
    d[d$year %in% years, ]
  }))
}

# ---- random forest / CART -----------------------------------------------------------
# X: data.frame of numeric predictors, y: 0/1. Returns impurity importance normalised to sum 1
# (like sklearn feature_importances_) and, optionally, permutation importance (OOB).
sd_rf <- function(X, y, trees = 100, depth = 4, seed = 0, perm = FALSE) {
  f <- ranger::ranger(x = X, y = factor(y), num.trees = trees, max.depth = depth, seed = seed,
                      importance = if (perm) "permutation" else "impurity", num.threads = 1)
  imp <- f$variable.importance
  if (!perm) imp <- imp / sum(imp)
  imp <- sort(imp, decreasing = TRUE)
  attr(imp, "oob_error") <- f$prediction.error      # out-of-bag misclassification rate (NOT informative with a 70/30 split: ~0.30 = predict 'other' always)
  if (!perm) attr(imp, "oob_auc") <- sd_oob_auc(X, y, trees, depth, seed)
  imp
}

# Out-of-bag AUC of the forest (0.5 = no skill, 1 = perfect). Use this, not the misclassification rate, to judge
# whether a forest explains anything: with 30% 'worst' runs a forest can predict 'other' for every run and still score 0.30.
sd_oob_auc <- function(X, y, trees = 100, depth = 4, seed = 0) {
  f <- ranger::ranger(x = X, y = factor(y, levels = c(0, 1)), num.trees = trees, max.depth = depth, seed = seed,
                      probability = TRUE, num.threads = 1)
  p <- f$predictions[, "1"]; ok <- !is.na(p); p <- p[ok]; yy <- y[ok]
  n1 <- sum(yy == 1); n0 <- sum(yy == 0)
  (sum(rank(p)[yy == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# CART depth 4 (rpart, no pruning; leaves have at least `minbucket` runs, so no 1-run 'boxes'). Leaves: rule, n, n_interest, density, coverage (as in JP tree.py)
sd_cart <- function(X, y, depth = 4, minbucket = 10) {
  d <- data.frame(y = factor(y, levels = c(0, 1)), X, check.names = FALSE)
  names(d) <- c("y", names(X))
  fit <- rpart::rpart(y ~ ., data = d, method = "class",
                      control = rpart::rpart.control(maxdepth = depth, cp = 0, minsplit = 2 * minbucket, minbucket = minbucket, xval = 0))
  fr <- fit$frame; leaf <- rownames(fr)[fr$var == "<leaf>"]
  cnt <- fr[fr$var == "<leaf>", "yval2"][, 2:3, drop = FALSE]    # counts class 0, class 1
  tot1 <- sum(y == 1)
  rules <- vapply(leaf, function(nd) {
    p <- rpart::path.rpart(fit, as.integer(nd), print.it = FALSE)[[1]]
    paste(p[-1], collapse = " & ")
  }, character(1))
  leaves <- data.frame(node = as.integer(leaf), rule = unname(rules), n = rowSums(cnt), n_interest = cnt[, 2],
                       density = cnt[, 2] / rowSums(cnt), coverage = if (tot1 > 0) cnt[, 2] / tot1 else NA_real_)
  list(fit = fit, leaves = leaves[order(-leaves$density * leaves$coverage), ])
}

# ---- plots --------------------------------------------------------------------------
# Left: top-n importance bars. Right: parallel axes of the top-n inputs + the outcome, coloured by class.
sd_plot_parcoords <- function(X, y, outcome, imp, outcome_label, class_labels, top_n = 5, title = NULL, fill = "#0b3d91") {
  top <- names(imp)[seq_len(min(top_n, length(imp)))]
  bar <- data.frame(var = factor(top, rev(top)), imp = imp[top])
  p1 <- ggplot(bar, aes(imp, var)) + geom_col(fill = fill, width = .7) +
    labs(x = "Feature importance", y = NULL, subtitle = paste("Top", length(top), "features")) + theme_paper(base_size = 10)
  pc <- X[, top, drop = FALSE]; pc[[outcome_label]] <- outcome
  pc$grp <- factor(y, c(0, 1), class_labels)
  p2 <- GGally::ggparcoord(pc, columns = seq_len(ncol(pc) - 1), groupColumn = "grp", scale = "uniminmax",
                           alphaLines = .35, showPoints = FALSE) +
    scale_colour_manual(values = c("grey60", "#D55E00"), name = NULL) +
    labs(x = NULL, y = "Scaled value (min-max)", subtitle = "Parallel axes") + theme_paper(base_size = 10) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  patchwork::wrap_plots(p1, p2, widths = c(.4, .6)) + patchwork::plot_annotation(title = title)
}

# ---- time-series clustering ---------------------------------------------------------
# k-means on the raw trajectories (JP: tslearn TimeSeriesKMeans, euclidean). Clusters are renumbered so
# that Cluster 1 = most adverse terminal year, then 2, 3 ...
sd_cluster <- function(M, ind, k = 3, seed = 0, nstart = 25) {
  set.seed(seed)
  km <- stats::kmeans(M, centers = k, nstart = nstart)
  ord <- order(sd_adverse(ind, km$centers[, ncol(km$centers)]), decreasing = TRUE)
  list(cluster = stats::setNames(match(km$cluster, ord), rownames(M)), centers = km$centers[ord, , drop = FALSE])
}

# ---- direction and shape of the effects (block E) ---------------------------------------
# Spearman correlation of every input with the adversity of the outcome (sd_adverse: larger = worse for households):
# rho > 0 = a higher value of the input makes the outcome worse. Returns input, rho, p (sorted by |rho|).
sd_direction <- function(X, adv) {
  r <- vapply(X, function(v) suppressWarnings(stats::cor.test(v, adv, method = "spearman", exact = FALSE)[c("estimate", "p.value")] |> unlist()), numeric(2))
  d <- data.frame(input = names(X), rho = unname(r[1, ]), p = unname(r[2, ]))
  d[order(-abs(d$rho)), ]
}

# Regression forest on the outcome itself (original units). Used for partial dependence: shape of the effect.
sd_rf_reg <- function(X, v, trees = 300, seed = 0)
  ranger::ranger(x = X, y = v, num.trees = trees, min.node.size = 5, seed = seed, num.threads = 1)

# 1-D partial dependence of the fitted forest for `vars`, on a grid between the 5th and 95th percentile of each input.
sd_pdp <- function(fit, X, vars, n_grid = 20, n_sub = 200, seed = 0) {
  set.seed(seed); S <- X[sample(nrow(X), min(n_sub, nrow(X))), , drop = FALSE]
  do.call(rbind, lapply(vars, function(v) {
    g <- seq(stats::quantile(X[[v]], .05), stats::quantile(X[[v]], .95), length.out = n_grid)
    pd <- vapply(g, function(x) { S[[v]] <- x; mean(stats::predict(fit, S, num.threads = 1)$predictions) }, numeric(1))
    data.frame(input = v, x = g, pd = pd)
  }))
}

# 2-D partial dependence for a pair of inputs (interaction).
sd_pdp2 <- function(fit, X, v1, v2, n_grid = 15, n_sub = 150, seed = 0) {
  set.seed(seed); S <- X[sample(nrow(X), min(n_sub, nrow(X))), , drop = FALSE]
  g1 <- seq(stats::quantile(X[[v1]], .05), stats::quantile(X[[v1]], .95), length.out = n_grid)
  g2 <- seq(stats::quantile(X[[v2]], .05), stats::quantile(X[[v2]], .95), length.out = n_grid)
  grid <- expand.grid(x1 = g1, x2 = g2)
  grid$pd <- vapply(seq_len(nrow(grid)), function(i) { S[[v1]] <- grid$x1[i]; S[[v2]] <- grid$x2[i]
    mean(stats::predict(fit, S, num.threads = 1)$predictions) }, numeric(1))
  grid$var1 <- v1; grid$var2 <- v2
  grid
}
