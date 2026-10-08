# R/functions/sd_helpers.R
# Scenario-discovery helpers. They mirror the JP-Data-Viz (Python) workflow:
#   eppa_viz/analysis/mappings.py   (InputOutputMapping, TimeSeriesClustering)
#   eppa_viz/analysis/targets.py    (percentile -> binary target)
#
# JP-Data-Viz settings reproduced here (so results are comparable):
#   - binary target: run is "of interest" if outcome > 70th percentile (threshold = 70, gt = TRUE)
#   - CART: depth 4, gini
#   - random forest: 100 trees, max depth 4, sqrt(p) features per split
#   - permutation importance: 10 repeats, on the training data, keep inputs with mean - 3*sd > 0
#   - clustering: k-means (k = 3, Euclidean) on the run x year matrix, clusters ordered by final-year value
#   - random seed 0

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ranger)
  library(rpart)
})

SD_SEED         <- 0
SD_THRESHOLD    <- 70     # percentile
SD_N_TREES      <- 100
SD_MAX_DEPTH    <- 4
SD_CART_DEPTH   <- 4
SD_N_REPEATS    <- 10
SD_TOP_N        <- 5

# ---- 1. Binary target ---------------------------------------------------------
# np.percentile (linear interpolation) == quantile(type = 7), the R default.
make_target <- function(values, threshold = SD_THRESHOLD, gt = TRUE) {
  cutoff <- quantile(values, probs = threshold / 100, type = 7, names = FALSE)
  as.integer(if (gt) values > cutoff else values < cutoff)
}

# ---- 2. Analysis table: design matrix + outcome per run -----------------------
# design  : data.frame, run_id + one column per varying parameter
# outcome : data.frame, run_id + value  (one row per run for the chosen indicator/group/year)
build_sd_table <- function(design, outcome, threshold = SD_THRESHOLD, gt = TRUE) {
  d <- inner_join(design, outcome[, c("run_id", "value")], by = "run_id") |>
    filter(!is.na(value))
  d$target <- make_target(d$value, threshold, gt)
  d
}

input_cols <- function(d) setdiff(names(d), c("run_id", "value", "target", "cluster"))

# ---- 3. CART (rpart set up to behave like sklearn's DecisionTreeClassifier) ----
fit_cart <- function(d, depth = SD_CART_DEPTH) {
  set.seed(SD_SEED)
  rpart(
    reformulate(input_cols(d), response = "factor(target)"),
    data = d, method = "class", parms = list(split = "gini"),
    control = rpart.control(maxdepth = depth, cp = 0, minsplit = 2, minbucket = 1,
                            xval = 0, maxcompete = 0, maxsurrogate = 0)
  )
}

# ---- 4. Random forest (ranger) ------------------------------------------------
fit_rf <- function(d, n_trees = SD_N_TREES, max_depth = SD_MAX_DEPTH, y = "target") {
  x <- d[, input_cols(d), drop = FALSE]
  dat <- cbind(x, .y = factor(d[[y]]))
  ranger(.y ~ ., data = dat, num.trees = n_trees, max.depth = max_depth,
         importance = "impurity", seed = SD_SEED)
}

# Impurity importance, normalised to sum to 1 (comparable to sklearn feature_importances_)
rf_importance <- function(rf, top_n = SD_TOP_N) {
  imp <- rf$variable.importance
  imp <- imp / sum(imp)
  out <- tibble(input = names(imp), importance = as.numeric(imp)) |>
    arrange(desc(importance))
  if (!is.null(top_n)) out <- head(out, top_n)
  out
}

# ---- 5. Permutation importance (as in InputOutputMapping.permutation_importance) ----
# Accuracy drop when one input is shuffled; 10 repeats; evaluated on the training data.
perm_importance <- function(d, n_repeats = SD_N_REPEATS, keep_significant = TRUE, ...) {
  rf <- fit_rf(d, ...)
  x  <- d[, input_cols(d), drop = FALSE]
  y  <- d$target
  acc <- function(newx) mean(as.integer(as.character(predict(rf, data = newx)$predictions)) == y)
  base <- acc(x)
  set.seed(SD_SEED)
  res <- lapply(names(x), function(v) {
    drops <- replicate(n_repeats, {
      xp <- x
      xp[[v]] <- sample(xp[[v]])
      base - acc(xp)
    })
    tibble(input = v, mean = mean(drops), sd = sd(drops))
  }) |> bind_rows() |> arrange(desc(mean))
  # sklearn uses population sd (ddof = 0) for importances_std
  res$sd <- res$sd * sqrt((n_repeats - 1) / n_repeats)
  if (keep_significant) res <- filter(res, mean - 3 * sd > 0)
  res
}

# ---- 6. Time-series clustering + one-vs-rest forest per cluster ----------------
# ts : data.frame run_id, year, value  (one outcome trajectory per run)
cluster_runs <- function(ts, k = 3) {
  wide <- ts |> arrange(year) |>
    pivot_wider(names_from = year, values_from = value) |>
    arrange(run_id)
  m <- as.matrix(wide[, -1])
  set.seed(SD_SEED)
  km <- kmeans(m, centers = k, nstart = 10)
  # order clusters by mean final-year value (cluster 1 = lowest), like reorder_clusters_by_terminal_year
  ord <- order(km$centers[, ncol(km$centers)])
  lab <- match(km$cluster, ord)
  tibble(run_id = wide$run_id, cluster = lab)
}

cluster_importance <- function(design, clusters, cluster_id, top_n = SD_TOP_N) {
  d <- inner_join(design, clusters, by = "run_id")
  d$in_cluster <- as.integer(d$cluster == cluster_id)
  d$target <- d$in_cluster
  rf <- fit_rf(d |> select(-cluster, -in_cluster))
  rf_importance(rf, top_n) |> mutate(cluster = cluster_id)
}
