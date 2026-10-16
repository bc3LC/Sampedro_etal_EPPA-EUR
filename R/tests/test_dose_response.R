# Dose-response sanity check: scaling the price changes must scale DI, EP and TP monotonically.
path_medusa <- "C:/Users/Jon Sampedro/Desktop/medusa"
devtools::load_all(path_medusa)
source("R/functions/run_ensemble.R"); source("config.R")

hbs <- readRDS("data/processed/hbs_2015_clean.rds")
hbs_codes <- grep("^CP", names(hbs), value = TRUE)
prices <- medusa::get_eppa_prices(file.path(path_eppa, "2C"), 2030)
sh <- medusa::build_coicop_shocks(prices, medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa,
                                  EU_NO_AT, hbs_codes, verbose = FALSE)$shocks

# a few scenarios; each gets scaled by k
ids <- c("1", "100", "200", "300")
ks  <- c(0, 0.5, 1, 1.5, 2)
res <- list()
for (id in ids) {
  s0 <- chunk_shocks_eu(sh, id)
  for (k in ks) {
    s <- s0; num <- setdiff(names(s), "coicop"); s[, num] <- 1 + k * (s0[, num] - 1)
    r <- indicators_eu(hbs, s, ep_index = ep_index, tp_index = tp_index, tp_exclude = tp_exclude)
    res[[length(res) + 1]] <- data.frame(scenario = id, k = k, di = r$scen$di, ep = r$scen$ep, tp = r$scen$tp,
                                         ep_base = unname(r$base["ep"]), tp_base = unname(r$base["tp"]))
  }
}
res <- do.call(rbind, res)
print(res, digits = 4)

# checks
chk <- function(x) all(diff(x) <= 1e-12)            # DI is negative for losses -> must decrease with k
chk_up <- function(x) all(diff(x) >= -1e-12)        # poverty must not fall with k
for (id in ids) {
  d <- res[res$scenario == id, ]
  cat(sprintf("scenario %s | DI decreasing: %s | EP non-decreasing: %s | TP non-decreasing: %s | k=0 -> DI=%.2g, EP-base=%.2g, TP-base=%.2g\n",
      id, chk(d$di), chk_up(d$ep), chk_up(d$tp), d$di[1], d$ep[1]-d$ep_base[1], d$tp[1]-d$tp_base[1]))
}
