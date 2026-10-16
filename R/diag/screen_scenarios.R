# Screen EPPA price shocks of all ensembles for implausible scenarios (to be dropped as a whole).
# Rules (edit thresholds after looking at the printed distributions):
#   identical_prev : shocks identical to the previous computed year (price file duplicated / run stopped)
#   ne_out         : common non-energy price factor outside [ne_lo, ne_hi]
#   ne_jump        : non-energy factor changes by more than ne_jump between consecutive years
path_medusa <- "C:/Users/Jon Sampedro/Desktop/medusa"
devtools::load_all(path_medusa)
source("R/functions/run_ensemble.R"); source("config.R")
suppressMessages({ hbs <- readRDS("data/processed/hbs_2015_clean.rds") })
hbs_codes <- grep("^CP", names(hbs), value = TRUE)
yrs <- c(2025, 2030, 2035, 2040)
ne_lo <- 0.90; ne_hi <- 1.15; ne_jump <- 0.10

cw <- tapply(hbs$weight * hbs$CP00, hbs$country, sum); cw <- cw / sum(cw)

one_year <- function(ens, yr) {
  sh <- suppressMessages(medusa::build_coicop_shocks(medusa::get_eppa_prices(file.path(path_eppa, ens), yr),
          medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)
  M <- as.matrix(sh[, -1]); rownames(M) <- sh$coicop; M
}
agg <- function(x, M) {                       # country-weighted mean per scenario of a per-column vector
  cn <- sub("_.*", "", colnames(M)); sc <- sub(".*_", "", colnames(M))
  w <- cw[cn]; ok <- !is.na(w)
  tapply(x[ok] * w[ok], sc[ok], sum) / tapply(w[ok], sc[ok], sum)
}

out <- list()
for (ens in ensembles) {
  prev <- NULL
  for (yr in yrs) {
    M <- one_year(ens, yr)
    ne <- !grepl("^CP045|^CP0722", rownames(M))
    sh_ne <- colSums(hbs$weight * as.matrix(hbs[, rownames(M)[ne]])) / sum(hbs$weight * hbs$CP00)
    ne_f  <- agg(colSums(M[ne, , drop = FALSE] * sh_ne) / sum(sh_ne), M)
    fuel  <- agg(M["CP0722", ], M)
    d <- data.frame(ensemble = ens, year = yr, run_id = names(ne_f), ne_factor = as.numeric(ne_f),
                    fuel_factor = as.numeric(fuel[names(ne_f)]))
    if (!is.null(prev)) {
      same <- tapply(apply(abs(M - prev$M[, colnames(M), drop = FALSE]), 2, max), sub(".*_", "", colnames(M)), max)
      d$identical_prev <- as.numeric(same[d$run_id]) < 1e-9
      d$ne_change <- d$ne_factor - prev$ne[d$run_id]
    } else { d$identical_prev <- FALSE; d$ne_change <- NA_real_ }
    prev <- list(M = M, ne = setNames(d$ne_factor, d$run_id))
    out[[length(out) + 1]] <- d
  }
}
scr <- do.call(rbind, out)
scr$ne_out  <- scr$ne_factor < ne_lo | scr$ne_factor > ne_hi
scr$ne_jump <- !is.na(scr$ne_change) & abs(scr$ne_change) > ne_jump
scr$flag <- scr$identical_prev | scr$ne_out | scr$ne_jump

cat("\nDistribution of the non-energy factor (by ensemble and year):\n")
print(aggregate(ne_factor ~ ensemble + year, scr, function(x) round(quantile(x, c(0, .01, .5, .99, 1)), 3)))
cat("\nFlagged scenario-years:\n")
print(scr[scr$flag, c("ensemble","year","run_id","ne_factor","fuel_factor","identical_prev","ne_out","ne_jump")], row.names = FALSE)
drop <- unique(scr[scr$flag, c("ensemble", "run_id")])
cat("\nScenarios to drop per ensemble:\n"); print(table(drop$ensemble))
saveRDS(scr, "data/processed/screening_table.rds")
saveRDS(drop, "data/processed/flagged_scenarios.rds")
