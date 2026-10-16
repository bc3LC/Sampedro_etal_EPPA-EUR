# Diagnose (a) scenarios with positive DI, (b) outliers, (c) identical 2035/2040 results.
# Run from the repo project. Prints which commodity price changes drive DI in chosen scenarios.
path_medusa <- "C:/Users/Jon Sampedro/Desktop/medusa"
devtools::load_all(path_medusa)
source("R/functions/run_ensemble.R"); source("config.R")
suppressMessages({
hbs <- readRDS("data/processed/hbs_2015_clean.rds")
hbs_codes <- grep("^CP", names(hbs), value = TRUE)
})
ens  <- "2C"
ids  <- c("14", "130", "337", "257", "223", "11", "2")      # outliers / positive-DI cases (edit)
yrs  <- c(2030, 2035, 2040)

shocks_year <- function(yr) suppressMessages(medusa::build_coicop_shocks(
  medusa::get_eppa_prices(file.path(path_eppa, ens), yr),
  medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)
SH <- lapply(setNames(yrs, yrs), shocks_year)

# ---- (1) are the 2035 and 2040 shocks identical for the "duplicated" scenarios? ----------
cat("\nMax |shock(2040) - shock(2035)| per scenario (0 = identical prices in both years)\n")
for (id in ids) {
  a <- chunk_shocks_eu(SH[["2035"]], id); b <- chunk_shocks_eu(SH[["2040"]], id)
  cat(sprintf("  scenario %-4s %.6f\n", id, max(abs(as.matrix(a[, -1]) - as.matrix(b[, -1])))))
}

# ---- (2) which price changes drive DI? (budget share x (shock - 1), EU, % points) --------
w   <- hbs$weight
cty <- hbs$country
codes <- SH[[1]]$coicop
share_c <- sapply(split(seq_len(nrow(hbs)), cty), function(i)
  colSums(w[i] * as.matrix(hbs[i, codes])) / sum(w[i] * hbs$CP00[i]))          # codes x countries
Wc <- tapply(w * hbs$CP00, cty, sum); Wc <- Wc / sum(Wc)

drivers <- function(yr, id) {
  s <- chunk_shocks_eu(SH[[as.character(yr)]], id)
  cn <- sub("_.*", "", setdiff(names(s), "coicop"))
  M  <- as.matrix(s[, -1]); rownames(M) <- s$coicop
  contrib <- rowSums(sapply(seq_along(cn), function(j) {
    if (!cn[j] %in% colnames(share_c)) return(rep(0, length(codes)))
    -100 * share_c[codes, cn[j]] * (M[codes, j] - 1) * Wc[cn[j]] }))
  data.frame(coicop = codes, mean_shock = round(rowMeans(M)[codes], 3),
             contribution_to_DI = round(contrib, 3))
}
for (id in ids) for (yr in yrs) {
  d <- drivers(yr, id)
  cat(sprintf("\n=== scenario %s, %d: approx. DI = %.2f (MEDUSA's DI differs slightly: ratio of means) ===\n",
              id, yr, sum(d$contribution_to_DI)))
  print(head(d[order(-abs(d$contribution_to_DI)), ], 8), row.names = FALSE)
}
