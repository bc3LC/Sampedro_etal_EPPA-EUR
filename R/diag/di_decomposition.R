# ==============================================================================
# R/diag/di_decomposition.R
# Which price changes drive the EU-level distributional impact (DI)?
#
# DI of household h = -100 * sum_c  exp_hc * (f_c - 1) / CP00_h          (f_c = shock of COICOP code c)
# EU DI = weighted mean over households  ->  DI = -100 * sum_{c, country} A[c, country] * (f[c, country] - 1)
# with the exposure A[c, country] = sum_h w_h * exp_hc / CP00_h / sum_h w_h. It reproduces
# medusa::di_total_eu() exactly (checked below), and splits DI by commodity group.
#
# Outputs (outputs/tables/, outputs/figures/0_decomposition/):
#   diag_di_by_variant.csv        DI per ensemble, year, scenario and price assumption
#   diag_di_contrib_by_group.csv  DI split by commodity group (percentage points), per scenario
#   diag_di_decomposition.png / diag_di_variants.png
# Run from the project root, AFTER screen_scenarios.R (the figures drop the flagged scenarios). Needs only the prices and the HBS (not 01_run_medusa.R).
# ==============================================================================
source(here::here("config.R"))
devtools::load_all(path_medusa)
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

yrs  <- years_calc                      # 2025, 2030, 2035, 2040
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)

hbs       <- readRDS(file.path(path_proc, "hbs_2015_clean.rds"))
hbs_codes <- grep("^CP", names(hbs), value = TRUE)
ctry <- as.character(hbs$country); w <- as.numeric(hbs$weight); cp00 <- as.numeric(hbs$CP00)
countries <- unique(ctry)

shocks_of <- function(ens, yr) suppressMessages(medusa::build_coicop_shocks(
  medusa::get_eppa_prices(file.path(path_eppa, ens), yr),
  medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)

# ---- codes that MEDUSA shocks (same selection as price_shock_eu) and exposure matrix A -------------------
sh0 <- shocks_of(ensembles[1], yrs[1])
coicop_cols <- setdiff(grep("^CP\\d+$", names(hbs), value = TRUE), "CP00")
usable <- !(sh0$coicop %in% sh0$coicop[duplicated(sh0$coicop)]) & sh0$coicop %in% coicop_cols
codesB <- sh0$coicop[usable]; rowsB <- which(usable)
A <- sapply(countries, function(cn) { i <- which(ctry == cn)
  m <- as.matrix(hbs[i, codesB, drop = FALSE]); m[is.na(m)] <- 0
  colSums(m * (w[i] / cp00[i])) / sum(w) })
rownames(A) <- codesB

grp <- function(cd) dplyr::case_when(
  grepl("^CP0451", cd) ~ "Electricity",       grepl("^CP0452", cd) ~ "Gas",
  grepl("^CP045[34]", cd) ~ "Liquid/solid fuels", grepl("^CP0455", cd) ~ "Heat",
  grepl("^CP0722", cd) ~ "Transport fuels",   grepl("^CP01", cd) ~ "Food",
  grepl("^CP04", cd) ~ "Other housing",       grepl("^CP07", cd) ~ "Other transport",
  TRUE ~ "Other goods & services")
G <- grp(codesB); energy <- grepl("^CP045|^CP0722", codesB)

# shocks as an array [code, country, scenario]; missing = no shock (1), as in MEDUSA
shocks_arr <- function(ens, yr) {
  sh <- as.data.frame(shocks_of(ens, yr)); cols <- setdiff(names(sh), "coicop")
  sc <- unique(sub(".*_", "", cols)); present <- countries[countries %in% unique(sub("_.*", "", cols))]
  arr <- array(1, c(length(codesB), length(countries), length(sc)), dimnames = list(codesB, countries, sc))
  for (cn in present) { m <- as.matrix(sh[rowsB, paste0(cn, "_", sc), drop = FALSE]); m[is.na(m)] <- 1; arr[, cn, ] <- m }
  arr
}
di_of          <- function(arr) -100 * apply(arr, 3, function(f) sum(A * (f - 1)))
contrib_groups <- function(arr) -100 * apply(arr, 3, function(f) tapply(rowSums(A * (f - 1)), G, sum))

# Price assumptions (what-if variants of the same shocks)
variants <- list(
  baseline          = function(arr) arr,
  gas_fixed         = function(arr) { arr[grepl("^CP0452", codesB), , ] <- 1; arr },          # household gas price unchanged
  solid_fixed       = function(arr) { arr[grepl("^CP0454", codesB), , ] <- 1; arr },          # solid fuels (coal) unchanged
  gas_solid_fixed   = function(arr) { arr[grepl("^CP045[24]", codesB), , ] <- 1; arr },
  energy_only       = function(arr) { arr[!energy, , ] <- 1; arr },                           # only energy prices move
  nonenergy_only    = function(arr) { arr[energy, , ] <- 1; arr },                            # only non-energy prices move
  numeraire_neutral = function(arr) {                                                         # all shocks / non-energy price index (country x scenario)
    out <- arr
    for (cn in dimnames(arr)[[2]]) { a <- A[!energy, cn]
      for (s in dimnames(arr)[[3]]) out[, cn, s] <- arr[, cn, s] / (sum(a * arr[!energy, cn, s]) / sum(a)) }
    out })

res <- list(); cg <- list()
for (e in ensembles) for (y in yrs) {
  arr <- shocks_arr(e, y)
  if (e == ensembles[1] && y == yrs[1]) {                                  # check against MEDUSA
    sh <- shocks_of(e, y); keep <- c("coicop", grep("_(1|2|3)$", names(sh), value = TRUE))
    ref <- medusa::di_total_eu(hbs, sh[, keep])
    message("Check vs medusa::di_total_eu: max abs difference = ", signif(max(abs(di_of(arr)[names(ref)] - ref)), 3))
  }
  for (v in names(variants)) { d <- di_of(variants[[v]](arr))
    res[[length(res) + 1]] <- data.frame(ensemble = e, year = y, variant = v, run_id = names(d), di = as.numeric(d)) }
  m <- contrib_groups(arr)
  cg[[length(cg) + 1]] <- data.frame(ensemble = e, year = y, run_id = colnames(m), t(m), check.names = FALSE, row.names = NULL)
  message(e, " ", y, " done")
}
res <- bind_rows(res); cg <- bind_rows(cg)
write.csv(res, file.path(path_tab, "diag_di_by_variant.csv"), row.names = FALSE)
write.csv(cg,  file.path(path_tab, "diag_di_contrib_by_group.csv"), row.names = FALSE)

# ---- figures (flagged scenarios removed if drop_flagged_scenarios = TRUE) -------------------------------
res <- drop_flagged(res |> mutate(ensemble = factor(ensemble, ensembles)))
cg  <- drop_flagged(cg  |> mutate(ensemble = factor(ensemble, ensembles)))
ens_cols <- c("2C" = "#0072B2", "1p5C" = "#009E73", "Ref" = "#E69F00", "PF" = "#CC79A7")
ens_lab  <- c("2C" = "2°C", "1p5C" = "1.5°C", "Ref" = "Reference", "PF" = "PF")
groups   <- c("Electricity", "Gas", "Liquid/solid fuels", "Transport fuels", "Food", "Other housing", "Other transport", "Other goods & services")

d1 <- cg |> pivot_longer(-c(ensemble, year, run_id), names_to = "group", values_to = "pp") |>
  filter(group %in% groups) |> group_by(ensemble, year, group) |> summarise(pp = median(pp), .groups = "drop") |>
  mutate(group = factor(group, groups))
tot <- res |> filter(variant == "baseline") |> group_by(ensemble, year) |> summarise(di = median(di), .groups = "drop")
p1 <- ggplot() +
  geom_col(data = d1, aes(year, pp, fill = group), width = 3.2) +
  geom_line(data = tot, aes(year, di), linewidth = .6) + geom_point(data = tot, aes(year, di), size = 1.6) +
  geom_hline(yintercept = 0, linewidth = .3) +
  scale_fill_manual(values = c("#d9a800", "#e07b39", "#8c564b", "#2a78d6", "#1baf7a", "#9aa0a6", "#7fb3e0", "#c9c4b8")) +
  scale_x_continuous(breaks = yrs) +
  facet_wrap(~ ensemble, nrow = 1, labeller = labeller(ensemble = ens_lab)) +
  labs(x = NULL, y = "Contribution to DI (percentage points)", fill = NULL,
       caption = "Bars: median contribution of each commodity group; line: median total DI.") + theme_paper()
save_fig(p1, "diag_di_decomposition", sub = "0_decomposition", width = 11, height = 4.2)

vlab <- c(baseline = "Baseline", energy_only = "Energy prices only", nonenergy_only = "Non-energy prices only",
          numeraire_neutral = "Relative prices (shocks / non-energy index)", gas_solid_fixed = "Gas and solid fuels unchanged")
d2 <- res |> filter(variant %in% names(vlab)) |> group_by(ensemble, year, variant) |> summarise(di = median(di), .groups = "drop") |>
  mutate(variant = factor(variant, names(vlab), vlab))
p2 <- ggplot(d2, aes(year, di, colour = variant, linetype = variant)) + geom_line(linewidth = .8) + geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linewidth = .3) + scale_x_continuous(breaks = yrs) +
  facet_wrap(~ ensemble, nrow = 1, labeller = labeller(ensemble = ens_lab)) +
  scale_colour_manual(values = c("black", "#e07b39", "#2a78d6", "#1baf7a", "#7a4fbf")) +
  scale_linetype_manual(values = c(1, 1, 1, 1, 2)) +
  labs(x = NULL, y = "Median DI (% of expenditure)", colour = NULL, linetype = NULL) +
  theme_paper() + guides(colour = guide_legend(nrow = 2))
save_fig(p2, "diag_di_variants", sub = "0_decomposition", width = 11, height = 4.6)

# ==============================================================================
# By EPPA sector (and services by COICOP division)
# Shock = 1 + sum_s w_cs (p_s - 1) is linear in the sector prices, so the contribution of one EPPA sector is
# DI computed with all the other sectors' prices set to 1; the sectors add up to the total DI exactly.
# ==============================================================================
bld <- function(pr) suppressMessages(medusa::build_coicop_shocks(pr, medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa,
                                                                 EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)
arr_of <- function(sh) {
  sh <- as.data.frame(sh); cols <- setdiff(names(sh), "coicop"); sc <- unique(sub(".*_", "", cols))
  present <- countries[countries %in% unique(sub("_.*", "", cols))]
  arr <- array(1, c(length(codesB), length(countries), length(sc)), dimnames = list(codesB, countries, sc))
  for (cn in present) { m <- as.matrix(sh[rowsB, paste0(cn, "_", sc), drop = FALSE]); m[is.na(m)] <- 1; arr[, cn, ] <- m }
  arr
}
division <- function(cd) sub("^CP(\\d\\d).*", "\\1", cd)
by_sector <- list(); serv_div <- list()
for (e in ensembles) for (y in yrs) {
  pr <- medusa::get_eppa_prices(file.path(path_eppa, e), y); total <- di_of(arr_of(bld(pr))); acc <- 0
  for (s in unique(pr$eppa_sector)) {
    p <- pr; p$value[p$eppa_sector != s] <- 1
    arr <- arr_of(bld(p)); d <- di_of(arr); acc <- acc + d
    by_sector[[length(by_sector) + 1]] <- data.frame(ensemble = e, year = y, sector = s, run_id = names(d), contrib = as.numeric(d))
    if (s == "SERV") {
      pc <- -100 * apply(arr, 3, function(f) rowSums(A * (f - 1)))               # code x scenario
      dv <- rowsum(pc, division(codesB))
      serv_div[[length(serv_div) + 1]] <- data.frame(ensemble = e, year = y, division = rep(rownames(dv), ncol(dv)),
                                                     run_id = rep(colnames(dv), each = nrow(dv)), contrib = as.vector(dv))
    }
  }
  message(e, " ", y, ": sectors add up to the total (max gap ", signif(max(abs(acc - total)), 2), ")")
}
by_sector <- bind_rows(by_sector); serv_div <- bind_rows(serv_div)
write.csv(by_sector, file.path(path_tab, "diag_di_contrib_by_eppa_sector.csv"), row.names = FALSE)
write.csv(serv_div,  file.path(path_tab, "diag_di_contrib_services_by_coicop_division.csv"), row.names = FALSE)

by_sector <- drop_flagged(by_sector |> mutate(ensemble = factor(ensemble, ensembles)))
serv_div  <- drop_flagged(serv_div  |> mutate(ensemble = factor(ensemble, ensembles)))
sec_order <- c("ELEC", "ROIL", "GAS", "COAL", "TRAN", "FOOD", "CROP", "LIVE", "FORS", "EINT", "OTHR", "SERV")
sec_lab   <- c(ELEC = "Electricity", ROIL = "Refined oil", GAS = "Gas", COAL = "Coal", TRAN = "Transport", FOOD = "Food",
               CROP = "Crops", LIVE = "Livestock", FORS = "Forestry", EINT = "Energy-intensive industry",
               OTHR = "Other industry", SERV = "Services")
d3 <- by_sector |> filter(sector %in% sec_order) |> group_by(ensemble, year, sector) |> summarise(pp = median(contrib), .groups = "drop") |>
  mutate(sector = factor(sector, sec_order, sec_lab[sec_order]))
tot3 <- by_sector |> group_by(ensemble, year, run_id) |> summarise(t = sum(contrib), .groups = "drop") |>
  group_by(ensemble, year) |> summarise(t = median(t), .groups = "drop")
p3 <- ggplot() + geom_col(data = d3, aes(year, pp, fill = sector), width = 3.2) +
  geom_line(data = tot3, aes(year, t)) + geom_point(data = tot3, aes(year, t), size = 1.6) + geom_hline(yintercept = 0, linewidth = .3) +
  scale_fill_manual(values = c("#d9a800", "#2a78d6", "#e07b39", "#8c564b", "#7fb3e0", "#1baf7a", "#7fc9a5", "#b5d99c",
                               "#9aa0a6", "#c9c4b8", "#dcd8cc", "#b2182b")) +
  scale_x_continuous(breaks = c(2030, 2035, 2040)) +
  facet_wrap(~ ensemble, nrow = 1, labeller = labeller(ensemble = ens_lab)) +
  labs(x = NULL, y = "Contribution to DI (percentage points)", fill = NULL,
       caption = "Median across scenarios; line: median total DI.") + theme_paper()
save_fig(p3, "diag_di_by_eppa_sector", sub = "0_decomposition", width = 11, height = 4.6)

div_lab <- c("01" = "Food & non-alc. beverages", "02" = "Alcohol & tobacco", "03" = "Clothing", "04" = "Housing, water, energy",
             "05" = "Furnishings", "06" = "Health", "07" = "Transport", "08" = "Communication", "09" = "Recreation",
             "10" = "Education", "11" = "Restaurants & hotels", "12" = "Other goods & services")
d4 <- serv_div |> group_by(ensemble, year, division) |> summarise(pp = median(contrib), .groups = "drop") |>
  mutate(division = factor(division, names(div_lab), paste(names(div_lab), div_lab)))
p4 <- ggplot(d4, aes(year, pp, fill = division)) + geom_col(width = 3.2) + geom_hline(yintercept = 0, linewidth = .3) +
  scale_x_continuous(breaks = c(2030, 2035, 2040)) +
  facet_wrap(~ ensemble, nrow = 1, labeller = labeller(ensemble = ens_lab)) +
  labs(x = NULL, y = "Services contribution to DI (percentage points)", fill = NULL) + theme_paper() +
  guides(fill = guide_legend(nrow = 3))
save_fig(p4, "diag_di_services_by_coicop_division", sub = "0_decomposition", width = 11, height = 5)
message("di_decomposition.R finished")
