# ==============================================================================
# 02_figures_results.R
# MEDUSA outputs and diagnostics (scenario discovery / ML and its figures: 03_*.R)
#
# Reads : outputs/platform/{di,ep,tp}_<ens>.xlsx   (from 01_run_medusa.R)
#         EPPA price files (data/eppa_outputs/<ens>) and the cleaned HBS
# Writes: data/processed/medusa_long.rds, price_index_long.rds, di_groups_<year>.rds (per year, and per ensemble)
#         outputs/figures/02_*.png (+ subfolders 02_prices, 02_di_groups)
#
# Main year: sel_year = 2030 (diagnostic figure). The DI-by-group figures (decile, gender/zone, country) are made for every year in yrs_groups.
# ==============================================================================
source(here::here("config.R"))
devtools::load_all(path_medusa)
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2)
                                 library(readxl); library(scales) })

sel_year   <- 2030
yrs_groups <- c(2025, 2030, 2035, 2040)   # years of the DI-by-group figures (one set of figures per year)
yrs_prices <- c(2025, 2030, 2035, 2040)
ens_cols   <- c("2C" = "#0072B2", "1p5C" = "#009E73", "Ref" = "#E69F00", "PF" = "#CC79A7")
ind_names  <- c(di = "Distributional impact (%)", ep = "Energy poverty rate", tp = "Transport poverty rate")
dir.create(path_fig, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. MEDUSA results in long format: run_id | region | year | group | indicator | value
# ==============================================================================
read_platform <- function(ind, ens) {
  f <- file.path(path_platform, sprintf("%s_%s.xlsx", ind, ens))
  if (!file.exists(f)) return(NULL)
  x <- suppressMessages(read_excel(f, sheet = "EUR", col_names = FALSE))
  yrs <- as.integer(unlist(x[1, -(1:4)])); v <- as.matrix(x[-1, -(1:4)])
  mode(v) <- "numeric"
  data.frame(ensemble = ens, run_id = as.character(as.integer(unlist(x[-1, 4]))), region = "EUR",
             group = "EU", indicator = ind,
             year = rep(yrs, each = nrow(v)), value = as.vector(v), stringsAsFactors = FALSE)
}
long_file <- file.path(path_proc, "medusa_long.rds")
results <- do.call(rbind, unlist(lapply(ensembles, function(e)
  lapply(c("di", "ep", "tp"), read_platform, ens = e)), recursive = FALSE))
saveRDS(results, long_file)
results$ensemble <- factor(results$ensemble, levels = ensembles)
results <- drop_flagged(results)          # flagged scenarios stay in the platform, not in the figures

# ==============================================================================
# Fig 1. Headline indicators, 2020-2040: rows = indicator, columns = ensemble
#   Thin grey lines: individual scenarios. Band: 10th-90th percentile. Line: median.
#   Dotted vertical line: 2030 (main year). Dashed horizontal line: 2020 level (base HBS).
#   EP and TP are shown in % of households.
# ==============================================================================
yr_lim   <- c(2020, 2040)
ens_lab  <- c("2C" = "2\u00b0C", "1p5C" = "1.5\u00b0C", "Ref" = "Reference", "PF" = "PF")
ind_lab  <- c(di = "Distributional impact\n(% of expenditure)",
              ep = "Energy poverty\n(% of households)",
              tp = "Transport poverty\n(% of households)")
res1 <- results |>
  filter(year >= yr_lim[1], year <= yr_lim[2]) |>
  mutate(value     = ifelse(indicator == "di", value, 100 * value),
         indicator = factor(indicator, names(ind_lab), ind_lab),
         ensemble  = droplevels(ensemble))
# y-axis zoom: a few extreme runs (e.g. one outlier trajectory) would stretch the axis of a whole row. With zoom_y = TRUE the thin
# lines are clipped at the edge of the plotted range (quantiles zoom_q of each indicator, plus zoom_pad of the range on each side).
# Median and 10-90 band are NEVER clipped, and the caption says how many runs are clipped. Set zoom_y <- FALSE for the full range.
zoom_y <- TRUE; zoom_q <- c(.005, .995); zoom_pad <- .15
clip_by <- function(d, col) d |> group_by(indicator) |>
  mutate(.q1 = quantile(.data[[col]], zoom_q[1]), .q2 = quantile(.data[[col]], zoom_q[2]),
         .lo = .q1 - zoom_pad * (.q2 - .q1), .hi = .q2 + zoom_pad * (.q2 - .q1),
         clipped = .data[[col]] < .lo | .data[[col]] > .hi,
         plot_val = if (zoom_y) pmin(pmax(.data[[col]], .lo), .hi) else .data[[col]]) |>
  ungroup() |> select(-.q1, -.q2, -.lo, -.hi)
clip_note <- function(d) {
  n <- d |> group_by(ensemble) |> summarise(n = n_distinct(run_id[clipped]), .groups = "drop") |> filter(n > 0)
  if (!zoom_y || !nrow(n)) "" else paste0("\nAxis zoomed: lines beyond the plotted range are clipped (runs: ",
                                          paste0(ens_lab[as.character(n$ensemble)], " ", n$n, collapse = ", "), ").")
}
res1 <- clip_by(res1, "value")
fan <- res1 |>
  group_by(ensemble, indicator, year) |>
  summarise(p10 = quantile(value, .10), med = median(value), p90 = quantile(value, .90), .groups = "drop")
base1 <- fan |> filter(year == yr_lim[1]) |> group_by(indicator) |> summarise(base = mean(med), .groups = "drop")
n_run <- res1 |> distinct(ensemble, run_id) |> count(ensemble)
n_txt <- paste0(ens_lab[as.character(n_run$ensemble)], " ", n_run$n, collapse = ", ")
tags  <- expand.grid(indicator = levels(fan$indicator), ensemble = levels(fan$ensemble), stringsAsFactors = FALSE)
tags$indicator <- factor(tags$indicator, levels(fan$indicator)); tags$ensemble <- factor(tags$ensemble, levels(fan$ensemble))
tags <- tags[order(tags$indicator, tags$ensemble), ]; tags$tag <- paste0("(", letters[seq_len(nrow(tags))], ")")

# Table for the paper: median and 10-90 range per ensemble, indicator and year
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)
write.csv(fan, file.path(path_tab, "02_headline_summary.csv"), row.names = FALSE)

p1 <- ggplot() +
  geom_line(data = res1, aes(year, plot_val, group = run_id), colour = "grey40", linewidth = .1, alpha = .12) +
  geom_ribbon(data = fan, aes(year, ymin = p10, ymax = p90, fill = ensemble), alpha = .35) +
  geom_line(data = fan, aes(year, med, colour = ensemble), linewidth = .95) +
  geom_hline(data = base1, aes(yintercept = base), linetype = "dashed", colour = "grey25", linewidth = .3) +
  geom_vline(xintercept = 2030, linetype = "dotted", colour = "grey25", linewidth = .35) +
  geom_text(data = tags, aes(x = -Inf, y = Inf, label = tag), hjust = -.25, vjust = 1.5, size = 3.1, fontface = "bold") +
  facet_grid(indicator ~ ensemble, scales = "free_y", switch = "y",
             labeller = labeller(ensemble = ens_lab)) +
  scale_x_continuous(breaks = c(2020, 2030, 2040), minor_breaks = seq(2020, 2040, 5), limits = yr_lim, expand = expansion(mult = c(.03, .05))) +
  scale_colour_manual(values = ens_cols, guide = "none") +
  scale_fill_manual(values = ens_cols, guide = "none") +
  labs(x = NULL, y = NULL,
       caption = paste0("Thin lines: individual scenarios; line: median; band: 10th-90th percentile.\n",
                        "Dashed: 2020 level (HBS 2015 base); dotted: 2030, the main analysis year.\n",
                        "Scenarios per ensemble (flagged runs excluded): ", n_txt, ". Poverty thresholds fixed at the 2015 baseline.", clip_note(res1))) +
  theme_paper(base_size = 10) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain"),
        strip.text.x = element_text(face = "bold", size = 11),
        panel.spacing = unit(.9, "lines"), panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "grey30", size = 8, lineheight = 1.1),
        plot.caption.position = "plot")
save_fig(p1, "02_headline_over_time", width = 7.5, height = 6.2)

# ==============================================================================
# 2. Price changes: EU price index of key commodity groups, per scenario and year
#    (shock = 1 + sum w (p - 1), relative to 2020 = 1). Cached: reads all EPPA files once.
# ==============================================================================
hbs <- readRDS(file.path(path_proc, "hbs_2015_clean.rds"))
hbs_codes <- grep("^CP", names(hbs), value = TRUE)
groups <- list(
  "Electricity"                     = "^CP0451",
  "Gas"                             = "^CP0452",
  "Liquid/solid fuels"              = "^CP045[34]",
  "Transport fuels"                 = "^CP0722",
  "Food"                            = "^CP01",
  "Household energy (all CP045)"    = "^CP045",
  "All shocked codes (simple mean)" = "^CP")
cw <- tapply(hbs$weight * hbs$CP00, hbs$country, sum); cw <- cw / sum(cw)   # country weights (expenditure)

# Index of a group = simple mean of the shocks of its COICOP codes, then country-weighted EU mean.
# (No budget shares: the figure shows what EPPA prices do, not a welfare weight; DI is the welfare measure.)
price_index <- function(ens, yr) {
  sh <- suppressMessages(medusa::build_coicop_shocks(medusa::get_eppa_prices(file.path(path_eppa, ens), yr),
          medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)
  M  <- as.matrix(sh[, -1]); rownames(M) <- sh$coicop
  cn <- sub("_.*", "", colnames(M)); sc <- sub(".*_", "", colnames(M))
  if (!all(unique(cn) %in% names(cw)))
    message("price_index: country codes in the shocks not found in the HBS: ",
            paste(setdiff(unique(cn), names(cw)), collapse = ", "))
  do.call(rbind, lapply(names(groups), function(g) {
    k <- grepl(groups[[g]], rownames(M)); if (!any(k)) return(NULL)
    d <- data.frame(cn, sc, idx = colMeans(M[k, , drop = FALSE]))
    d$w <- cw[d$cn]; d <- d[!is.na(d$w), ]
    a <- tapply(d$idx * d$w, d$sc, sum) / tapply(d$w, d$sc, sum)
    data.frame(ensemble = ens, year = yr, run_id = names(a), group = g, index = as.numeric(a))
  }))
}
pi_file <- file.path(path_proc, "price_index_long.rds")
price_long <- if (file.exists(pi_file)) readRDS(pi_file) else NULL
todo <- setdiff(ensembles, unique(as.character(price_long$ensemble)))     # cache is per ensemble
if (length(todo)) {
  new <- do.call(rbind, lapply(todo, function(e) do.call(rbind, lapply(yrs_prices, function(y) price_index(e, y)))))
  price_long <- rbind(price_long, new)
  saveRDS(price_long, pi_file)
}
price_long <- price_long[price_long$ensemble %in% ensembles, ]
base <- unique(price_long[, c("ensemble", "run_id", "group")]); base$year <- 2020; base$index <- 1
price_long <- rbind(price_long, base)
price_long$ensemble <- factor(price_long$ensemble, levels = ensembles)
price_long$group <- factor(price_long$group, names(groups))
price_long <- drop_flagged(price_long)

for (e in ensembles) {
  d <- price_long |> filter(ensemble == e)
  s <- d |> group_by(group, year) |>
    summarise(min = min(index), p10 = quantile(index, .1), med = median(index),
              p90 = quantile(index, .9), max = max(index), .groups = "drop")
  p <- ggplot(s, aes(year)) +
    geom_line(data = d, aes(y = index, group = run_id), colour = "grey70", linewidth = .15, alpha = .3) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = ens_cols[[e]], alpha = .25) +
    geom_line(aes(y = med), colour = ens_cols[[e]], linewidth = .9) +
    geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
    facet_wrap(~ group, scales = "free_y") +
    labs(title = paste("Consumer price index of commodity groups,", e), x = NULL, y = "Index (2020 = 1)",
         caption = "Grey: scenarios. Line/band: median and 10-90th percentile. Simple mean of the COICOP codes in each group, EU country-weighted.") +
    theme_paper()
  save_fig(p, paste0("prices_", e), sub = "02_prices", width = 10, height = 6)
}

# ==============================================================================
# 3. DI by household group, every year in yrs_groups (decile, gender, zone, country; same groups as calc_di_eu,
#    households with unknown gender/zone are left out of the figures)
#    Needs the shocks of each year; cached per year and ensemble in data/processed (di_groups_<year>_<ens>.rds).
#    (Delete these caches after changing prices.)
# ==============================================================================
group_vars <- c(decile = "Expenditure decile", gender = "Gender", zone = "Urban / rural", country = "Country")
dg_one <- function(e, yr) {
  f_e <- file.path(path_proc, sprintf("di_groups_%d_%s.rds", yr, e))   # per-ensemble cache
  if (file.exists(f_e)) return(readRDS(f_e))
  sh <- suppressMessages(medusa::build_coicop_shocks(medusa::get_eppa_prices(file.path(path_eppa, e), yr),
          medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, EU_NO_AT, hbs_codes, verbose = FALSE)$shocks)
  ids <- unique(sub(".*_", "", setdiff(names(sh), "coicop")))
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  out <- do.call(rbind, lapply(seq_along(chunks), function(k) {
    message(e, " ", yr, ": chunk ", k, "/", length(chunks), "  ", format(Sys.time(), "%H:%M:%S"))
    s <- chunk_shocks_eu(sh, chunks[[k]])
    medusa::di_by_group_eu(hbs, s, names(group_vars))      # one shock computation for all 4 grouping variables
  }))
  out$ensemble <- e; out$year <- yr
  saveRDS(out, f_e)
  out
}
dg <- do.call(rbind, lapply(yrs_groups, function(yr) {
  f_y <- file.path(path_proc, sprintf("di_groups_%d.rds", yr))
  if (file.exists(f_y)) { x <- readRDS(f_y); if (!"year" %in% names(x)) x$year <- yr; return(x) }
  x <- do.call(rbind, lapply(ensembles, dg_one, yr = yr)); saveRDS(x, f_y); x
}))

dg <- drop_flagged(dg, id_col = "scenario")
dg <- dg |> filter(group != "NA")
qsum <- function(df, v) df |> group_by(ensemble, year, variable, group) |>
  summarise(p10 = quantile(.data[[v]], .1), p25 = quantile(.data[[v]], .25), med = median(.data[[v]]),
            p75 = quantile(.data[[v]], .75), p90 = quantile(.data[[v]], .9), .groups = "drop")

# Three figures per ensemble and year (decile / gender and zone / country), saved in `sub`.
group_figs <- function(summ, e, yr, sub, ylab) {
  s <- summ |> filter(ensemble == e, year == yr); col <- ens_cols[[e]]
  ttl <- function(x) paste0(x, ", ", ens_lab[[e]], ", ", yr)
  dec <- s |> filter(variable == "decile") |> mutate(group = as.numeric(group))
  p_dec <- ggplot(dec, aes(group)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = col, alpha = .2) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = col, alpha = .35) +
    geom_line(aes(y = med), colour = col, linewidth = .9) + geom_point(aes(y = med), colour = col) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = ttl("By expenditure decile"), x = "Decile (1 = poorest)", y = ylab) + theme_paper()
  oth <- s |> filter(variable %in% c("gender", "zone")) |>
    mutate(variable = factor(variable, names(group_vars), group_vars))
  p_oth <- ggplot(oth, aes(group, med)) +
    geom_linerange(aes(ymin = p10, ymax = p90), colour = col, linewidth = 1.4, alpha = .35) +
    geom_linerange(aes(ymin = p25, ymax = p75), colour = col, linewidth = 2.4, alpha = .6) +
    geom_point(colour = col, size = 2.4) + facet_wrap(~ variable, scales = "free_x") +
    labs(title = ttl("By gender and urban/rural"), x = NULL, y = ylab,
         caption = "Households with unknown gender are excluded.") + theme_paper()
  cty <- s |> filter(variable == "country") |> mutate(group = reorder(group, med))
  p_cty <- ggplot(cty, aes(med, group)) +
    geom_linerange(aes(xmin = p10, xmax = p90), colour = col, alpha = .4, linewidth = 1.2) +
    geom_point(colour = col) +
    labs(title = ttl("By country"), x = ylab, y = NULL) + theme_paper()
  save_fig(p_dec, paste0("di_decile_", e, "_", yr), sub = sub, width = 6, height = 4)
  save_fig(p_oth, paste0("di_gender_zone_", e, "_", yr), sub = sub, width = 7, height = 4)
  save_fig(p_cty, paste0("di_country_", e, "_", yr), sub = sub, width = 6, height = 6)
}

summ <- qsum(dg, "di")
write.csv(summ, file.path(path_tab, "02_di_groups_summary.csv"), row.names = FALSE)
for (yr in yrs_groups) for (e in ensembles) group_figs(summ, e, yr, "02_di_groups", "DI (% of expenditure)")

# ==============================================================================
# 4. Diagnostics (sel_year): outcome vs overall price index; flags
# ==============================================================================
xmap <- c(di = "All shocked codes (simple mean)", ep = "Household energy (all CP045)", tp = "Transport fuels")
d4 <- do.call(rbind, lapply(names(xmap), function(ind) {
  r <- results |> filter(year == sel_year, indicator == ind) |> select(ensemble, run_id, value)
  p <- price_long |> filter(year == sel_year, group == xmap[[ind]]) |> select(ensemble, run_id, price_index = index)
  inner_join(r, p, by = c("ensemble", "run_id")) |> mutate(panel = paste0(ind_names[[ind]], " vs ", xmap[[ind]]))
}))
p4 <- ggplot(d4, aes(price_index, value, colour = ensemble)) +
  geom_point(alpha = .35, size = .8) + facet_wrap(~ panel, scales = "free") +
  scale_colour_manual(values = ens_cols) +
  labs(x = paste("EPPA price index,", sel_year, "(2020 = 1)"), y = NULL, colour = NULL,
       caption = "Higher prices should mean lower DI (more negative) and higher poverty rates.") + theme_paper()
save_fig(p4, paste0("02_diagnostic_price_vs_outcome_", sel_year), width = 12, height = 4)

flags <- results |> filter(indicator == "di") |> group_by(ensemble, year) |>
  summarise(n_positive = sum(value > 0), n_abs_gt5 = sum(abs(value) > 5), .groups = "drop")
print(flags, n = Inf)

# ---- Platform comparison placeholder ---------------------------------------------
# platform <- read.csv(file.path(root, "data", "external", "platform_results.csv"))

message("02_figures_results.R finished: figures in ", path_fig)
