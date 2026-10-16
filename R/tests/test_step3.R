# ---- Step 2/3 test script (run in your session, MEDUSA loaded from dev-js-eppa with final4 files) ----
# 0. shocks (already built): out <- build_coicop_shocks(prices, cpa_coicop_eurBridge, map_eppa_cpa, EU_NO_AT, hbs_codes)
targets <- out$partition

# 1. clean HBS 2015 (new prep_hbs_eu: targets, dead-children fix, CP0411/CP0421/CP073x index codes)
hbs_2015_clean <- prep_hbs_eu(hbs_2015, targets = targets)

# 2. negatives table BEFORE flooring (materiality, % of household budget)
cpn <- grep("^CP\\d+$", names(hbs_2015), value = TRUE)
neg <- do.call(rbind, lapply(setdiff(cpn, "CP00"), function(v) {
  x <- suppressWarnings(as.numeric(hbs_2015[[v]])); i <- which(x < 0)
  if (!length(i)) return(NULL)
  data.frame(code = v, country = hbs_2015$country[i], n = 1, pct_budget = 100 * x[i] / as.numeric(hbs_2015$CP00[i]))
}))
negtab <- aggregate(cbind(n, pct_budget) ~ code + country, neg, function(z) c(sum(z)))
print(head(negtab[order(-negtab$n), ], 25))
print(summary(neg$pct_budget))

# 3. post-clean coverage: shocked codes should add up to CP00 (gap ~ 0)
cov <- sapply(split(seq_len(nrow(hbs_2015_clean)), hbs_2015_clean$country), function(i)
  sum(rowSums(hbs_2015_clean[i, intersect(targets, names(hbs_2015_clean))] * hbs_2015_clean$weight[i])) /
  sum(hbs_2015_clean$CP00[i] * hbs_2015_clean$weight[i]))
print(round(cov, 3))

# 4. one scenario (_100): DI / EP / TP with baseline thresholds
sc <- "_100"   # adapt to your column suffix in out$shocks
# di <- calc_di_eu(...); ep <- calc_ep_eu(...); tp <- calc_tp_eu(...)  # same arguments as your HBS-2020 runs, with data = hbs_2015_clean, shocks = out$shocks, thresholds = "baseline"
