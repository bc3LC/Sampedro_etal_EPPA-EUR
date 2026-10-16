# Robust test: hide CP0723, CP0731/32/34/35/36 in FR / IT / BE, re-clean, compare with truth.
# Needs in memory: raw (hbs_2015_raw.rds), part (partition), shocks for one scenario (sh100 or sh),
# and indicators_eu() sourced; MEDUSA loaded with devtools::load_all().
shk <- if (exists("sh100")) sh100 else sh
hide <- c("CP0723","CP0731","CP0732","CP0734","CP0735","CP0736")

run <- function(df, ...) {
  cl <- prep_hbs_eu(df, targets = part, verbose = FALSE, ...)
  list(n_pt = sum(rowSums(cl[, intersect(c("CP0731","CP0732","CP0733","CP0734","CP0735","CP0736"), names(cl))],
                          na.rm = TRUE) > 0 & cl$weight > 0),
       cl = cl)
}
met <- function(cl, tp_index) tryCatch({
  r <- indicators_eu(cl, shk, ep_index = "LIHC", tp_index = tp_index, tp_exclude = NULL)
  c(di = r$scen$di[1], tp = r$scen$tp[1], tp_base = unname(r$base["tp"]))
}, error = function(e) c(di = NA, tp = NA, tp_base = NA))

res <- list()
for (cty in c("FR","IT","BE")) {
  d <- raw[raw$country == cty, ]
  h <- d; h[, intersect(hide, names(h))] <- 0
  v <- list(
    truth        = run(d, method = "pooled", struct = FALSE),
    old_pooled   = run(h, method = "pooled", struct = FALSE),
    new_pooled   = run(h, method = "pooled", struct = TRUE),
    new_hotdeck  = run(h, method = "hotdeck", struct = TRUE))
  for (nm in names(v)) for (ti in c("VTU","LIHC")) {
    m <- met(v[[nm]]$cl, ti)
    res[[length(res) + 1]] <- data.frame(country = cty, version = nm, tp_index = ti,
                                         hh_with_public_transport = v[[nm]]$n_pt, di = m["di"], tp = m["tp"], tp_base = m["tp_base"])
  }
}
res <- do.call(rbind, res); rownames(res) <- NULL
print(res, digits = 4)
# error vs truth
tr <- res[res$version == "truth", c("country","tp_index","tp","di")]
names(tr)[3:4] <- c("tp_true","di_true")
cmp <- merge(res[res$version != "truth", ], tr)
cmp$tp_err <- cmp$tp - cmp$tp_true; cmp$di_err <- cmp$di - cmp$di_true
cat("\nErrors vs truth (NA = that version failed):\n")
print(cmp[order(cmp$country, cmp$tp_index, cmp$version), c("country","tp_index","version","tp_err","di_err")], digits = 3)
