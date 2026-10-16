# Real-data test of the allocation methods on Germany (run in the repo project, MEDUSA dev-js-eppa loaded)
source(here::here("config.R")); devtools::load_all(path_medusa)
source(here::here("R/functions/run_ensemble.R"))

raw  <- readRDS(file.path(path_proc, "hbs_2015_raw.rds"))
cp   <- grep("^CP\\d+$", names(raw), value = TRUE)
part <- medusa::coicop_partition(medusa::cpa_coicop_eurBridge, medusa::map_eppa_cpa, cp)
num  <- function(x) suppressWarnings(as.numeric(as.character(x)))

# residual of a parent not explained by its children (NA children count as 0)
resid <- function(d, p) {
  k <- cp[nchar(cp) == nchar(p) + 1 & startsWith(cp, p)]
  num(d[[p]]) - rowSums(sapply(k, function(v) { x <- num(d[[v]]); x[is.na(x)] <- 0; x }))
}
de <- raw[raw$country == "DE", ]
ok <- function(p) (abs(resid(de, p)) <= 0.5) %in% TRUE
complete <- ok("CP07") & ok("CP072") & ok("CP073")        # German households that really report the CP07 detail
cat("DE households:", nrow(de), "| complete CP07 detail:", sum(complete), "\n")
de_c <- de[complete, ]

set.seed(1)
blank <- sample(nrow(de_c), nrow(de_c) %/% 2)
c07   <- grep("^CP07(1|2|3)", cp, value = TRUE)            # all children of CP07 at any depth
de_b  <- de_c; de_b[blank, c07] <- NA                      # half of them now report CP07 only

other <- raw[raw$country != "DE", ]
run <- function(d, method) { x <- prep_hbs_eu(rbind(other, d), targets = part, method = method, verbose = FALSE)
                             x[x$country == "DE", ] }
truth <- run(de_c, "hotdeck")                              # nothing to allocate: equals the complete data
cl <- list(pooled = run(de_b, "pooled"), hotdeck = run(de_b, "hotdeck"))

sh100 <- chunk_shocks_eu(sh, "100")                        # `sh`: shocks of an ensemble/year, as before
met <- function(x) { t <- medusa::calc_tp_eu(x, shocks = sh100); t[, c("scenario", "TP_index", "DE")] }
out <- met(truth); names(out)[3] <- "true"
for (m in names(cl)) out[[m]] <- met(cl[[m]])$DE
out$err_pooled  <- out$pooled  / out$true - 1
out$err_hotdeck <- out$hotdeck / out$true - 1
print(out, digits = 3)

# distributional impact (mean, %)
imp <- function(x) { r <- medusa::price_shock_eu(x, sh100, keep = character(0))
                     s <- sub("^CP00_", "", grep("^CP00_", names(r), value = TRUE))[1]
                     sum(r$weight * 100 * (r$CP00 - r[[paste0("CP00_", s)]]) / r$CP00) / sum(r$weight) }
cat("mean DI  true:", round(imp(truth), 3), " pooled:", round(imp(cl$pooled), 3), " hotdeck:", round(imp(cl$hotdeck), 3), "\n")
