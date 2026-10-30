# ==============================================================================
# R/diag/screen_outcomes.R
# Outcome-based screening: flag runs whose DI jumps and comes back (a one-step spike), which the price rules can miss.
#
# Rule: for each interior year t (2025, 2030, 2035) compare DI_t with the mean of its neighbours (t-5, t+5):
#           spike = | DI_t - (DI_{t-5} + DI_{t+5}) / 2 |   (percentage points)
#       a run is flagged if spike > spike_pp in any interior year. A steady trend gives spike ~ 0, so steep ensembles (1.5C) are not penalised;
#       a one-year excursion (e.g. -64% in 2035 between -4% and -6%) gives a large value.
#       With the current results 8 pp flags 2C 14 and 257 and 1p5C 110; the next largest values are 7.4 (2C 337) and 7.3 (2C 388, 2030, natural kink).
#       The 2C median peaks in 2030, so a threshold below ~7 would start flagging ordinary runs: look at the table before lowering it.
#
# Reads : outputs/platform/di_<ens>.xlsx (sheet EUR; row 1 = years, column 4 = run id)   -> run AFTER 01_run_medusa.R
# Writes: data/processed/flagged_outcomes.rds   (ensemble, run_id), read by load_flagged() with the price and GDP flags
#         outputs/tables/diag_flagged_outcomes.csv   (ensemble, run_id, year, spike_pp, di_prev, di, di_next), and the top spikes per ensemble
#         outputs/figures/0_outliers/outliers_di_spikes.png
# Run from the project root. Re-run 02 / 03 afterwards (they drop the flagged runs).
# ==============================================================================
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(tidyr); library(ggplot2) })

# ---- settings ----------------------------------------------------------------------------
spike_max <- 8                           # percentage points; see the header before changing it
yrs_chk  <- c(2020, 2025, 2030, 2035, 2040)

read_di <- function(e) {
  x <- suppressMessages(read_excel(file.path(path_platform, paste0("di_", e, ".xlsx")), sheet = "EUR", col_names = FALSE))
  y <- suppressWarnings(as.integer(unlist(x[1, -(1:4)])))
  m <- as.matrix(x[-1, -(1:4)]); mode(m) <- "numeric"; colnames(m) <- y
  m <- m[, as.character(yrs_chk), drop = FALSE]
  data.frame(ensemble = e, run_id = as.character(as.integer(unlist(x[-1, 4]))), m, check.names = FALSE)
}
dw <- bind_rows(lapply(ensembles, read_di))   # wide: one row per run, one column per year

sp <- bind_rows(lapply(c(2025, 2030, 2035), function(t) {
  a <- as.character(t - 5); b <- as.character(t + 5)
  tibble(ensemble = dw$ensemble, run_id = dw$run_id, year = t, di_prev = dw[[a]], di = dw[[as.character(t)]], di_next = dw[[b]]) |>
    mutate(spike_pp = abs(di - (di_prev + di_next) / 2))
}))

flags <- sp |> filter(spike_pp > spike_max) |> arrange(desc(spike_pp))
cat("\nLargest spikes per ensemble (pp):\n")
print(sp |> group_by(ensemble) |> slice_max(spike_pp, n = 5) |> ungroup() |> mutate(across(where(is.numeric) & !matches("year"), ~ round(.x, 1))), n = Inf)
cat("\nFlagged (spike >", spike_max, "pp):\n"); print(flags |> mutate(across(where(is.numeric) & !matches("year"), ~ round(.x, 1))), n = Inf)

dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)
write.csv(flags, file.path(path_tab, "diag_flagged_outcomes.csv"), row.names = FALSE)
saveRDS(distinct(flags, ensemble, run_id) |> as.data.frame(), file.path(path_proc, "flagged_outcomes.rds"))   # read by load_flagged()

# ---- figure: DI trajectories, flagged runs in red --------------------------------------------
fl <- paste(flags$ensemble, flags$run_id)
long <- dw |> pivot_longer(-c(ensemble, run_id), names_to = "year", values_to = "di") |>
  mutate(year = as.integer(year), ensemble = factor(ensemble, ensembles), flagged = paste(ensemble, run_id) %in% fl)
p <- ggplot(long, aes(year, di, group = run_id)) +
  geom_line(data = filter(long, !flagged), colour = "grey70", linewidth = .2, alpha = .6) +
  geom_line(data = filter(long, flagged), colour = "#B2182B", linewidth = .6) +
  facet_wrap(~ensemble, nrow = 1) + scale_x_continuous(breaks = yrs_chk) +
  labs(x = NULL, y = "Distributional impact (%)", title = "Screening: one-step spikes in DI",
       subtitle = sprintf("Red: runs with |DI(t) - mean of DI(t-5), DI(t+5)| > %g pp (%d runs). Grey: all others.", spike_max, nrow(distinct(flags, ensemble, run_id)))) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
fig_dir <- file.path(path_fig, "0_outliers"); dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(fig_dir, "outliers_di_spikes.png"), p, width = 12, height = 3.8, dpi = 200, bg = "white")
message("screen_outcomes.R: ", nrow(distinct(flags, ensemble, run_id)), " runs flagged. Figure: ", fig_dir)
