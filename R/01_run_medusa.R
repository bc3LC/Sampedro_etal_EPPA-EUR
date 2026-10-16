# ==============================================================================
# 01_run_medusa.R
# HBS 2015 + EPPA sectoral prices -> MEDUSA -> EU-level DI, energy poverty and
# transport poverty per scenario, for every ensemble and year.
#
# Reads : data/hbs_data/, data/eppa_outputs/<ensemble>/
# Writes: data/processed/hbs_2015_clean.rds
#         data/processed/chunks/*.rds          (resumable)
#         data/processed/medusa_<ens>.rds      (long results + base rates)
#         outputs/platform/{di,ep,tp}_<ens>.xlsx
# ==============================================================================

source(here::here("config.R"))
devtools::load_all(path_medusa)
source(here::here("R/functions/run_ensemble.R"))

dir.create(path_proc, showWarnings = FALSE, recursive = TRUE)
dir.create(path_platform, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Cleaned HBS 2015 (built once) ------------------------------------------
f_hbs <- file.path(path_proc, "hbs_2015_clean.rds")
if (!file.exists(f_hbs)) {
  # clean = TRUE: weights, CP00 <= 0, parent -> child allocation at the shocked codes
  # (derived from the package bridge / map), negatives floored at 0
  hbs <- hbs_eu(year = hbs_year, country = "all", path = path_hbs, clean = TRUE)
  saveRDS(hbs, f_hbs)
} else {
  hbs <- readRDS(f_hbs)
}
hbs_codes <- grep("^CP\\d+$", names(hbs), value = TRUE)

# ---- 2. Ensembles x years ------------------------------------------------------
for (ens in ensembles) {
  message("==== Ensemble ", ens, " ====")
  run <- run_ensemble_eu(
    eppa_path  = file.path(path_eppa, ens),
    years      = years_calc,
    hbs        = hbs,
    countries  = EU_NO_AT,
    hbs_codes  = hbs_codes,
    ep_index   = ep_index,
    tp_index   = tp_index,
    tp_exclude = tp_exclude,
    chunk      = chunk_size,
    out_dir    = path_chunks,
    ensemble   = ens
  )
  saveRDS(run, file.path(path_proc, paste0("medusa_", ens, ".rds")))

  # ---- 3. Platform tables (scenario x year, 2020-2100) -------------------------
  tabs <- assemble_platform_eu(run)
  for (k in names(tabs)) {
    write_platform_xlsx(tabs[[k]],
                        file = file.path(path_platform, paste0(k, "_", ens, ".xlsx")),
                        template = platform_template,
                        labels = platform_labels[[k]])
  }
}
