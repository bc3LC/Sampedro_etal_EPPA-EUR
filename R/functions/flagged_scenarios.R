# Scenarios flagged as implausible. They stay in the platform files; scripts 02 and 03 (and the diag scripts) drop them from the
# analysis and figures. Three sources, merged here:
#   data/processed/flagged_scenarios.rds   written by R/diag/screen_scenarios.R (price rules)
#   data/processed/flagged_gdp.rds         written by R/diag/gdp_compare.R (GDP collapse of a run, GLB or EUR)
#   data/processed/flagged_outcomes.rds    written by R/diag/screen_outcomes.R (one-step spike in the DI of a run)
#   drop_flagged(df)          df has columns `ensemble` and `run_id` (or give id_col / ens_col)
#   config.R: drop_flagged_scenarios <- TRUE   (switch to FALSE to keep everything)
load_flagged <- function(path = file.path(path_proc, "flagged_scenarios.rds"), path_gdp = file.path(path_proc, "flagged_gdp.rds"),
                         path_out = file.path(path_proc, "flagged_outcomes.rds")) {
  rd <- function(p) if (file.exists(p)) { x <- readRDS(p); data.frame(ensemble = as.character(x$ensemble), run_id = as.character(x$run_id)) } else
    data.frame(ensemble = character(), run_id = character())
  unique(rbind(rd(path), rd(path_gdp), rd(path_out)))
}
drop_flagged <- function(df, ens_col = "ensemble", id_col = "run_id", flagged = load_flagged()) {
  if (exists("drop_flagged_scenarios") && !isTRUE(drop_flagged_scenarios)) return(df)
  key <- paste(as.character(df[[ens_col]]), as.character(df[[id_col]]))
  out <- df[!key %in% paste(flagged$ensemble, flagged$run_id), , drop = FALSE]
  message("drop_flagged: removed ", nrow(df) - nrow(out), " rows (", nrow(flagged), " flagged scenarios)")
  out
}
