# Scenarios flagged by screen_scenarios.R (implausible EPPA prices). They stay in the platform
# files; scripts 02 and 03 drop them from the analysis and figures.
#   drop_flagged(df)          df has columns `ensemble` and `run_id` (or give id_col / ens_col)
#   config.R: drop_flagged_scenarios <- TRUE   (switch to FALSE to keep everything)
load_flagged <- function(path = file.path(path_proc, "flagged_scenarios.rds")) {
  if (file.exists(path)) readRDS(path) else data.frame(ensemble = character(), run_id = character())
}
drop_flagged <- function(df, ens_col = "ensemble", id_col = "run_id", flagged = load_flagged()) {
  if (exists("drop_flagged_scenarios") && !isTRUE(drop_flagged_scenarios)) return(df)
  key <- paste(as.character(df[[ens_col]]), as.character(df[[id_col]]))
  out <- df[!key %in% paste(flagged$ensemble, flagged$run_id), , drop = FALSE]
  message("drop_flagged: removed ", nrow(df) - nrow(out), " rows (", nrow(flagged), " flagged scenarios)")
  out
}
