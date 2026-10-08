# ==============================================================================
# 01_run_medusa.R
# EU Household Budget Survey + EPPA sectoral prices -> MEDUSA -> distributional impacts
#
# Reads : data/hbs_data/, data/eppa_outputs/
# Writes: data/processed/medusa_di_results.rds / .csv
# ==============================================================================

source(here::here("config.R"))

# Load MEDUSA (uncomment / edit as needed)
# library(medusa)
# source(file.path(root, "R", "functions", "medusa_functions.R"))

# Household budget survey
hbs <- hbs_eu(year = hbs_year, country = "all", path = path_hbs)

# EPPA price shocks
shocks <- get_prices_eppa(path_eppa, price_year)

# Distributional impacts
di_results <- calc_di_eu(data = hbs, shocks = shocks, by_country = FALSE)

# Save
dir.create(path_proc, showWarnings = FALSE, recursive = TRUE)
saveRDS(di_results, file.path(path_proc, "medusa_di_results.rds"))
write.csv(di_results, file.path(path_proc, "medusa_di_results.csv"), row.names = FALSE)
