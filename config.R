# config.R
# Paths and global settings. Edit here only; every script starts with
#   source(here::here("config.R"))
# Paths are relative to the project root (found through the .Rproj / .git file).

root <- here::here()

# ---- Data ---------------------------------------------------------------------
path_hbs     <- file.path(root, "data", "hbs_data")
path_eppa    <- file.path(root, "data", "eppa_outputs")
path_design  <- file.path(root, "data", "design")
path_bridge  <- file.path(root, "data", "external")
path_proc    <- file.path(root, "data", "processed")

# ---- Outputs ------------------------------------------------------------------
path_fig     <- file.path(root, "outputs", "figures")
path_tab     <- file.path(root, "outputs", "tables")

# ---- Settings -----------------------------------------------------------------
hbs_year     <- 2020
price_year   <- 2030
