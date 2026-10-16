# config.R
# Paths and global settings. Edit here only; every script starts with
#   source(here::here("config.R"))
# Paths are relative to the project root (found through the .Rproj / .git file).

root <- here::here()

# ---- MEDUSA (branch dev-js-eppa, loaded with devtools::load_all) ---------------
path_medusa <- "C:/Users/Jon Sampedro/Desktop/medusa"

# ---- Data ---------------------------------------------------------------------
path_hbs     <- file.path(root, "data", "hbs_data")
path_eppa    <- file.path(root, "data", "eppa_outputs")
path_design  <- file.path(root, "data", "design")
path_bridge  <- file.path(root, "data", "external")
path_proc    <- file.path(root, "data", "processed")
path_inputs  <- file.path(path_design, "Inputs_Master.xlsx")   # EPPA uncertain parameters per run (sheet "samples")

# ---- Outputs ------------------------------------------------------------------
path_fig      <- file.path(root, "outputs", "figures")
path_tab      <- file.path(root, "outputs", "tables")
path_platform <- file.path(root, "outputs", "platform")      # di_<ens>.xlsx, ep_<ens>.xlsx, tp_<ens>.xlsx
path_chunks   <- file.path(path_proc, "chunks")              # saved chunks (a rerun resumes from here)

# ---- Settings -----------------------------------------------------------------
hbs_year     <- 2015                                  # base HBS (Austria has no 2015 data)
ensembles    <- c("2C", "1p5C", "Ref", "PF")          # folders of data/eppa_outputs
years_calc   <- c(2025, 2030, 2035, 2040)             # computed; 2020 = base, 2045-2100 copy 2040
chunk_size   <- 50                                    # scenarios per chunk
drop_flagged_scenarios <- FALSE

# Countries (EU without Austria)
EU_NO_AT <- c("Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
              "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# Headline indicators (EU value). Poverty thresholds are fixed at the base HBS in MEDUSA.
ep_index   <- "LIHC"
tp_index   <- "VTU"       # or "LIHC"
tp_exclude <- NULL        # countries left out of the EU TP value (NULL = none; "DE" only if the test says so)

# Platform tables: template (layout) and the three label columns of each indicator
platform_template <- file.path(path_eppa, "example", "di_2C.xlsx")
platform_labels <- list(
  di = c("distributional impacts", "di", "di"),
  ep = c("energy poverty", "ep", "ep"),
  tp = c("transport poverty", "tp", "tp")
)
