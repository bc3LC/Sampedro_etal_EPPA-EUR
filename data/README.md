# data/

Inputs and intermediate data. Most of this folder is **not tracked by Git** (see `.gitignore`). Tracked: `design/`, the final bridge tables in `external/` and this README.

| Folder | Contents | Tracked |
|---|---|---|
| `hbs_data/` | EU Household Budget Survey: raw workbooks `2015/` (`{CC}_MFR_hh.xlsx`, `_hm.xlsx`) and `2020/` (`HBS_HH_{CC}.xlsx`, `HBS_HM_{CC}.xlsx`); `inputs/{CC}/hbs_{year}_{CC}.RData` processed per country | No |
| `eppa_outputs/` | EPPA price indices, one subfolder per ensemble: `2C/`, `1p5C/`, `PF/`, `Ref/`. One workbook per sector: `9_price_<sector>_price_index_<ensemble>.xlsx`. Sheet `pol_EUR` (policy ensembles) or `ref_EUR` (Ref); rows = runs 1-400, columns = years 2020-2100. `example/` holds the platform template `di_2C.xlsx` | No |
| `design/` | `Inputs_Master.xlsx`: sheet `samples`, one row per run (1-400), one column per EPPA uncertain input (elasticities, fossil resources, technology costs, AEEI, GDP and population ratios); the rows below the runs are percentiles and are ignored | Yes |
| `external/` | Final bridge tables: `map_eppa_cpa.csv`, `map_eppa_sectornames.csv`, `cpa_coicop_eurBridge.csv`. Work-in-progress files live in `WiP/` | Yes (final tables) |
| `processed/` | Regenerable results: cleaned HBS (`hbs_2015_clean.rds`, ...), `chunks/` (resumable MEDUSA chunks), `medusa_<ens>.rds` (per ensemble), `medusa_long.rds` (all ensembles, long), `price_index_long.rds`, `di_groups_<year>[_<ens>].rds`, `screening_table.rds`, `flagged_scenarios.rds` | No |
| `publication_database/` | SQL dump of the JP-Data-Viz publication database (about 380 MB; keep outside Git). It contains model **outputs** only (33 variables, 38 regions, scenarios 2C and Ref); it does not contain the EPPA inputs | No |

## Conventions

- `run_id` links the inputs (`Inputs_Master.xlsx`, run number), the EPPA price files (scenario id, column 4 of each sheet) and the MEDUSA results. Scripts convert it to character on both sides before joining. Draw *k* is the same uncertainty sample in every ensemble.
- Processed results are in long format: `run_id`, `region`, `year`, `group`, `indicator`, `value` (plus `ensemble`).
- Flagged scenarios: `flagged_scenarios.rds` with columns `ensemble`, `run_id`. They stay in the platform files and are dropped by scripts 02 and 03 when `drop_flagged_scenarios <- TRUE` in `config.R`.
- Never edit raw data by hand. All transformations go through scripts in `R/`.

## Where to get the raw data

- HBS: *(add source, version and access date)*
- EPPA7 outputs: *(add model version, scenario names, extraction date; Ref ensemble re-extracted in October 2026)*
- COICOP/CPA mappings: *(add source files)*
