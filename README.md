# Sampedro et al. - EPPA-EUR

Distributional household impacts of EU decarbonisation pathways under uncertainty.

> Working title: *Towards Equitable Climate Policy: Identifying the Drivers of Potential Welfare Impacts and Energy Poverty in the European Union* (provisional)

## Research question

Which uncertain drivers (technology, resource and policy assumptions) most shape the potential welfare impacts and energy-poverty risks of EU decarbonisation, so that climate policy can be designed more equitably?

The study identifies drivers and potential impacts. It does not design compensation mechanisms.

## Pipeline

```
EPPA ensembles (400 runs each) -> sector price indices (EUR) -> MEDUSA + HBS 2015 -> DI, energy poverty, transport poverty -> figures + scenario discovery
```

1. **EPPA ensembles.** Four ensembles of 400 runs each: `2C`, `1p5C`, `PF` (policy) and `Ref` (reference). Draw *k* is the same uncertainty sample in every ensemble, so policy-minus-Ref differences are paired by `run_id`. The uncertain inputs of each run are in `data/design/Inputs_Master.xlsx`.
2. **Prices.** One workbook per sector and ensemble (13 sectors), region `EUR`, years 2020-2100. Crude oil is dropped before the shock (MEDUSA `get_eppa_prices(drop = "oil")`).
3. **MEDUSA** (R package, branch `dev-js-eppa`, not merged; loaded with `devtools::load_all(path_medusa)`). Sector price indices become COICOP price shocks through the EPPA -> CPA -> COICOP bridge (`data/external/`) and are applied to the EU Household Budget Survey.
4. **Indicators** (EU level): distributional impact (DI), energy poverty (EP) and transport poverty (TP).
5. **Scenario discovery**: random forest, CART, permutation importance and time-series clustering, following the JP-Data-Viz app so that results are comparable.

### Method choices

- HBS 2015 base. Austria is excluded (no 2015 data); Romania is excluded from EP and TP.
- DI = change in welfare as % of baseline expenditure (negative = loss). The shock is linear in the sector price changes; static budget (no quantity response), full pass-through, district heat not shocked. EU value = household-weighted mean.
- EP = LIHC, TP = VTU (`config.R`). Poverty thresholds are fixed at the base HBS, so rates change only because costs change.
- Years computed: 2025, 2030, 2035, 2040. 2020 is the base (from the HBS); 2045-2100 copy 2040 (needed only for the platform tables).
- Main analysis year: 2030. Time series in the figures: 2020-2040.
- Scenarios flagged as implausible (see `R/diag/screen_scenarios.R`) stay in the platform files; scripts 02 and 03 drop them when `drop_flagged_scenarios <- TRUE`.

## Repository structure

```
Sampedro_etal_EPPA-EUR/
├── README.md
├── .gitignore
├── Sampedro_etal_EPPA-EUR.Rproj
├── config.R                       # paths and global settings (edit here only)
├── R/
│   ├── 01_run_medusa.R            # HBS 2015 + EPPA prices -> MEDUSA -> data/processed + outputs/platform
│   ├── 02_figures_results.R       # results and diagnostics figures (headline, prices, DI by group)
│   ├── 03_scenario_discovery.R    # blocks A-D: input-output, heatmaps, output-output, clustering
│   ├── diag/                      # diagnostics, run on demand (see below)
│   ├── functions/                 # helpers sourced by the scripts (see below)
│   └── tests/                     # checks of the MEDUSA changes (HBS allocation, shocks, dose-response)
├── data/                          # see data/README.md
├── outputs/                       # see outputs/README.md
├── manuscript/                    # outline, notes, bibliography, drafts
├── docs/                          # notes and decisions
└── Notas cambios MEDUSA.txt       # notes on the changes made in the MEDUSA branch
```

### `R/diag/`

| Script | Purpose |
|---|---|
| `di_decomposition.R` | Exact decomposition of DI by commodity group, EPPA sector and services by COICOP division; sensitivity variants (gas/solid fixed, energy only, non-energy only, numeraire-neutral) |
| `screen_scenarios.R` | Screens the EPPA price trajectories and writes the screening table |
| `diag_outliers.R` | Looks at flagged / outlier scenarios |
| `eppa_price_compare.R` | EPPA sector price index per ensemble (median and 10-90 band), for comparing ensembles |

### `R/functions/`

| File | Purpose |
|---|---|
| `run_ensemble.R` | Runs one ensemble through MEDUSA in resumable chunks; builds and writes the platform tables |
| `prep_hbs.R` | HBS cleaning helpers |
| `flagged_scenarios.R` | `load_flagged()` / `drop_flagged()` (honours `drop_flagged_scenarios`) |
| `plot_helpers.R` | Plot theme, palettes and `save_fig()` (PNG, optional subfolder) |
| `scenario_discovery.R` | Helpers of script 03 (design file reader, random forest, CART, clustering) |
| `sd_helpers.R` | Earlier scenario-discovery helpers, superseded by `scenario_discovery.R`; not used by script 03 |

## How to run

1. Open `Sampedro_etal_EPPA-EUR.Rproj` in RStudio.
2. Check paths and settings in `config.R` (`path_medusa` must point to the MEDUSA branch `dev-js-eppa`).
3. Place the raw inputs as described in `data/README.md`.
4. Run in order:
   - `R/01_run_medusa.R` (long: about 8 chunks per ensemble and year; resumable from `data/processed/chunks/`)
   - `R/02_figures_results.R`
   - `R/03_scenario_discovery.R` (blocks A-D can be switched on and off in its section 0)
5. If you change the ensembles or the flagged list, delete the caches in `data/processed/` first: `medusa_long.rds`, `price_index_long.rds`, `di_groups_*.rds` (and the chunks of the ensemble you rerun).

## Requirements

- R >= 4.2; MEDUSA (branch `dev-js-eppa`).
- Packages: `here`, `devtools`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `readxl`, `openxlsx`, `ranger`, `rpart`, `partykit`, `GGally`, `patchwork`.
- (Optional) JP-Data-Viz app for interactive exploration of the ensembles.

## Status and open items

- [x] Pipeline 01 -> 02 -> 03 for the four ensembles
- [ ] Final EPPA ensembles (the EPPA team may supply edited or additional ensembles)
- [ ] Flagged scenarios (truncated or blown-up price trajectories): final list agreed with the EPPA team
- [ ] Treatment of coal / gas price shocks (sensitivities); crude oil already dropped
- [ ] Final year for the time series (2040) and the main analysis year (2030)
- [ ] Run script 03 on the final ensembles
- [ ] Upload to the platform (long format)
- [ ] EU country-level extension (deferred; current scope is EU level)

## Contact

Jon Sampedro, BC3 Research
