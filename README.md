# Sampedro et al. - EPPA-EUR

Distributional an dpoverty impacts of EU decarbonisation pathways under uncertainty.

> Working title: *Towards Equitable Climate Policy: Identifying the Drivers of Potential Welfare Impacts and Energy Poverty in the European Union* (provisional)

## Research question

Which uncertain drivers (technology, resource and policy assumptions) most shape the potential welfare impacts and energy-poverty risks of EU decarbonisation, so that climate policy can be designed more equitably?

The study identifies drivers and potential impacts. It does not design compensation mechanisms.

## Pipeline

```
EPPA ensembles (400 runs each) -> sector price indices (EUR) -> MEDUSA + HBS 2015 -> DI, energy poverty, transport poverty -> figures + scenario discovery
```

1. **EPPA ensembles.** Four ensembles of 400 runs each: `2C`, `1p5C`, `PF` (policy) and `Ref` (reference). Draw *k* is the same uncertainty sample in every ensemble, so policy-minus-Ref differences are paired by `run_id`. The uncertain inputs of each run are in `data/design/Inputs_Master.xlsx`.
2. **Prices.** One workbook per sector and ensemble (13 sectors), region `EUR`, years 2020-2100.
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
│   ├── 03_scenario_discovery.R    # blocks A-G: input-output, heatmaps, output-output, clustering, direction, extreme runs, filtered mode
│   ├── diag/                      # diagnostics, run on demand (see below)
│   ├── functions/                 # helpers sourced by the scripts (see below)
│   └── tests/                     # checks of the MEDUSA changes (HBS allocation, shocks, dose-response)
├── data/                          # see data/README.md
├── outputs/                       # see outputs/README.md
├── manuscript/                    # outline, notes, bibliography, drafts
├── docs/                          # notes and decisions

```

### `R/diag/`

| Script | Purpose |
|---|---|
| `di_decomposition.R` | Exact decomposition of DI by commodity group, EPPA sector and services by COICOP division; sensitivity variants (gas/solid fixed, energy only, non-energy only, numeraire-neutral) |
| `screen_scenarios.R` | Screens the EPPA price trajectories and writes the screening table |
| `diag_outliers.R` | Looks at flagged / outlier scenarios |
| `eppa_price_compare.R` | EPPA sector price index per ensemble (median and 10-90 band), for comparing ensembles |
| `hbs_expenditure_pies.R` | Pie charts of the expenditure structure (14 categories, energy and transport fuels split out) by country and national income quintile, weighted; one figure for a slide, tables in `outputs/tables/` |

### `R/functions/`

| File | Purpose |
|---|---|
| `run_ensemble.R` | Runs one ensemble through MEDUSA in resumable chunks; builds and writes the platform tables |
| `prep_hbs.R` | HBS cleaning helpers |
| `flagged_scenarios.R` | `load_flagged()` / `drop_flagged()` (honours `drop_flagged_scenarios`) |
| `plot_helpers.R` | Plot theme, palettes and `save_fig()` (PNG, optional subfolder) |
| `scenario_discovery.R` | Helpers of script 03: design file reader, random forest (importance, permutation importance, OOB AUC), CART, clustering, direction (Spearman), partial dependence (1-D, 2-D), `sd_adverse()` (DI sign convention), `sd_filter_select()` (filtered mode) and the parallel-axes plots |
| `sd_helpers.R` | Earlier scenario-discovery helpers, superseded by `scenario_discovery.R`; not used by script 03 |

## Scenario discovery (`R/03_scenario_discovery.R`)

Which EPPA uncertainties (and prices) separate the runs that are bad for households from the rest? A random forest (100 trees, depth 4, seed 0), a depth-4 CART and permutation importance are fitted on the 400 runs of each ensemble (400 = 399 for 2C once the flagged run is dropped). The skill of each forest is reported as the out-of-bag AUC (`oob_auc` in the tables).

**Conventions**

- Adversity: larger = worse for households. DI is a welfare change in % (negative = loss), so a larger loss is worse (adversity = -DI; `di_loss_is_negative <- TRUE` in section 0). EP and TP are poverty rates: higher = worse. `sd_adverse()` in `R/functions/scenario_discovery.R` holds this rule for every block.
- "Worst" runs = the worst 30% of the ensemble for one indicator (`adverse_pct <- 70`). "Best" = smallest DI loss, lowest EP and TP.
- Only the level of each ensemble is analysed (the earlier policy-minus-Ref "diff" mode was removed because it was hard to interpret).
- Year: `sc_year <- 2030` for blocks A, C and the cluster outputs. 2C and 1.5C are almost identical in 2030, so 2035 or 2040 is better for comparing them (open item).

**Blocks**

| Block | What it does | Figures (`outputs/figures/`) |
|---|---|---|
| A | Inputs -> worst 30% of runs, at `sc_year` (importance + parallel axes, CART) | `03_input_output/` |
| B | Input importance over time (heatmap, top inputs) | `03_heatmaps/` |
| C | EPPA sector prices -> worst 30% (output-output) | `03_output_output/` |
| D | k-means clustering of the 2020-2040 trajectories (DI, EP, TP; k = 3; cluster 1 = most adverse), binary RF per cluster, outputs by cluster | `03_clusters/` |
| E | Direction (Spearman rho vs adversity: red = a higher input makes the outcome worse, blue = better) and shape (1-D and 2-D partial dependence) of the input effects | `03_direction_pdp/` |
| F | Extreme runs: the best (least harm at the same climate target) and worst runs, and the inputs that characterise them | `03_extreme_cases/` |
| G | Filtered mode (see below) | `04_filtered_*/` |

**Block G: filtered mode** (as in the JP-Data-Viz platform). A case selects the runs that meet ALL its conditions on DI, EP and TP at once; the RF/CART then ask which inputs separate those runs from the rest.

- Bounds are adversity percentiles of the ensemble (0 = best run, 1 = worst run).
- Default cases (`filter_cases` in section 0): "Best on all three" and "Worst on all three" use `q = "auto"`: one common fraction q (the same on DI, EP and TP) tuned so that about `filter_n_target <- 100` runs satisfy all three conditions (the log prints q and the number of runs; q is usually about 0.5). This is not the 100 runs with the best combined score. The third case, "Low EP and TP (best 45%)", keeps the runs in the best 45% of both EP and TP.
- The platform recommends 80-150 selected runs of 400; the script warns outside `filter_n_range`.
- Years: `filter_years` (default: `sc_year` and the last year, 2030 and 2040).
- Customise a case with `ind` (which metrics), `q` (one number, or `c(di = .4, ep = .5, tp = .5)`) or explicit `bounds`.
- Figures: `04_filtered_io/` (importance + parallel axes), `04_filtered_cart/`, `04_filtered_runs/` (selected runs x top inputs, as input percentiles), `04_filtered_direction/` (direction on the combined outcome), `04_filtered_common_drivers/` (inputs that matter for DI, EP and TP at once: importance in the weakest of the three).

**Reading the results.** Importance has no sign; use the direction heatmap for the sign and the partial dependence plots for the shape. Input names are the EPPA design columns (for example FF-OIL = oil resource supply elasticity: a higher elasticity damps oil price rises and goes with lower transport poverty; WindGas = a cost parameter whose increase raises energy poverty). A sensible check on any such mechanism is to correlate the input with the corresponding EPPA price index of each run.

## How to run

1. Open `Sampedro_etal_EPPA-EUR.Rproj` in RStudio.
2. Check paths and settings in `config.R` (`path_medusa` must point to the MEDUSA branch `dev-js-eppa`).
3. Place the raw inputs as described in `data/README.md`.
4. Run in order:
   - `R/01_run_medusa.R` (long: about 8 chunks per ensemble and year; resumable from `data/processed/chunks/`)
   - `R/02_figures_results.R`
   - `R/03_scenario_discovery.R` (blocks A-G can be switched on and off in its section 0; see "Scenario discovery" below)
5. If you change the ensembles or the flagged list, delete the caches in `data/processed/` first: `medusa_long.rds`, `price_index_long.rds`, `di_groups_*.rds` (and the chunks of the ensemble you rerun).

## Requirements

- R >= 4.2; MEDUSA (branch `dev-js-eppa`).
- Packages: `here`, `devtools`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `readxl`, `openxlsx`, `ranger`, `rpart`, `partykit`, `GGally`, `patchwork`.
- (Optional) JP-Data-Viz app for interactive exploration of the ensembles.

## Status and open items

- [x] Pipeline 01 -> 02 -> 03 for the four ensembles
- [x] Final EPPA ensembles (the EPPA team may supply edited or additional ensembles)
- [x] Flagged scenarios (truncated or blown-up price trajectories): final list agreed with the EPPA team
- [x] Treatment of coal / gas price shocks (sensitivities); crude oil already dropped
- [x] Final year for the time series (2040) and the main analysis year (`sc_year`: 2030 or 2040, since 2C = 1.5C in 2030)
- [x] Run script 03 on the final ensembles and review the pathways and outliers (run 257 of 2C is flagged in `flagged_scenarios.rds`)
- [x] Fill the source placeholders in `data/README.md` (HBS source and version, EPPA version and extraction date, mappings)
- [ ] Upload to the platform (long format)

## Contact

Jon Sampedro, BC3 Research
jon.sampedro@bc3research.org
