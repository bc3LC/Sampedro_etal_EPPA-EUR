# Sampedro et al. - EPPA-EUR

Distributional household impacts of EU decarbonization pathways under uncertainty.

> Working title: *Towards Equitable Climate Policy: Identifying the Drivers of Potential Welfare Impacts and Energy Poverty in the European Union* (provisional)

## Research question

Which uncertain drivers (technology, resource and policy assumptions) most shape the potential welfare impacts and energy-poverty risks of EU decarbonization, so that climate policy can be designed more equitably?

The study identifies drivers and potential impacts. It does not design compensation mechanisms.

## Pipeline

```
Varying parameters -> Sampling -> MIT EPPA7 (NDC-LTT) -> Prices by sector -> MEDUSA -> Distributional impacts -> Scenario discovery
```

1. **Varying parameters**: renewable technology costs, CDR cost/availability, biomass constraint, nuclear availability, energy efficiency improvements, fossil fuel availability (others TBD).
2. **Sampling**: Latin hypercube vs discrete design (to be decided). Stored in `data/design/design_matrix.csv`.
3. **EPPA7**: all scenarios run at the same NDC-LTT policy level; sectoral price indices extracted per run for the EUR region (one workbook per sector and scenario, one sheet per region).
4. **MEDUSA** (R): translates sectoral price changes into household distributional impacts using EU HBS data, through the EPPA -> CPA -> COICOP bridge.
5. **Scenario discovery**: CART, random forest and permutation importance, clustering (and optionally PRIM) on the design matrix vs outcomes. Settings follow the JP-Data-Viz app so results are comparable.

## Repository structure

```
Sampedro_etal_EPPA-EUR/
├── README.md
├── .gitignore
├── Sampedro_etal_EPPA-EUR.Rproj
├── config.R                     # paths and global settings (edit here only)
├── R/
│   ├── 01_run_medusa.R          # HBS + EPPA prices -> MEDUSA -> data/processed
│   ├── 02_figures_results.R     # distribution results and other non-scenario-discovery figures
│   ├── 03_scenario_discovery.R  # CART, RF, permutation importance, clustering
│   └── functions/               # helpers (plot theme, scenario-discovery functions)
├── data/                        # see data/README.md
├── outputs/                     # figures/ and tables/ (generated)
├── manuscript/                  # outline, process notes, bibliography, drafts
└── docs/                        # notes and decisions
```

## How to run

1. Open `Sampedro_etal_EPPA-EUR.Rproj` in RStudio.
2. Check paths and settings in `config.R`.
3. Place raw inputs as described in `data/README.md`.
4. Run the scripts in order:
   - `R/01_run_medusa.R`
   - `R/02_figures_results.R`
   - `R/03_scenario_discovery.R`

## Requirements

- R >= 4.2
- MEDUSA functions (`hbs_eu()`, `get_prices_eppa()`, `calc_di_eu()`)
- Packages: `here`, `dplyr`, `tidyr`, `ggplot2`, `forcats`, `scales`, `ranger`, `rpart`, `rpart.plot`, `GGally` (optional: `prim`, `DBI`, `RMariaDB`)
- (Optional) JP-Data-Viz app for interactive exploration of the ensemble

## Status and open items

- [ ] EPPA -> (CPA) -> COICOP bridge: weights and treatment of CROP / LIVE / COAL / OIL
- [ ] Sampling design (LHS vs discrete) and number of runs
- [ ] Final list of varying parameters and ranges
- [ ] Investigate `before_tax = 0` records and zero-inflated expenditure models
- [ ] Load ensemble into the SQL publication database (in progress)
- [ ] EU country-level extension (deferred; current scope is EUR-level)

## Contact

Jon Sampedro, BC3 Research
