# data/

Inputs and intermediate data. Most of this folder is **not tracked by Git** (see `.gitignore`); the design matrix, the bridge tables and this README are.

| Folder | Contents | Tracked |
|---|---|---|
| `hbs_data/` | EU Household Budget Survey (`2020/` raw workbooks, `inputs/` processed `.RData` per country) used by `hbs_eu()` | No |
| `eppa_outputs/` | EPPA7 outputs: one workbook per sector and scenario (`{n}_price_{sector}_price_index_{scenario}.xlsx`), sheet `pol_{REGION}`, one row per run, years in columns | No |
| `design/` | `design_matrix.csv`: one row per run, one column per varying parameter | Yes |
| `external/bridge_eppa_medusa/` | Final bridge tables: `map_eppa_cpa.csv`, `map_eppa_sectornames.csv`, `cpa_coicop_eurBridge.csv`. Work-in-progress files live in `WiP/` (not tracked) | Yes (final tables) |
| `publication_database/` | SQL dump with all ensemble results (large, ~380 MB; keep outside Git) | No |
| `processed/` | MEDUSA results (`medusa_di_results.rds/.csv`) and derived tables | No (regenerable) |

## Conventions

- `run_id` is the key linking `design_matrix.csv`, EPPA outputs and MEDUSA results. EPPA workbooks store it as text; scripts convert it to character on both sides before joining.
- Processed results are in long format: `run_id`, `region`, `year`, `group` (e.g., income decile), `indicator`, `value`.
- Never edit raw data by hand. All transformations go through scripts in `R/`.

## Where to get the raw data

- HBS: *(add source, version and access date)*
- EPPA7 outputs: *(add model version, scenario names, extraction date)*
- COICOP/CPA mappings: *(add source files)*
- Publication database: *(add where the SQL dump is stored)*
