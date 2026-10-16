# Orchestration of the MEDUSA runs for the platform (analysis repo, not part of the package).
# Uses medusa::get_eppa_prices, build_coicop_shocks, di_total_eu, calc_ep_eu and calc_tp_eu.


# Keep only the expenditure columns the three indicators need: CP00, the shocked codes, their
# ancestors (needed for the roll-up) and the index codes of the poverty indicators. All
# non-expenditure columns are kept. Results are identical; the shock step copies far fewer columns.
slim_hbs_eu <- function(hbs, targets,
                        index_codes = c("CP0411", "CP0421", "CP0731", "CP0732", "CP0733", "CP0734", "CP0735")) {
  cp <- grep("^CP\\d+$", names(hbs), value = TRUE)
  codes <- unique(c(targets, index_codes))
  anc <- unique(unlist(lapply(codes, function(t) { n <- seq_len(nchar(t) - 1); n <- n[n >= 4]
    if (length(n)) substring(t, 1, n) else character(0) })))
  keep_cp <- intersect(cp, unique(c("CP00", codes, anc)))
  hbs[, c(setdiff(names(hbs), cp), keep_cp)]
}

# columns of the shocks table that belong to the given scenario ids
chunk_shocks_eu <- function(shocks, ids) {
  keep <- vapply(strsplit(setdiff(names(shocks), "coicop"), "_"), function(p) tail(p, 1) %in% ids, logical(1))
  shocks[, c("coicop", setdiff(names(shocks), "coicop")[keep])]
}

#' indicators_eu
#'
#' EU value of the three headline indicators for the scenarios in `shocks`: distributional
#' impact (di_total_eu), energy poverty (`ep_index` of calc_ep_eu, EU column) and transport
#' poverty (`tp_index` of calc_tp_eu, EU column, optionally without some countries).
#' @param hbs cleaned HBS.
#' @param shocks shocks table for a set of scenarios.
#' @param ep_index,tp_index indicator names (e.g. "LIHC", "VTU").
#' @param tp_exclude countries left out of the transport-poverty EU value (e.g. "DE").
#' @return list(scen = data.frame(sce, di, ep, tp), base = c(ep, tp)).
indicators_eu <- function(hbs, shocks, ep_index = "LIHC", tp_index = "VTU", tp_exclude = NULL) {
  ids <- unique(sub(".*_", "", setdiff(names(shocks), "coicop")))
  di <- medusa::di_total_eu(hbs, shocks)
  ep <- medusa::calc_ep_eu(hbs, shocks = shocks)
  hbs_tp <- if (length(tp_exclude)) hbs[!hbs$country %in% tp_exclude, ] else hbs
  tp <- medusa::calc_tp_eu(hbs_tp, shocks = shocks)
  pick <- function(x, col, idx) {
    d <- x[x[[col]] == idx, c("scenario", "EU")]
    stats::setNames(d$EU, d$scenario)
  }
  e <- pick(ep, "EP_index", ep_index); t <- pick(tp, "TP_index", tp_index)
  list(scen = data.frame(sce = ids, di = unname(di[ids]), ep = unname(e[ids]), tp = unname(t[ids]),
                         stringsAsFactors = FALSE),
       base = c(ep = unname(e["base"]), tp = unname(t["base"])))
}

#' run_ensemble_eu
#'
#' Runs MEDUSA for every scenario of one EPPA ensemble and every year: reads the sector prices,
#' builds the COICOP shocks, and returns the EU value of DI, EP and TP per scenario and year.
#' Scenarios are processed in chunks, and each chunk is saved so an interrupted run can resume.
#' @param eppa_path folder with the EPPA price files of the ensemble.
#' @param years years to compute (2020 is the base and is added in assemble_platform_eu).
#' @param hbs cleaned HBS 2015.
#' @param countries country names for the shocks (build_coicop_shocks).
#' @param chunk number of scenarios per chunk.
#' @param scenarios optional subset of scenario ids (e.g. "100") for quick tests; no chunks are saved.
#' @param slim if TRUE (default), drop expenditure columns that the indicators do not use (faster).
#' @param out_dir folder where chunks are saved (NULL = no saving / no resuming).
#' @param ensemble label used in the chunk file names.
#' @inheritParams indicators_eu
#' @return list(res = long data.frame sce, year, di, ep, tp; base = c(ep, tp)).
run_ensemble_eu <- function(eppa_path, years = c(2025, 2030, 2035, 2040), hbs, countries,
                            hbs_codes = grep("^CP\\d+$", names(hbs), value = TRUE),
                            bridge = medusa::cpa_coicop_eurBridge, map_cpa = medusa::map_eppa_cpa,
                            ep_index = "LIHC", tp_index = "VTU", tp_exclude = NULL,
                            chunk = 50, out_dir = NULL, ensemble = "ens", slim = TRUE, scenarios = NULL, verbose = TRUE) {
  if (!is.null(scenarios)) out_dir <- NULL     # test runs never write chunks (they would be reused by a full run)
  if (slim) {
    part <- medusa::coicop_partition(bridge, map_cpa, hbs_codes)
    hbs <- slim_hbs_eu(hbs, part)
    if (verbose) message("HBS reduced to ", sum(grepl("^CP\\d+$", names(hbs))), " expenditure columns")
  }
  if (!is.null(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  res <- list(); base <- NULL
  for (yr in years) {
    prices <- medusa::get_eppa_prices(eppa_path, yr)
    sh <- medusa::build_coicop_shocks(prices, bridge, map_cpa, countries, hbs_codes, verbose = FALSE)$shocks
    ids <- unique(sub("^[^_]+_", "", setdiff(names(sh), "coicop")))
    ids <- ids[order(suppressWarnings(as.numeric(ids)), ids)]
    if (!is.null(scenarios)) ids <- ids[ids %in% as.character(scenarios)]
    for (k in seq_along(split(ids, ceiling(seq_along(ids) / chunk)))) {
      idk <- split(ids, ceiling(seq_along(ids) / chunk))[[k]]
      f <- if (!is.null(out_dir)) file.path(out_dir, sprintf("%s_%d_chunk%02d.rds", ensemble, yr, k)) else NULL
      if (!is.null(f) && file.exists(f)) { r <- readRDS(f); if (verbose) message(basename(f), ": loaded") }
      else {
        t0 <- Sys.time()
        r <- indicators_eu(hbs, chunk_shocks_eu(sh, idk), ep_index, tp_index, tp_exclude)
        if (!is.null(f)) saveRDS(r, f)
        if (verbose) message(ensemble, " ", yr, " chunk ", k, " (", length(idk), " scenarios): ",
                             round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
      }
      res[[length(res) + 1]] <- cbind(r$scen[, "sce", drop = FALSE], year = yr, r$scen[, c("di", "ep", "tp")])
      base <- r$base
    }
  }
  list(res = do.call(rbind, res), base = base)
}

#' assemble_platform_eu
#'
#' Builds the three wide tables of the platform (scenario x year, 2020-2100 every 5 years):
#' 2020 is the base year (no price shock: DI = 0; EP and TP = the base-HBS rate), the computed
#' years follow, and later years repeat the last computed year. Values are absolute, as MEDUSA
#' computes them: DI = mean impact on household expenditure in % (negative = welfare loss);
#' EP and TP = share of households.
#' @param run output of run_ensemble_eu().
#' @param years_all years of the platform table.
#' @return list(di, ep, tp), each a matrix with scenarios in rows and years in columns.
assemble_platform_eu <- function(run, years_all = seq(2020, 2100, 5)) {
  r <- run$res; ids <- unique(r$sce); yrs <- sort(unique(r$year))
  one <- function(col, base2020, tf = identity) {
    m <- matrix(NA_real_, length(ids), length(years_all), dimnames = list(ids, years_all))
    m[, "2020"] <- base2020
    for (y in yrs) m[, as.character(y)] <- tf(r[[col]][match(paste(ids, y), paste(r$sce, r$year))])
    last <- as.character(max(yrs))
    for (y in years_all[years_all > max(yrs)]) m[, as.character(y)] <- m[, last]
    m
  }
  list(di = one("di", 0),
       ep = one("ep", run$base[["ep"]]),
       tp = one("tp", run$base[["tp"]]))
}

#' write_platform_xlsx
#'
#' Writes one platform table with the layout of the template (sheet "EUR": three label columns,
#' the scenario id and one column per year; the other sheets of the template are kept).
#' @param m matrix from assemble_platform_eu().
#' @param file output .xlsx.
#' @param template the example .xlsx (e.g. di_2C.xlsx).
#' @param labels the three label columns (e.g. c("distributional impacts", "di", "di")).
write_platform_xlsx <- function(m, file, template, labels, sheet = "EUR") {
  wb <- openxlsx::loadWorkbook(template)
  if (sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)
  hdr <- as.data.frame(rbind(c(NA, NA, NA, NA, colnames(m))), stringsAsFactors = FALSE)
  body <- data.frame(labels[1], labels[2], labels[3], rownames(m), m, stringsAsFactors = FALSE, check.names = FALSE)
  openxlsx::writeData(wb, sheet, hdr, colNames = FALSE, startRow = 1)
  openxlsx::writeData(wb, sheet, body, colNames = FALSE, startRow = 2)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
