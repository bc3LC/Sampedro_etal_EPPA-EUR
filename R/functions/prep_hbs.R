# R/functions/prep_hbs.R
# Cleans the MEDUSA HBS microdata before price shocks are applied.

prep_hbs <- function(df, floor_negatives = TRUE,
                     cee = c("BG","HR","HU","LT","LV","SK","SI","CZ","EE"),
                     verbose = TRUE) {
  cp <- grep("^CP\\d+$", names(df), value = TRUE)

  # 1. weights numeric
  df$weight <- as.numeric(df$weight)

  # 2. suppressed character columns ("." / NA) -> 0
  for (v in cp) if (is.character(df[[v]])) {
    x <- suppressWarnings(as.numeric(df[[v]])); df[[v]] <- ifelse(is.na(x), 0, x)
  }

  # 3. drop households with non-positive/missing total expenditure or bad weight
  bad <- is.na(df$CP00) | df$CP00 <= 0 | is.na(df$weight) | df$weight <= 0
  if (verbose) message("prep_hbs: dropping ", sum(bad), " households (CP00 <= 0 / NA or bad weight)")
  df <- df[!bad, ]

  # 4. harmonise reporting depth (parents -> leaves)
  others <- setdiff(unique(df$country), "RO")
  df <- allocate_to_leaves(df, receivers = others, verbose = verbose)
  if ("RO" %in% df$country)
    df <- allocate_to_leaves(df, receivers = "RO", donors = cee, verbose = verbose)

  # 4b. anything still NA has nothing to allocate (its parent is 0 / not reported): set to 0
  na_c <- vapply(split(seq_len(nrow(df)), df$country),
                 function(i) sum(is.na(df[i, cp])), numeric(1))
  if (verbose) message("prep_hbs: NA cells set to 0 after allocation: ",
                       if (any(na_c > 0)) paste(names(na_c)[na_c > 0], na_c[na_c > 0], sep = ":", collapse = " ") else "none")
  df[cp] <- lapply(df[cp], function(x) { x[is.na(x)] <- 0; x })

  # 5. negative leaf values (e.g. NL net electricity credits): no price exposure
  if (floor_negatives) {
    leaves <- cp[nchar(cp) == 7]
    n_neg <- sum(sapply(df[leaves], function(x) sum(x < 0, na.rm = TRUE)))
    if (verbose) message("prep_hbs: flooring ", n_neg, " negative leaf values at 0 (parents untouched)")
    df[leaves] <- lapply(df[leaves], function(x) pmax(x, 0))
  }
  df
}

allocate_to_leaves <- function(df, tol = 0.5, receivers = NULL, donors = NULL, verbose = TRUE) {
  cp <- setdiff(grep("^CP\\d+$", names(df), value = TRUE), "CP00")
  mid <- cp[nchar(cp) >= 4 & nchar(cp) <= 6]
  parents <- c("CP00", mid[order(nchar(mid))])
  w   <- df$weight
  rec <- if (is.null(receivers)) rep(TRUE, nrow(df)) else df$country %in% receivers
  don <- if (is.null(donors))    rep(TRUE, nrow(df)) else df$country %in% donors
  log <- character(0)

  for (p in parents) {
    kids <- if (p == "CP00") cp[nchar(cp) == 4] else cp[nchar(cp) == nchar(p) + 1 & startsWith(cp, p)]
    if (!length(kids)) next
    K    <- as.matrix(df[kids]); isna <- is.na(K)
    K0   <- K; K0[isna] <- 0
    pv   <- df[[p]]
    res  <- pv - rowSums(K0)
    need <- rec & !is.na(res) & res > tol & !is.na(pv) & pv > 0
    if (!any(need)) next

    if (length(kids) == 1) {
      share <- 1
    } else {
      ok  <- don & !is.na(pv) & pv > 0 & rowSums(isna) == 0 & abs(res) <= tol
      tot <- colSums(K[ok, , drop = FALSE] * w[ok])
      if (sum(tot) == 0) { warning("no donor households to split ", p); next }
      share <- tot / sum(tot)
    }
    S    <- matrix(share, nrow(K), length(kids), byrow = TRUE)
    S_na <- S * isna
    use_na <- rowSums(isna) > 0 & rowSums(S_na) > 0       # only NA children receive
    S[use_na, ] <- S_na[use_na, ]
    S <- S / rowSums(S)

    K_new <- K0 + res * S
    K_out <- K
    K_out[need, ] <- K_new[need, ]
    df[kids] <- as.data.frame(K_out)
    cn <- paste(names(table(df$country[need])), table(df$country[need]), sep = ":", collapse = " ")
    log <- c(log, sprintf("%s -> %d kids [%s]", p, length(kids), cn))
  }
  if (verbose) message("allocate_to_leaves:\n  ", paste(log, collapse = "\n  "))
  df
}
