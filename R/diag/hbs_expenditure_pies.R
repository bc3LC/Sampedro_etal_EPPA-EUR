# ==============================================================================
# R/diag/hbs_expenditure_pies.R
# Household budget structure: main expenditure categories by country and national income quintile (pie charts)
#
# Share of category k for country c and quintile q = sum_h w_h * exp_hk / sum_h w_h * exp_h(all categories)
# i.e. the weighted share of the group's TOTAL spending (not the average of household shares), using hbs$weight.
# Quintiles are the national ones (column `quintile`, ranked within each country).
#
# Categories: the 12 COICOP divisions, with household energy (CP045) split out of housing (CP04) and
# transport fuels (CP0722) split out of transport (CP07) -> 14 categories.
#
# Reads : data/processed/hbs_2015_clean.rds
# Writes: outputs/tables/diag_hbs_expenditure_shares.csv  (country, quintile, category, share_pct, n_households, spend_eur)
#         outputs/figures/diag_hbs_expenditure/hbs_pies_all.png            (slide, 16:9: countries as columns x quintiles as rows)
#         outputs/figures/diag_hbs_expenditure/hbs_pies_<country>.png      (one figure per country, if per_country_figs)
# Run from the project root. It does not need MEDUSA.
# ==============================================================================
source(here::here("config.R"))
for (f in list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)) source(f)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

# ---- settings ----------------------------------------------------------------------------
# One country per region (north, west, centre, south, east, south-east); codes as in hbs$country. Edit freely.
pie_countries   <- c("SE", "DE", "FR", "ES", "RO")
group_var       <- "quintile"       # "quintile" (national) or "decile"
per_country_figs <- FALSE           # also one larger figure per country
show_groups     <- NULL             # NULL = all groups; e.g. c(1, 3, 5) for a lighter slide (lowest, middle, highest quintile)
label_min_pct   <- 4                # slice labels only for shares >= this value (%)

# ---- categories --------------------------------------------------------------------------
# name = label; value = c(plus, minus): spend = sum(plus codes) - sum(minus codes)
cats <- list(
  "Food and non-alcoholic drinks"  = list(plus = "CP01"),
  "Alcohol and tobacco"            = list(plus = "CP02"),
  "Clothing and footwear"          = list(plus = "CP03"),
  "Housing (excl. energy)"         = list(plus = "CP04", minus = "CP045"),
  "Household energy"               = list(plus = "CP045"),
  "Furnishings and household"      = list(plus = "CP05"),
  "Health"                         = list(plus = "CP06"),
  "Transport (excl. fuels)"        = list(plus = "CP07", minus = "CP0722"),
  "Transport fuels"                = list(plus = "CP0722"),
  "Communication"                  = list(plus = "CP08"),
  "Recreation and culture"         = list(plus = "CP09"),
  "Education"                      = list(plus = "CP10"),
  "Restaurants and hotels"         = list(plus = "CP11"),
  "Other goods and services"       = list(plus = "CP12")
)
# energy and transport fuels in warm colours (the focus of the project), the rest muted
pal <- c("Food and non-alcoholic drinks" = "#4C78A8", "Alcohol and tobacco" = "#9D755D", "Clothing and footwear" = "#B6992D",
         "Housing (excl. energy)" = "#54A24B", "Household energy" = "#D55E00", "Furnishings and household" = "#88D27A",
         "Health" = "#B279A2", "Transport (excl. fuels)" = "#79706E", "Transport fuels" = "#F2A900",
         "Communication" = "#9ECAE9", "Recreation and culture" = "#E377C2", "Education" = "#17BECF",
         "Restaurants and hotels" = "#BAB0AC", "Other goods and services" = "#D4D4D4")

# ---- data ----------------------------------------------------------------------------------
hbs <- readRDS(file.path(path_proc, "hbs_2015_clean.rds"))
need <- unique(unlist(lapply(cats, function(x) c(x$plus, x$minus))))
stopifnot("missing columns in hbs" = all(c(need, "country", "weight", group_var) %in% names(hbs)))
num <- function(v) { v <- as.numeric(v); v[is.na(v)] <- 0; v }

spend <- sapply(cats, function(x) num(rowSums(sapply(x$plus, function(cn) num(hbs[[cn]])), na.rm = TRUE)) -
                  if (is.null(x$minus)) 0 else num(rowSums(sapply(x$minus, function(cn) num(hbs[[cn]])), na.rm = TRUE)))
spend <- as.data.frame(spend, check.names = FALSE)
if (any(spend < 0)) message("Note: ", sum(spend < 0), " household x category values are negative (e.g. a sub-code larger than its parent); kept in the sums.")
total <- rowSums(spend)
if ("CP00" %in% names(hbs)) {
  r <- sum(as.numeric(hbs$weight) * total, na.rm = TRUE) / sum(as.numeric(hbs$weight) * num(hbs$CP00), na.rm = TRUE)
  message(sprintf("Sum of the 14 categories = %.1f%% of CP00 (weighted). The shares below use the sum of the categories as 100%%.", 100 * r))
}

d <- data.frame(country = as.character(hbs$country), group = as.integer(hbs[[group_var]]), w = as.numeric(hbs$weight), spend, check.names = FALSE) |>
  filter(!is.na(group), !is.na(w), country %in% pie_countries)
if (!is.null(show_groups)) d <- filter(d, group %in% show_groups)
miss <- setdiff(pie_countries, unique(d$country))
if (length(miss)) warning("countries not found in hbs$country: ", paste(miss, collapse = ", "), " (available: ", paste(sort(unique(as.character(hbs$country))), collapse = ", "), ")")
pie_countries <- intersect(pie_countries, unique(d$country))

# check that group 1 = lowest income (mean equivalised expenditure should rise with the group)
if ("total_exp_eq" %in% names(hbs)) {
  chk <- tibble(group = as.integer(hbs[[group_var]]), w = as.numeric(hbs$weight), x = as.numeric(hbs$total_exp_eq)) |>
    filter(!is.na(group), !is.na(x)) |> group_by(group) |> summarise(mean_exp_eq = weighted.mean(x, w, na.rm = TRUE), .groups = "drop")
  message("Mean equivalised expenditure by ", group_var, " (all countries): ", paste(sprintf("%d: %.0f", chk$group, chk$mean_exp_eq), collapse = " | "))
  if (cor(chk$group, chk$mean_exp_eq, method = "spearman") < 0) warning(group_var, " seems to run from richest (1) to poorest: check labels")
}

# ---- weighted shares by country and group ---------------------------------------------------
cat_names <- names(cats)
shares <- d |> group_by(country, group) |>
  summarise(n_households = n(), across(all_of(cat_names), ~ sum(w * .x)), .groups = "drop") |>
  pivot_longer(all_of(cat_names), names_to = "category", values_to = "spend_eur") |>
  group_by(country, group) |> mutate(share_pct = 100 * spend_eur / sum(spend_eur)) |> ungroup() |>
  mutate(category = factor(category, cat_names), country = factor(country, pie_countries))
dir.create(path_tab, showWarnings = FALSE, recursive = TRUE)
write.csv(arrange(shares, country, group, category), file.path(path_tab, "diag_hbs_expenditure_shares.csv"), row.names = FALSE)
print(shares |> filter(category %in% c("Household energy", "Transport fuels")) |>
        select(country, group, category, share_pct) |> pivot_wider(names_from = category, values_from = share_pct) |>
        mutate(across(where(is.numeric) & !matches("group"), ~ round(.x, 1))), n = Inf)

# ---- figures ----------------------------------------------------------------------------------
grp_lab <- function(g) factor(g, sort(unique(g)), sprintf("%s %d%s", tools::toTitleCase(group_var), sort(unique(g)),
                                                          ifelse(sort(unique(g)) == min(g), " (lowest)", ifelse(sort(unique(g)) == max(g), " (highest)", ""))))
pie_plot <- function(s, ncol_countries = NULL, base = 9) {
  s <- s |> mutate(grp = grp_lab(group)) |> arrange(country, group, desc(category)) |>
    group_by(country, group) |> mutate(pos = cumsum(share_pct) - share_pct / 2) |> ungroup()
  g <- ggplot(s, aes(x = 1, y = share_pct, fill = category)) +
    geom_col(width = 1, colour = "white", linewidth = .25) +
    geom_text(data = filter(s, share_pct >= label_min_pct), aes(x = 1.18, y = pos, label = sprintf("%.0f", share_pct)),
              size = base * 0.3, colour = "grey15") +
    coord_polar(theta = "y", start = 0) + scale_fill_manual(values = pal, name = NULL, drop = FALSE) +
    labs(x = NULL, y = NULL) + theme_void(base_size = base) +
    theme(legend.position = "bottom", strip.text = element_text(size = base, margin = margin(b = 3)), plot.title = element_text(size = base + 3),
          plot.subtitle = element_text(size = base, colour = "grey30"), plot.background = element_rect(fill = "white", colour = NA))
  g
}
fig_dir <- file.path(path_fig, "diag_hbs_expenditure"); dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
sub_txt <- sprintf("Share of total household spending (%%), weighted; national %ss of each country. Labels: shares of %g%% or more.", group_var, label_min_pct)

# slide layout (16:9): countries as columns, income groups as rows, legend on the right
pa <- pie_plot(shares) + facet_grid(grp ~ country, switch = "y") +
  labs(title = "Household expenditure structure by country and income group (HBS 2015)", subtitle = sub_txt) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1), strip.text.x = element_text(face = "bold", size = 11),
        panel.spacing = unit(.1, "lines"), legend.position = "right") +
  guides(fill = guide_legend(ncol = 1))
ggsave(file.path(fig_dir, "hbs_pies_all.png"), pa, width = 13.33, height = 7.5, dpi = 200, bg = "white")

if (per_country_figs) for (cc in pie_countries) {
  p <- pie_plot(filter(shares, country == cc), base = 11) + facet_wrap(~grp, nrow = 1) +
    labs(title = sprintf("Household expenditure structure, %s (HBS 2015)", cc), subtitle = sub_txt) + guides(fill = guide_legend(nrow = 3))
  ggsave(file.path(fig_dir, sprintf("hbs_pies_%s.png", cc)), p, width = 13, height = 4.2, dpi = 200, bg = "white")
}
message("Done. Figures: ", fig_dir, "  Table: ", file.path(path_tab, "diag_hbs_expenditure_shares.csv"))
