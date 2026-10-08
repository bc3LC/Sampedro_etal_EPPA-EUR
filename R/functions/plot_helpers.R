# R/functions/plot_helpers.R
# Shared plot theme, palettes and figure-saving helper.
# Edit here once; every figure in 02_ and 03_ picks it up.

library(ggplot2)

theme_paper <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(colour = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90"),
      axis.title       = element_text(colour = "grey25"),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold")
    )
}

# Palette for clusters / groups (colour-blind friendly, Okabe-Ito)
pal_cluster <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#56B4E9")

# Sequential palette for importance heatmaps
scale_fill_importance <- function(...) {
  scale_fill_gradient(low = "white", high = "#0b3d91", limits = c(0, NA), ...)
}

# Save a figure as PDF (editable vector) + PNG (quick look).
# Usage: save_fig(p, "03_rf_importance", width = 7, height = 4)
save_fig <- function(plot, name, dir = path_fig, width = 7, height = 4.5, dpi = 300) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".pdf")), plot, width = width, height = height,
         device = cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), plot, width = width, height = height,
         dpi = dpi, bg = "white")
  invisible(plot)
}
