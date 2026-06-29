# ============================================================
# 02_plots.R
# Plotting functions, all base graphics (one ggalluvial exception
# for static Sankey in PDF output).
# Each function returns the plot object invisibly so it can be
# called inside .save_plot() or directly in an Rmd chunk.
# ============================================================

source(here::here("R", "00_setup.R"))

# -- Theme-ish defaults -----------------------------------------------
.pal_diverging <- function(n = 11) {
  # red (aggravation) -> grey (neutral) -> blue (relief)
  grDevices::colorRampPalette(
    c("#b2182b", "#ef8a62", "#fddbc7",
      "#f7f7f7",
      "#d1e5f0", "#67a9cf", "#2166ac")
  )(n)
}

# =====================================================================
# Heat maps: session x part for valence and impact
# =====================================================================

#' Heatmap of mean score per session x part
#'
#' @param df part_level data frame
#' @param outcome "valence" or "impact"
#' @param title plot title
plot_heatmap_session_part <- function(df, outcome = c("valence", "impact"),
                                      title = NULL) {
  outcome <- match.arg(outcome)
  # Aggregate
  agg <- stats::aggregate(
    df[[outcome]],
    by = list(session = df$session, part = df$part),
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  names(agg)[3] <- "mean"

  # Build matrix sessions (rows) x parts (cols)
  sessions <- levels(df$session)
  parts <- levels(df$part)
  mat <- matrix(NA_real_, nrow = length(sessions), ncol = length(parts),
                dimnames = list(sessions, parts))
  for (i in seq_len(nrow(agg))) {
    mat[as.character(agg$session[i]), as.character(agg$part[i])] <- agg$mean[i]
  }

  # Same diverging palette for both outcomes; midpoint is the scale midpoint (5)
  pal <- .pal_diverging(11)
  breaks <- seq(0, 10, length.out = 12)
  legend_title <- if (outcome == "impact") {
    "Vpliv na tinitus\n(0=poslab., 10=olajš.)"
  } else {
    "Prijetnost\n(0=neprijetno, 10=prijetno)"
  }

  # Plot
  graphics::par(mar = c(4, 5, 3, 8))
  graphics::image(
    x = seq_along(parts), y = seq_along(sessions),
    z = t(mat),
    col = pal, breaks = breaks,
    xlab = "", ylab = "", axes = FALSE,
    main = title %||% paste("Povprečna ocena -", outcome)
  )
  graphics::axis(1, at = seq_along(parts),
                 labels = unname(SLO_LABELS$part[parts]))
  graphics::axis(2, at = seq_along(sessions),
                 labels = paste0(sessions, "."), las = 1)
  graphics::box()

  # Cell values overlaid
  for (i in seq_along(sessions)) {
    for (j in seq_along(parts)) {
      v <- mat[i, j]
      if (!is.na(v)) {
        graphics::text(j, i, sprintf("%.1f", v),
                       col = ifelse(v > 7 || v < 3, "white", "black"),
                       cex = 0.9)
      }
    }
  }

  # Legend strip
  .add_color_legend(pal, breaks, legend_title)
  invisible(mat)
}

# Tiny null-coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a

.add_color_legend <- function(pal, breaks, title) {
  usr <- graphics::par("usr")
  # right-side strip
  graphics::par(xpd = NA)
  x0 <- usr[2] + (usr[2] - usr[1]) * 0.04
  x1 <- usr[2] + (usr[2] - usr[1]) * 0.08
  y_breaks <- seq(usr[3], usr[4], length.out = length(breaks))
  for (k in seq_along(pal)) {
    graphics::rect(x0, y_breaks[k], x1, y_breaks[k + 1],
                   col = pal[k], border = NA)
  }
  # ticks
  graphics::text(x1, y_breaks[c(1, ceiling(length(y_breaks) / 2),
                                length(y_breaks))],
                 labels = sprintf("%.0f", breaks[c(1,
                                                   ceiling(length(breaks)/2),
                                                   length(breaks))]),
                 pos = 4, cex = 0.75)
  graphics::text((x0 + x1) / 2, usr[4] + (usr[4] - usr[3]) * 0.05,
                 title, cex = 0.75, adj = c(0.5, 0))
  graphics::par(xpd = FALSE)
}

# =====================================================================
# Sample-description bar charts (Q1, Q3, Q4)
# =====================================================================

plot_sample_demographics <- function(ps) {
  graphics::par(mfrow = c(2, 2), mar = c(5, 4, 3, 1))

  # Q1 frequency
  tab1 <- table(ps$freq, useNA = "no")
  graphics::barplot(
    tab1,
    names.arg = unname(SLO_LABELS$freq[names(tab1)]),
    main = "Pogostost tinitusa", col = "#67a9cf",
    ylab = "Število odgovorov"
  )

  # Q3 severity
  tab3 <- table(ps$severity, useNA = "no")
  graphics::barplot(
    tab3,
    names.arg = unname(SLO_LABELS$severity[names(tab3)]),
    main = "Resnost v vsakdanjem življenju", col = "#fdae61",
    ylab = "Število odgovorov", las = 2, cex.names = 0.8
  )

  # Q4 masker experience
  tab4 <- table(ps$masker_exp, useNA = "no")
  graphics::barplot(
    tab4,
    names.arg = unname(SLO_LABELS$masker_exp[names(tab4)]),
    main = "Prejšnje izkušnje z zvočnimi maskerji", col = "#74add1",
    ylab = "Število odgovorov", las = 2, cex.names = 0.8
  )

  # Type composition (multi-select, share who marked each)
  types <- c(
    Piskanje  = mean(ps$type_piskanje,  na.rm = TRUE),
    Šumenje   = mean(ps$type_sumenje,   na.rm = TRUE),
    Brenčanje = mean(ps$type_brencanje, na.rm = TRUE),
    Bobnenje  = mean(ps$type_bobnenje,  na.rm = TRUE),
    Drugo     = mean(ps$type_has_other, na.rm = TRUE)
  )
  graphics::barplot(
    100 * types,
    main = "Zaznavanje tinitusa (delež)",
    col = "#9970ab",
    ylab = "Delež odgovorov (%)", las = 2, cex.names = 0.85
  )

  graphics::par(mfrow = c(1, 1))
  invisible(NULL)
}

# =====================================================================
# RQ7: valence vs impact scatter
# =====================================================================

#' Scatter of valence (x) vs impact (y), pooled across all parts.
#' Optionally coloured by a subgroup variable with per-group OLS fits.
#'
#' @param pl part_level data frame
#' @param group "severity", "primary_type", or "none"
#' @param min_per_group drop groups with fewer than this many non-NA pairs
plot_valence_vs_impact <- function(pl,
                                   group = c("none", "severity",
                                             "primary_type"),
                                   min_per_group = 10L) {
  group <- match.arg(group)
  pl <- pl[!is.na(pl$valence) & !is.na(pl$impact), ]

  graphics::par(mar = c(4, 4, 3, 10))

  if (group == "none") {
    rho <- suppressWarnings(stats::cor(pl$valence, pl$impact,
                                       method = "spearman"))
    graphics::plot(
      jitter(pl$valence, 0.5), jitter(pl$impact, 0.5),
      xlim = c(0, 10), ylim = c(0, 10),
      xlab = "Prijetnost (0-10)",
      ylab = "Vpliv na tinitus (0=poslabš., 10=olajš.)",
      main = sprintf("Prijetnost vs vpliv (rho = %.2f)", rho),
      pch = 19, col = grDevices::adjustcolor("#2166ac", 0.4)
    )
    graphics::abline(h = 5, lty = 2, col = "grey50")
    graphics::abline(stats::lm(impact ~ valence, data = pl),
                     col = "#b2182b", lwd = 2)
    graphics::par(mar = c(5, 4, 4, 2) + 0.1)
    return(invisible(NULL))
  }

  pl <- pl[!is.na(pl[[group]]), ]
  pl[[group]] <- droplevels(as.factor(pl[[group]]))
  group_n <- table(pl[[group]])
  drop <- names(group_n)[group_n < min_per_group]
  if (length(drop)) pl <- pl[!pl[[group]] %in% drop, ]
  pl[[group]] <- droplevels(pl[[group]])

  lvls <- levels(pl[[group]])
  # Colour palette: diverging for severity, qualitative for type
  cols <- if (group == "severity") {
    grDevices::colorRampPalette(c("#2166ac", "#bdbdbd", "#b2182b"))(length(lvls))
  } else {
    c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02")[seq_along(lvls)]
  }
  names(cols) <- lvls

  point_col <- grDevices::adjustcolor(cols[as.character(pl[[group]])], 0.45)

  graphics::plot(
    jitter(pl$valence, 0.5), jitter(pl$impact, 0.5),
    xlim = c(0, 10), ylim = c(0, 10),
    xlab = "Prijetnost (0-10)",
    ylab = "Vpliv na tinitus (0=poslabš., 10=olajš.)",
    main = if (group == "severity") "Prijetnost vs vpliv, po resnosti tinitusa"
           else                       "Prijetnost vs vpliv, po podtipu tinitusa",
    pch = 19, col = point_col
  )
  graphics::abline(h = 5, lty = 2, col = "grey80")

  # Per-group regression + rho
  rhos <- vapply(lvls, function(g) {
    sub <- pl[pl[[group]] == g, ]
    if (nrow(sub) < 4) return(NA_real_)
    fit <- stats::lm(impact ~ valence, data = sub)
    graphics::abline(fit, col = cols[g], lwd = 2)
    suppressWarnings(stats::cor(sub$valence, sub$impact, method = "spearman"))
  }, numeric(1))

  # Translate level names to Slovenian for legend
  lvl_labels <- if (group == "severity") {
    unname(SLO_LABELS$severity[lvls])
  } else if (group == "primary_type") {
    c(unname(SLO_LABELS$type), "Več tipov")[match(
      lvls, c("piskanje","sumenje","brencanje","bobnenje","drugo","vec_tipov")
    )]
  } else {
    lvls
  }

  legend_text <- sprintf("%s\n  rho=%.2f, n=%d",
                         lvl_labels, rhos,
                         as.integer(table(pl[[group]])[lvls]))

  graphics::par(xpd = NA)
  graphics::legend(
    x = 10.5, y = 10,
    legend = legend_text,
    col = cols, lwd = 2, pch = 19,
    bty = "n", cex = 0.8, y.intersp = 1.5
  )
  graphics::par(xpd = FALSE, mar = c(5, 4, 4, 2) + 0.1)
  invisible(NULL)
}

# =====================================================================
# Subgroup boxplots (impact ~ primary_type, ~ severity, ~ masker_exp)
# =====================================================================

#' Boxplot of an outcome split by a subgroup variable.
#'
#' Pools across composition parts: each person-session contributes 3
#' observations (one per part) to the outcome distribution. The function
#' does not split by part itself — parts of different sessions are not
#' meaningfully comparable.
#'
#' @param pl part_level data frame (rows: response x part)
#' @param group grouping column name (must exist in pl)
#' @param outcome "valence" or "impact"
#' @param min_per_group drop groups with fewer than this many obs
plot_subgroup_box <- function(pl, group,
                              outcome = c("valence", "impact"),
                              min_per_group = 10L) {
  outcome <- match.arg(outcome)
  pl <- pl[!is.na(pl[[group]]) & !is.na(pl[[outcome]]), ]

  group_n <- table(pl[[group]])
  drop <- names(group_n)[group_n < min_per_group]
  if (length(drop)) pl <- pl[!pl[[group]] %in% drop, ]

  if (nrow(pl) == 0) {
    graphics::plot.new()
    graphics::title(main = paste("Premalo podatkov za", group))
    return(invisible(NULL))
  }

  pl[[group]] <- droplevels(as.factor(pl[[group]]))

  # Map factor levels to Slovenian display labels where available
  group_labels <- if (group == "primary_type") {
    c(unname(SLO_LABELS$type), "Več tipov")[match(
      levels(pl[[group]]),
      c("piskanje","sumenje","brencanje","bobnenje","drugo","vec_tipov")
    )]
  } else if (group == "severity") {
    unname(SLO_LABELS$severity[levels(pl[[group]])])
  } else if (group == "masker_exp") {
    unname(SLO_LABELS$masker_exp[levels(pl[[group]])])
  } else if (group == "freq") {
    unname(SLO_LABELS$freq[levels(pl[[group]])])
  } else if (group == "awareness_cat") {
    unname(SLO_LABELS$awareness[levels(pl[[group]])])
  } else {
    levels(pl[[group]])
  }

  # Annotate sample size on labels
  ns <- as.integer(table(pl[[group]]))
  group_labels <- sprintf("%s\n(n=%d)", group_labels, ns)

  group_title <- c(
    primary_type  = "podtipu tinitusa",
    severity      = "resnosti tinitusa",
    masker_exp    = "prejšnjih izkušnjah z maskerji",
    freq          = "pogostosti tinitusa",
    awareness_cat = "zavedanju tinitusa med kopeljo"
  )[group]
  if (is.na(group_title)) group_title <- group

  graphics::par(mar = c(6, 4, 3, 1))
  graphics::boxplot(
    pl[[outcome]] ~ pl[[group]],
    names  = group_labels,
    las    = 1,
    ylim   = c(0, 10),
    xlab   = "",                                   # silence default
    ylab   = ifelse(outcome == "valence",
                    "Prijetnost (0-10)",
                    "Vpliv (0=poslabš., 10=olajš.)"),
    main   = paste0(
      ifelse(outcome == "valence", "Prijetnost", "Vpliv na tinitus"),
      " glede na ", group_title
    ),
    col    = "#cbd5e8",
    cex.axis = 0.85
  )
  if (outcome == "impact") graphics::abline(h = 5, lty = 2, col = "grey50")
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  invisible(NULL)
}

message("Plot functions loaded.")

# =====================================================================
# Element-level analysis plots
# =====================================================================

#' Horizontal bar chart of mean impact (or valence) per element
#'
#' @param elem_summary data frame from 05_composition.R
#' @param grand_mean reference mean (numeric)
#' @param type "musical" or "masking"
#' @param outcome "impact" or "valence"
#' @param min_n_parts drop elements present in fewer than this many parts
plot_element_means <- function(elem_summary, grand_mean,
                               type = c("musical", "masking"),
                               outcome = c("impact", "valence"),
                               min_n_parts = 2L) {
  type <- match.arg(type)
  outcome <- match.arg(outcome)
  d <- elem_summary[elem_summary$element_type == type &
                      elem_summary$n_parts >= min_n_parts, ]
  if (nrow(d) == 0) {
    graphics::plot.new()
    graphics::title("Premalo elementov za prikaz")
    return(invisible(NULL))
  }
  col_mean <- paste0("mean_", outcome)
  col_n    <- paste0("n_responses_",
                     ifelse(outcome == "impact", "imp", "val"))
  d <- d[order(d[[col_mean]]), ]  # ascending — most positive on top

  # Label string with N
  lbl <- sprintf("%s (n=%d)", d$element_label_sl, d[[col_n]])

  # Bars coloured by direction from grand mean
  cols <- ifelse(d[[col_mean]] >= grand_mean, "#2166ac", "#b2182b")

  graphics::par(mar = c(4, 11, 3, 2))
  bp <- graphics::barplot(
    d[[col_mean]],
    horiz = TRUE,
    names.arg = lbl,
    las = 1,
    xlim = c(0, 10),
    col = cols, border = NA,
    main = paste0(
      ifelse(outcome == "impact",
             "Povprečen vpliv na tinitus",
             "Povprečna prijetnost"),
      ": ",
      ifelse(type == "musical",
             "glasbeni elementi",
             "elementi prekrivanja")
    ),
    xlab = ifelse(outcome == "impact",
                  "Povprečna ocena vpliva (0=poslabš., 10=olajš.)",
                  "Povprečna prijetnost (0-10)"),
    cex.names = 0.85
  )
  # Grand-mean reference line
  graphics::abline(v = grand_mean, lty = 2, col = "grey40", lwd = 1.5)
  graphics::axis(3, at = grand_mean,
                 labels = sprintf("\u00f8 %.1f", grand_mean),
                 cex.axis = 0.75, padj = 1.3, col.axis = "grey40")
  # Annotate bars with value
  graphics::text(
    d[[col_mean]] + 0.15, bp,
    sprintf("%.1f", d[[col_mean]]),
    pos = 4, cex = 0.75, col = "grey20"
  )
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  invisible(d)
}

#' Composition matrix heatmap (24 parts x ~25 elements presence)
plot_composition_matrix <- function(comp_matrix, elements_meta_df = NULL) {
  # Reorder rows: masking elements above musical, alphabetical within type.
  if (!is.null(elements_meta_df)) {
    ord_df <- elements_meta_df[order(elements_meta_df$element_type,
                                     elements_meta_df$element_label_sl), ]
    keep <- intersect(ord_df$element_id, rownames(comp_matrix))
    comp_matrix <- comp_matrix[keep, , drop = FALSE]
    row_labels <- ord_df$element_label_sl[match(rownames(comp_matrix),
                                                ord_df$element_id)]
    row_types  <- ord_df$element_type[match(rownames(comp_matrix),
                                            ord_df$element_id)]
  } else {
    row_labels <- rownames(comp_matrix)
    row_types  <- rep("?", nrow(comp_matrix))
  }

  graphics::par(mar = c(5, 11, 3, 2))
  graphics::image(
    x = seq_len(ncol(comp_matrix)),
    y = seq_len(nrow(comp_matrix)),
    z = t(comp_matrix),
    col = c("white", "#2c3e50"),
    breaks = c(-0.5, 0.5, 1.5),
    xlab = "", ylab = "", axes = FALSE,
    main = "Prisotnost elementov po delih"
  )
  graphics::axis(1, at = seq_len(ncol(comp_matrix)),
                 labels = colnames(comp_matrix), las = 2, cex.axis = 0.75)
  # Row labels coloured by type
  type_col <- ifelse(row_types == "musical", "#7570b3", "#1b9e77")
  graphics::axis(2, at = seq_len(nrow(comp_matrix)),
                 labels = row_labels, las = 1, cex.axis = 0.75,
                 col.axis = "grey20")
  # Tiny coloured chip on the left of each label to show type
  graphics::par(xpd = NA)
  usr <- graphics::par("usr")
  chip_x <- usr[1] - (usr[2] - usr[1]) * 0.005
  graphics::points(
    rep(chip_x, nrow(comp_matrix)),
    seq_len(nrow(comp_matrix)),
    pch = 15, col = type_col, cex = 1.3
  )
  graphics::par(xpd = FALSE)
  graphics::box()
  # Vertical dividers between sessions
  n_per_session <- 3
  for (k in seq(n_per_session, ncol(comp_matrix) - 1, by = n_per_session)) {
    graphics::abline(v = k + 0.5, col = "grey60", lwd = 0.8)
  }
  # Legend
  graphics::legend(
    "topright", inset = c(-0.0, -0.1),
    legend = c("Glasbeni", "Prekrivanje"),
    pch = 15, col = c("#7570b3", "#1b9e77"),
    bty = "n", cex = 0.8, xpd = NA
  )
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  invisible(NULL)
}

# =====================================================================
# Concordance dot-plot
# =====================================================================

#' Dumbbell chart showing whether participants explicitly name each sonic
#' element more often when the artist coded it as present vs absent.
#'
#' For each element with a recognisable Slovenian word-stem, two rates are
#' plotted:
#'   - blue filled dot: % of qualitative responses that mention the element,
#'     among parts where the artist coded the element as PRESENT
#'   - white open dot: same % among parts where the element was ABSENT
#' The gap (present - absent) is the "concordance signal" — a meaningful
#' positive gap means listeners actually noticed and named that element.
#' A gap near zero means the element was not part of listeners' vocabulary,
#' regardless of whether the artist included it.
plot_concordance <- function(concordance, min_n_present = 10L) {
  d <- concordance[concordance$n_resp_present >= min_n_present, ]
  # Biggest gap at the TOP of the chart (most informative first).
  d <- d[order(d$gap_pp), ]   # ascending — plotting at y=1..nrow puts top last

  n <- nrow(d)
  graphics::par(mar = c(5, 11, 3, 8))
  xmax <- max(d$pct_mention_when_present,
              d$pct_mention_when_absent, na.rm = TRUE) * 1.15
  graphics::plot(
    NA, xlim = c(0, xmax), ylim = c(0.5, n + 0.5), axes = FALSE,
    xlab = "Delež odgovorov, ki element omenjajo (%)",
    ylab = "",
    main = "Skladnost med kodirano prisotnostjo in omembo v opisu"
  )
  graphics::axis(1)
  # Faint horizontal guide lines
  graphics::abline(h = seq_len(n), col = "grey95")

  # Highlight rows with meaningful gap (>=2pp) for visual hierarchy
  bg_signal <- d$gap_pp >= 2
  if (any(bg_signal)) {
    graphics::rect(
      xleft   = -xmax * 0.02,
      xright  = xmax,
      ybottom = which(bg_signal) - 0.45,
      ytop    = which(bg_signal) + 0.45,
      col = grDevices::adjustcolor("#fff7bc", 0.5), border = NA
    )
  }

  graphics::axis(2, at = seq_len(n),
                 labels = sprintf("%s (n=%d)", d$element_label_sl,
                                  d$n_resp_present),
                 las = 1, cex.axis = 0.85, tick = FALSE)

  # Segments
  graphics::segments(
    x0 = d$pct_mention_when_absent,
    x1 = d$pct_mention_when_present,
    y0 = seq_len(n),
    col = "grey55", lwd = 1.5
  )
  graphics::points(d$pct_mention_when_absent, seq_len(n),
                   pch = 21, bg = "white", col = "grey30", cex = 1.4)
  graphics::points(d$pct_mention_when_present, seq_len(n),
                   pch = 19, col = "#2166ac", cex = 1.4)

  # Gap annotation in the right margin
  graphics::par(xpd = NA)
  graphics::text(
    xmax * 1.04, seq_len(n),
    sprintf(ifelse(d$gap_pp >= 0, "+%.1f pp", "%.1f pp"), d$gap_pp),
    cex = 0.8, pos = 4,
    col = ifelse(bg_signal, "#7f2704", "grey50"),
    font = ifelse(bg_signal, 2, 1)
  )
  graphics::par(xpd = FALSE)

  # Legend bottom-left, outside the cluttered right side
  graphics::legend(
    "bottomright",
    legend = c("Prisoten v kompoziciji  ",
               "Odsoten iz kompozicije  ",
               "Razlika \u2265 2 odst. točk"),
    pch = c(19, 21, 22), pt.cex = c(1.4, 1.4, 1.6),
    col = c("#2166ac", "grey30", "#fff7bc"),
    pt.bg = c("#2166ac", "white", "#fff7bc"),
    bty = "n", cex = 0.8, inset = c(0.02, 0.02)
  )
  graphics::par(mar = c(5, 4, 4, 2) + 0.1)
  invisible(d)
}
