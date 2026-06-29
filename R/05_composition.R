# ============================================================
# 05_composition.R
# Element-level analysis: join Bostjan's post-hoc composition coding
# with response data, produce element summaries and a qualitative
# concordance check (do participants' words match the elements the
# artist thinks were present?).
#
# REVIEW NEEDED: data/raw/composition_coding.csv was derived from
# Bostjan's prose descriptions by the analyst (not by Bostjan himself).
# Have him verify before publishing the report.
# ============================================================

source(here::here("R", "00_setup.R"))

pl <- readRDS(here::here("data", "derived", "part_level.rds"))
cc <- utils::read.csv2(
  here::here("data", "raw", "composition_coding.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "CP1250"
)

# Coerce part to character for the join (pl$part is an ordered factor).
cc$part <- as.character(cc$part)
pl$part_chr <- as.character(pl$part)

# -- Long join: one row per (response, element-present-in-its-part) --
# nrow(joined) > nrow(pl) because each response gets repeated once per
# element coded for its (session, part).
joined <- merge(
  pl, cc,
  by.x = c("session", "part_chr"),
  by.y = c("session", "part"),
  all.x = FALSE  # responses with no coded elements drop out
)

# -- Element-level summary -------------------------------------------
# Mean impact and valence pooled across all responses to all parts
# containing the element. Sample sizes annotated.
elem_summary <- do.call(rbind, lapply(
  split(joined, joined$element_id),
  function(d) {
    data.frame(
      element_id       = d$element_id[1],
      element_type     = d$element_type[1],
      element_label_sl = d$element_label_sl[1],
      n_parts          = length(unique(paste(d$session, d$part_chr))),
      n_responses_val  = sum(!is.na(d$valence)),
      n_responses_imp  = sum(!is.na(d$impact)),
      mean_valence     = mean(d$valence, na.rm = TRUE),
      mean_impact      = mean(d$impact,  na.rm = TRUE),
      sd_valence       = stats::sd(d$valence, na.rm = TRUE),
      sd_impact        = stats::sd(d$impact,  na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
))
rownames(elem_summary) <- NULL

# Sort by mean_impact descending within element_type
elem_summary <- elem_summary[
  order(elem_summary$element_type, -elem_summary$mean_impact), ]

# -- Grand means for reference ---------------------------------------
GRAND <- list(
  mean_valence = mean(pl$valence, na.rm = TRUE),
  mean_impact  = mean(pl$impact,  na.rm = TRUE)
)

# -- Composition matrix (24 parts x elements) ------------------------
# 1 if element is present in (session, part).
# Used in appendix to show compositional clustering.
cc$part_id <- paste0(cc$session, ".", cc$part)
all_parts <- sort(unique(cc$part_id))
all_elems <- unique(cc[, c("element_id", "element_type", "element_label_sl")])
all_elems <- all_elems[order(all_elems$element_type, all_elems$element_id), ]

comp_matrix <- matrix(
  0L,
  nrow = nrow(all_elems), ncol = length(all_parts),
  dimnames = list(all_elems$element_id, all_parts)
)
for (i in seq_len(nrow(cc))) {
  comp_matrix[cc$element_id[i], cc$part_id[i]] <- 1L
}

# -- Qualitative concordance check -----------------------------------
# Stem-regexes for elements that have a plausible lexical signature in
# Slovenian. Elements without a clear signature (e.g. abstract musical
# categories like 'akordi', 'drone') are omitted from the check.
#
# For each element with a stem, compute:
#   - % of responses to parts where element is PRESENT that mention it
#   - % of responses to parts where element is ABSENT that mention it
# A positive gap (present > absent) indicates participants correctly
# perceived what the artist intended.
ELEMENT_STEM_MAP <- list(
  cricki     = "\\bčrič",
  skrzati    = "\\bškrž",
  voda       = "\\bvod[aeo]|\\breka|\\bpotok|\\bnadiž",
  dez        = "\\bdež|\\bdezj",
  ptici      = "\\bptič|\\bpetj",
  jutro      = "\\bjutr",
  vinil      = "\\bvinil|\\bprasket|\\bplošč",
  vrecka     = "\\bvreč|\\bmečk",
  kraguljcki = "\\bkragul|\\bzvonč",
  beli_sum   = "\\bšum",
  kalimba    = "\\bkalimb",
  klarinet   = "\\bklarine",
  saksofon   = "\\bsaks|\\bsax",
  gamelan    = "\\bgamel|\\bgong",
  terenski   = "\\bterensk|\\bposnet|\\brafut|\\blijak"
)

# Per-response: which elements does the text mention?
# Result: logical matrix (nrow(pl) x n_elements_with_stems).
text_low <- tolower(pl$text)
text_low[is.na(text_low)] <- ""
mentioned <- vapply(
  ELEMENT_STEM_MAP,
  function(rex) grepl(rex, text_low, perl = TRUE),
  logical(nrow(pl))
)
colnames(mentioned) <- names(ELEMENT_STEM_MAP)

# Per-response: which elements were present in the composition?
# Build a presence matrix indexed the same way.
present <- vapply(
  names(ELEMENT_STEM_MAP),
  function(eid) {
    parts_with_e <- paste(cc$session[cc$element_id == eid],
                          cc$part[cc$element_id == eid], sep = ".")
    pl_part_id <- paste(pl$session, pl$part_chr, sep = ".")
    pl_part_id %in% parts_with_e
  },
  logical(nrow(pl))
)
colnames(present) <- names(ELEMENT_STEM_MAP)

# Build concordance table.
concordance <- do.call(rbind, lapply(names(ELEMENT_STEM_MAP), function(eid) {
  pres <- present[, eid]
  ment <- mentioned[, eid] & !is.na(pl$text)
  data.frame(
    element_id     = eid,
    n_resp_present = sum(pres & !is.na(pl$text)),
    n_resp_absent  = sum(!pres & !is.na(pl$text)),
    pct_mention_when_present = if (sum(pres & !is.na(pl$text)) > 0) {
      100 * sum(ment & pres) / sum(pres & !is.na(pl$text))
    } else NA_real_,
    pct_mention_when_absent = if (sum(!pres & !is.na(pl$text)) > 0) {
      100 * sum(ment & !pres) / sum(!pres & !is.na(pl$text))
    } else NA_real_,
    stringsAsFactors = FALSE
  )
}))
concordance$gap_pp <- concordance$pct_mention_when_present -
  concordance$pct_mention_when_absent
concordance <- merge(
  concordance,
  unique(cc[, c("element_id", "element_label_sl", "element_type")]),
  by = "element_id"
)
concordance <- concordance[order(-concordance$gap_pp), ]

# -- Save -------------------------------------------------------------
saveRDS(elem_summary,
        here::here("data", "derived", "element_summary.rds"))
saveRDS(comp_matrix,
        here::here("data", "derived", "composition_matrix.rds"))
saveRDS(concordance,
        here::here("data", "derived", "concordance.rds"))
saveRDS(GRAND,
        here::here("data", "derived", "grand_means.rds"))

message("Element summary: ", nrow(elem_summary), " elements; ",
        "concordance table: ", nrow(concordance), " elements with stems.")

if (interactive()) {
  cat("Grand means: valence =", round(GRAND$mean_valence, 2),
      " impact =", round(GRAND$mean_impact, 2), "\n\n")

  cat("Element summary (sorted by mean_impact within type):\n")
  print(elem_summary[, c("element_type", "element_label_sl",
                         "n_parts", "n_responses_imp",
                         "mean_valence", "mean_impact")])

  cat("\nConcordance check (artist coding vs participant words):\n")
  print(concordance[, c("element_label_sl", "n_resp_present",
                        "pct_mention_when_present",
                        "pct_mention_when_absent", "gap_pp")])
}
