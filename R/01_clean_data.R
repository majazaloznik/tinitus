# ============================================================
# 01_clean_data.R
# Convert the raw coded CSV into two tidy data frames:
#   - person_session: one row per (session, row_id) — wide
#   - part_level:     one row per (session, row_id, part) — long
# Saves both as .rds in data/derived/.
# ============================================================

source(here::here("R", "00_setup.R"))

# -- Read raw, skipping the original header --------------------------
# The original header has duplicate column names ('0','1') which would
# get mangled by read.csv. We bypass that by reading without a header
# and assigning meaningful names ourselves.
raw_path <- here::here("data", "raw", "tinitus_2026.csv")

raw <- utils::read.csv(
  raw_path,
  header           = FALSE,
  skip             = 1,
  na.strings       = c("", "NA"),
  stringsAsFactors = FALSE,
  encoding         = "UTF-8",
  check.names      = FALSE
)

# The trailing comma in the header creates a 36th column. If the read
# also produced an empty 37th, drop it.
if (ncol(raw) == 37 && all(is.na(raw[[37]]))) raw[[37]] <- NULL

stopifnot(ncol(raw) == 36)

# -- Assign meaningful names ------------------------------------------
names(raw) <- c(
  "session_raw", "row_id",
  # Q1 frequency
  "q1_stalno", "q1_obcasno",
  # Q2 type (multi-select) + free text
  "q2_piskanje", "q2_sumenje", "q2_brencanje", "q2_bobnenje", "q2_drugo_text",
  # Q3 severity (single-select 1..5)
  "q3_sploh_ne", "q3_malo", "q3_zmerno", "q3_zelo", "q3_ekstremno",
  # Q4 prior masker experience (0..3)
  "q4_nikoli", "q4_enkrat", "q4_veckrat", "q4_redno",
  # Q5 overall valence (-2..2)
  "q5_n2", "q5_n1", "q5_0", "q5_p1", "q5_p2",
  # Q6 awareness during session (0, 0.5, 1)
  "q6_sploh_ne", "q6_vcasih", "q6_celotne",
  # Per-part numeric scales (0..10 each)
  "p1_valence", "p1_impact",
  "p2_valence", "p2_impact",
  "p3_valence", "p3_impact",
  # Per-part free text
  "p1_text", "p2_text", "p3_text",
  # Overall free comment
  "overall_comment"
)

# -- Drop empty rows --------------------------------------------------
# A row is considered empty if session_raw is NA or all-but-id is NA.
empty_row <- is.na(raw$session_raw) |
  rowSums(!is.na(raw[, -c(1, 2)])) == 0
if (any(empty_row)) {
  message("Dropping ", sum(empty_row), " empty rows.")
  raw <- raw[!empty_row, , drop = FALSE]
}

# -- Normalise session label ------------------------------------------
# 'Vii' and 'vii' both occur; coerce to uppercase Roman.
raw$session <- toupper(trimws(raw$session_raw))
roman_levels <- c("I","II","III","IV","V","VI","VII","VIII")
stopifnot(all(raw$session %in% roman_levels))
raw$session <- factor(raw$session, levels = roman_levels, ordered = TRUE)

# -- Q1: frequency ----------------------------------------------------
# Two mutually exclusive flags -> one factor.
freq_mat <- as.matrix(raw[, c("q1_stalno", "q1_obcasno")])
which_freq <- .row_which_one(freq_mat)
raw$freq <- factor(
  c("stalno", "obcasno")[which_freq],
  levels = c("stalno", "obcasno")
)

# -- Q2: type (multi-select) -----------------------------------------
# Keep four binaries as logicals; 'drugo_text' stays as free text.
raw$type_piskanje  <- !is.na(raw$q2_piskanje)  & raw$q2_piskanje  == 1
raw$type_sumenje   <- !is.na(raw$q2_sumenje)   & raw$q2_sumenje   == 1
raw$type_brencanje <- !is.na(raw$q2_brencanje) & raw$q2_brencanje == 1
raw$type_bobnenje  <- !is.na(raw$q2_bobnenje)  & raw$q2_bobnenje  == 1
raw$type_drugo_text <- .clean_text(raw$q2_drugo_text)
raw$type_has_other <- !is.na(raw$type_drugo_text)

raw$n_types <- raw$type_piskanje + raw$type_sumenje +
  raw$type_brencanje + raw$type_bobnenje + raw$type_has_other

# Primary type — used for compact grouping in subgroup plots.
# Rule: if exactly one of the four canonical types is ticked, use that.
# Otherwise label as "Vec_tipov" (multiple) or "Drugo" if only 'drugo' present.
primary <- rep(NA_character_, nrow(raw))
single_canonical <- raw$n_types == 1 & !raw$type_has_other
primary[single_canonical & raw$type_piskanje]  <- "piskanje"
primary[single_canonical & raw$type_sumenje]   <- "sumenje"
primary[single_canonical & raw$type_brencanje] <- "brencanje"
primary[single_canonical & raw$type_bobnenje]  <- "bobnenje"
primary[is.na(primary) & raw$n_types == 1 & raw$type_has_other] <- "drugo"
primary[is.na(primary) & raw$n_types >= 2] <- "vec_tipov"
raw$primary_type <- factor(
  primary,
  levels = c("piskanje", "sumenje", "brencanje", "bobnenje", "drugo", "vec_tipov")
)

# -- Q3: severity (1..5) ---------------------------------------------
# Q3 is conceptually single-select but 9/118 rows tick two adjacent boxes
# (mostly Zmerno+Zelo). Treat those as the midpoint of the two ticked
# levels (e.g. 3.5 for Zm+Z). Flag the uncertainty in a separate column.
# Non-adjacent multi-ticks (none in current data) are coerced to NA.
sev_mat <- as.matrix(raw[, c("q3_sploh_ne", "q3_malo", "q3_zmerno",
                             "q3_zelo", "q3_ekstremno")])
sev_mat[is.na(sev_mat)] <- 0
sev_ticks <- rowSums(sev_mat)
sev_weighted <- as.numeric(sev_mat %*% (1:5))
sev_numeric <- sev_weighted / sev_ticks    # mean of ticked positions
sev_numeric[sev_ticks == 0] <- NA_real_

# Adjacency check: a multi-tick is "adjacent" iff the ticked positions
# form a contiguous run. apply() here is acceptable — runs once over rows.
sev_adjacent <- apply(sev_mat, 1, function(x) {
  pos <- which(x == 1)
  if (length(pos) <= 1) return(TRUE)
  all(diff(pos) == 1)
})
non_adj_multi <- sev_ticks > 1 & !sev_adjacent
if (any(non_adj_multi)) {
  warning(sum(non_adj_multi),
          " rows have non-adjacent multi-tick severity; coerced to NA.")
  sev_numeric[non_adj_multi] <- NA_real_
}

raw$severity_numeric   <- sev_numeric
raw$severity_uncertain <- sev_ticks > 1

# Coarse factor for tabulation (round to nearest integer; ties go up).
sev_int <- round(sev_numeric + 1e-9)  # +eps so 3.5 -> 4 (more conservative)
raw$severity <- factor(
  sev_int,
  levels = 1:5,
  ordered = TRUE,
  labels = c("sploh_ne", "malo", "zmerno", "zelo", "ekstremno")
)

# -- Q4: masker experience (0..3) ------------------------------------
mask_mat <- as.matrix(raw[, c("q4_nikoli", "q4_enkrat",
                              "q4_veckrat", "q4_redno")])
mask_idx <- .row_which_one(mask_mat)
raw$masker_exp <- factor(
  c("nikoli", "enkrat", "veckrat", "redno")[mask_idx],
  levels = c("nikoli", "enkrat", "veckrat", "redno"),
  ordered = TRUE
)

# -- Q5: overall valence (-2..2) -------------------------------------
# Same treatment as Q3: 3 rows in current data have adjacent multi-ticks;
# treat those as the average. Non-adjacent (none observed) -> NA.
val_mat <- as.matrix(raw[, c("q5_n2", "q5_n1", "q5_0", "q5_p1", "q5_p2")])
val_mat[is.na(val_mat)] <- 0
val_ticks <- rowSums(val_mat)
val_weighted <- as.numeric(val_mat %*% (-2:2))
val_numeric  <- val_weighted / val_ticks
val_numeric[val_ticks == 0] <- NA_real_

val_adjacent <- apply(val_mat, 1, function(x) {
  pos <- which(x == 1); if (length(pos) <= 1) return(TRUE); all(diff(pos) == 1)
})
non_adj_val <- val_ticks > 1 & !val_adjacent
if (any(non_adj_val)) {
  warning(sum(non_adj_val),
          " rows have non-adjacent multi-tick valence; coerced to NA.")
  val_numeric[non_adj_val] <- NA_real_
}
raw$overall_valence <- val_numeric
raw$overall_valence_uncertain <- val_ticks > 1

# -- Q6: awareness (0, 0.5, 1) ---------------------------------------
aw_mat <- as.matrix(raw[, c("q6_sploh_ne", "q6_vcasih", "q6_celotne")])
raw$awareness <- .row_score(aw_mat, weights = c(0, 0.5, 1))
aw_multi <- rowSums(aw_mat, na.rm = TRUE) > 1
if (any(aw_multi)) {
  warning(sum(aw_multi), " rows have multiple Q6 ticks; coerced to NA.")
  raw$awareness[aw_multi] <- NA_real_
}

# -- Italian placeholder detection -----------------------------------
# Any "ita" placeholder in any text cell flags the row.
raw$p1_text         <- .clean_text(raw$p1_text)
raw$p2_text         <- .clean_text(raw$p2_text)
raw$p3_text         <- .clean_text(raw$p3_text)
raw$overall_comment <- .clean_text(raw$overall_comment)

raw$needs_translation <- .is_italian_marker(raw$p1_text) |
  .is_italian_marker(raw$p2_text) |
  .is_italian_marker(raw$p3_text)

# Where 'ita' appears in a text cell, replace with NA so it doesn't pollute
# word frequencies.
raw$p1_text[.is_italian_marker(raw$p1_text)] <- NA_character_
raw$p2_text[.is_italian_marker(raw$p2_text)] <- NA_character_
raw$p3_text[.is_italian_marker(raw$p3_text)] <- NA_character_

# -- Build person-session data frame ---------------------------------
person_session <- data.frame(
  session       = raw$session,
  row_id        = as.integer(raw$row_id),
  freq          = raw$freq,
  type_piskanje  = raw$type_piskanje,
  type_sumenje   = raw$type_sumenje,
  type_brencanje = raw$type_brencanje,
  type_bobnenje  = raw$type_bobnenje,
  type_has_other = raw$type_has_other,
  type_drugo_text= raw$type_drugo_text,
  n_types        = raw$n_types,
  primary_type   = raw$primary_type,
  severity       = raw$severity,
  severity_numeric  = raw$severity_numeric,
  severity_uncertain= raw$severity_uncertain,
  masker_exp     = raw$masker_exp,
  overall_valence= raw$overall_valence,
  overall_valence_uncertain = raw$overall_valence_uncertain,
  awareness      = raw$awareness,
  p1_valence     = as.numeric(raw$p1_valence),
  p1_impact      = as.numeric(raw$p1_impact),
  p2_valence     = as.numeric(raw$p2_valence),
  p2_impact      = as.numeric(raw$p2_impact),
  p3_valence     = as.numeric(raw$p3_valence),
  p3_impact      = as.numeric(raw$p3_impact),
  p1_text        = raw$p1_text,
  p2_text        = raw$p2_text,
  p3_text        = raw$p3_text,
  overall_comment= raw$overall_comment,
  needs_translation = raw$needs_translation,
  stringsAsFactors = FALSE
)

# -- Build long part-level data frame --------------------------------
# One row per (session, row_id, part). Used for the heat maps and the
# correlation between valence and impact in §5.6.
part_level <- data.frame(
  session = rep(person_session$session, times = 3),
  row_id  = rep(person_session$row_id,  times = 3),
  part    = factor(rep(1:3, each = nrow(person_session)),
                   levels = 1:3, labels = c("1", "2", "3"), ordered = TRUE),
  valence = c(person_session$p1_valence,
              person_session$p2_valence,
              person_session$p3_valence),
  impact  = c(person_session$p1_impact,
              person_session$p2_impact,
              person_session$p3_impact),
  text    = c(person_session$p1_text,
              person_session$p2_text,
              person_session$p3_text),
  needs_translation = rep(person_session$needs_translation, times = 3),
  stringsAsFactors = FALSE
)

# Carry through subgroup attributes so we can subset without re-joining.
part_level$primary_type <- rep(person_session$primary_type, times = 3)
part_level$severity     <- rep(person_session$severity,     times = 3)
part_level$masker_exp   <- rep(person_session$masker_exp,   times = 3)
part_level$freq         <- rep(person_session$freq,         times = 3)
# awareness is numeric (0, 0.5, 1); turn into a factor with keys matching
# SLO_LABELS$awareness so plot_subgroup_box can look up Slovenian labels.
part_level$awareness_cat <- factor(
  as.character(rep(person_session$awareness, times = 3)),
  levels = c("0", "0.5", "1"),
  ordered = TRUE
)

# -- Save -------------------------------------------------------------
dir.create(here::here("data", "derived"),
           recursive = TRUE, showWarnings = FALSE)

saveRDS(person_session,
        here::here("data", "derived", "person_session.rds"))
saveRDS(part_level,
        here::here("data", "derived", "part_level.rds"))

message("Saved person_session.rds (", nrow(person_session),
        " rows) and part_level.rds (", nrow(part_level), " rows).")

# -- Quick sanity printout when sourced interactively ----------------
if (interactive()) {
  cat("Session counts:\n"); print(table(person_session$session))
  cat("\nFrequency:\n");    print(table(person_session$freq, useNA = "ifany"))
  cat("\nSeverity:\n");     print(table(person_session$severity, useNA = "ifany"))
  cat("\nMasker exp:\n");   print(table(person_session$masker_exp, useNA = "ifany"))
  cat("\nPrimary type:\n"); print(table(person_session$primary_type, useNA = "ifany"))
  cat("\nNeeds translation:", sum(person_session$needs_translation), "rows\n")
}

