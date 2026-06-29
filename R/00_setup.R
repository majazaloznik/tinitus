# ============================================================
# 00_setup.R
# Libraries, helpers, Slovenian labels.
# Sourced at the top of every other script and the Rmd.
# ============================================================

# -- Working directory / paths -----------------------------------------
# Use here::here() so the same code runs in script and in knitted Rmd.
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

# -- Package check (install if missing, do not load globally) ---------
# All downstream code uses pkg::fn() style explicitly. No library() calls.
.required_pkgs <- c(
  "here",          # paths
  "udpipe",        # Slovenian lemmatisation
  "ggalluvial",    # static alluvial / Sankey diagrams (depends on ggplot2)
  "knitr",
  "rmarkdown"
)
.missing <- setdiff(.required_pkgs, rownames(utils::installed.packages()))
if (length(.missing)) {
  message("Installing missing packages: ", paste(.missing, collapse = ", "))
  utils::install.packages(.missing)
}

# -- Slovenian labels for factors --------------------------------------
# Keep all human-readable Slovenian strings in one place so the report
# stays consistent and we can swap them later if needed.

SLO_LABELS <- list(
  freq = c(stalno = "Stalno", obcasno = "Občasno"),

  severity = c(
    sploh_ne  = "Sploh me ne moti",
    malo      = "Malo moteč",
    zmerno    = "Zmerno moteč",
    zelo      = "Zelo moteč",
    ekstremno = "Ekstremno moteč"
  ),

  masker_exp = c(
    nikoli  = "Nikoli",
    enkrat  = "Da, enkrat",
    veckrat = "Da, večkrat",
    redno   = "Redno"
  ),

  overall_valence = c(
    "-2" = "Zelo neprijetno",
    "-1" = "Neprijetno",
    "0"  = "Nevtralno",
    "1"  = "Prijetno",
    "2"  = "Zelo prijetno"
  ),

  awareness = c(
    "0"   = "Sploh ne",
    "0.5" = "Včasih",
    "1"   = "Tekom celotne kopeli"
  ),

  type = c(
    piskanje  = "Piskanje",
    sumenje   = "Šumenje",
    brencanje = "Brenčanje",
    bobnenje  = "Bobnenje",
    drugo     = "Drugo"
  ),

  part = c("1" = "Prvi del", "2" = "Drugi del", "3" = "Tretji del"),

  session = paste0(c("I","II","III","IV","V","VI","VII","VIII"), ". seja")
)

# -- Helper: row-wise "which one of these binaries is 1" ---------------
# Vectorised. Returns NA when none are 1.
.row_which_one <- function(mat) {
  mat[is.na(mat)] <- 0
  out <- max.col(mat, ties.method = "first")
  out[rowSums(mat) == 0] <- NA_integer_
  out
}

# -- Helper: convert binary set to weighted ordinal -------------------
# (e.g. severity from 5 binary columns -> 1..5)
.row_score <- function(mat, weights) {
  mat[is.na(mat)] <- 0
  s <- as.numeric(mat %*% weights)
  s[rowSums(mat) == 0] <- NA_real_
  s
}

# -- Helper: clean NA-ish text ----------------------------------------
.clean_text <- function(x) {
  x <- trimws(x)
  x[x %in% c("", "NA", "na", "-")] <- NA_character_
  x
}

# -- Helper: detect Italian placeholder ("ita") -----------------------
.is_italian_marker <- function(x) {
  !is.na(x) & tolower(trimws(x)) == "ita"
}

message("Setup complete. Helpers loaded; no packages attached.")
