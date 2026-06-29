# ============================================================
# 04_signup_check.R
# Estimate the number of unique attendees across sessions from
# the OCR'd sign-up sheets. The estimate is rough — handwriting
# made some name spellings uncertain. Used in §4 limitations.
# ============================================================

source(here::here("R", "00_setup.R"))

signups <- utils::read.csv(
  here::here("data", "raw", "signup_estimate.csv"),
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# Normalise names: lowercase, strip diacritics, collapse whitespace.
# This catches "Marjan Miška" vs "MIŠKA MARJAN" only after we also sort
# the tokens, so do that too.
.normalise_name <- function(x) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  # Token-sort so "Marjan Miska" and "Miska Marjan" collide
  vapply(strsplit(x, " "), function(toks) {
    paste(sort(toks), collapse = " ")
  }, character(1))
}

signups$key <- .normalise_name(signups$name_normalised)

# Per-attendee session count
attendee_n <- as.data.frame(
  table(signups$key),
  stringsAsFactors = FALSE
)
names(attendee_n) <- c("key", "n_sessions")
attendee_n <- attendee_n[order(-attendee_n$n_sessions), ]

# Per-session totals
session_totals <- as.data.frame(
  table(signups$session),
  stringsAsFactors = FALSE
)
names(session_totals) <- c("session", "n_signed")

# Summary numbers used in the report
SIGNUP_SUMMARY <- list(
  n_unique_attendees   = nrow(attendee_n),
  n_total_signups      = nrow(signups),
  mean_sessions_per_p  = nrow(signups) / nrow(attendee_n),
  max_sessions_per_p   = max(attendee_n$n_sessions),
  n_attended_once      = sum(attendee_n$n_sessions == 1),
  n_attended_2_3       = sum(attendee_n$n_sessions %in% 2:3),
  n_attended_4plus     = sum(attendee_n$n_sessions >= 4),
  attendee_n           = attendee_n,
  session_totals       = session_totals
)

saveRDS(SIGNUP_SUMMARY,
        here::here("data", "derived", "signup_summary.rds"))

if (interactive()) {
  cat("Unique attendees (estimate):", SIGNUP_SUMMARY$n_unique_attendees, "\n")
  cat("Total signups (sum across sheets):",
      SIGNUP_SUMMARY$n_total_signups, "\n")
  cat("Mean sessions per attendee:",
      round(SIGNUP_SUMMARY$mean_sessions_per_p, 2), "\n")
  cat("Attended >=4 sessions:",
      SIGNUP_SUMMARY$n_attended_4plus, "\n")
  cat("\nPer-session sign-up totals:\n")
  print(SIGNUP_SUMMARY$session_totals)
}
