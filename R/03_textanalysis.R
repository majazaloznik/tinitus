# ============================================================
# 03_textanalysis.R
# Slovenian lemmatisation via udpipe, per-part word frequencies,
# co-occurrence with numeric scores.
# Italian-flagged rows excluded automatically.
# ============================================================

source(here::here("R", "00_setup.R"))

pl <- readRDS(here::here("data", "derived", "part_level.rds"))

# -- Download model on first run -------------------------------------
.model_dir <- here::here("data", "udpipe_models")
if (!dir.exists(.model_dir)) dir.create(.model_dir, recursive = TRUE)

.model_file <- list.files(.model_dir, pattern = "slovenian.*\\.udpipe$",
                          full.names = TRUE)
if (length(.model_file) == 0) {
  message("Downloading Slovenian udpipe model (one-time, ~50 MB)...")
  m <- udpipe::udpipe_download_model(
    language = "slovenian-ssj",
    model_dir = .model_dir
  )
  .model_file <- m$file_model
} else {
  .model_file <- .model_file[1]
}
.model <- udpipe::udpipe_load_model(.model_file)

# -- Subset: drop Italian rows and missing text ----------------------
text_df <- pl[!pl$needs_translation & !is.na(pl$text), ]
text_df$doc_id <- paste(text_df$session, text_df$row_id,
                        text_df$part, sep = "_")

message("Analysing ", nrow(text_df), " text snippets ",
        "(excluded ", sum(pl$needs_translation), " Italian rows ",
        "and ", sum(is.na(pl$text)) - sum(pl$needs_translation & is.na(pl$text)),
        " empty cells).")

# -- Lemmatise --------------------------------------------------------
anno <- udpipe::udpipe_annotate(
  .model,
  x      = text_df$text,
  doc_id = text_df$doc_id
) |>
  as.data.frame()

# Join back the part-level metadata we'll need for co-occurrence analysis.
meta <- text_df[, c("doc_id", "session", "part", "valence", "impact",
                    "primary_type", "severity", "masker_exp")]
anno <- merge(anno, meta, by = "doc_id", all.x = TRUE)

# -- Filter to content words -----------------------------------------
# Keep nouns, verbs, adjectives, adverbs. Drop stopwords-style very
# common words; drop punctuation.
content_upos <- c("NOUN", "VERB", "ADJ", "ADV", "PROPN")
anno_content <- anno[anno$upos %in% content_upos &
                       nchar(anno$lemma) > 2, ]

# A small manual extra-stopword list — words that survive POS filtering
# but carry little content in this corpus (judged after a first pass).
.extra_stopwords <- c(
  "biti", "imeti", "moč", "lahko", "kar", "ker", "tudi",
  "samo", "še", "že", "sploh", "del", "drugi", "prvi", "tretji"
)
anno_content <- anno_content[!tolower(anno_content$lemma) %in% .extra_stopwords, ]
anno_content$lemma <- tolower(anno_content$lemma)

# UDPipe SSJ artifact correction: the lemmatiser sometimes produces a
# nonsense verb-shaped lemma for the adjective sproščujoč/-e. Map it back.
.lemma_fix <- c(
  "sproščujoteti" = "sproščujoč"
)
.matches <- anno_content$lemma %in% names(.lemma_fix)
anno_content$lemma[.matches] <- unname(.lemma_fix[anno_content$lemma[.matches]])

# -- Frequency tables ------------------------------------------------
freq_overall <- as.data.frame(
  sort(table(anno_content$lemma), decreasing = TRUE),
  stringsAsFactors = FALSE
)
names(freq_overall) <- c("lemma", "n")

freq_by_part <- as.data.frame(
  table(anno_content$lemma, anno_content$part),
  stringsAsFactors = FALSE
)
names(freq_by_part) <- c("lemma", "part", "n")
freq_by_part <- freq_by_part[freq_by_part$n > 0, ]

# -- Co-occurrence with numeric scores -------------------------------
# For each lemma occurring >= MIN_N times, compute mean valence and
# mean impact among the documents containing it.
MIN_N <- 5L

# Unique (doc_id, lemma) — we don't want a lemma counted multiple times
# per doc when computing doc-level means.
doc_lemma <- unique(anno_content[, c("doc_id", "lemma")])

# Doc -> scores lookup
doc_scores <- text_df[, c("doc_id", "valence", "impact")]
doc_lemma <- merge(doc_lemma, doc_scores, by = "doc_id")

agg_lemma <- stats::aggregate(
  doc_lemma[, c("valence", "impact")],
  by = list(lemma = doc_lemma$lemma),
  FUN = function(x) mean(x, na.rm = TRUE)
)
n_per_lemma <- as.data.frame(table(doc_lemma$lemma), stringsAsFactors = FALSE)
names(n_per_lemma) <- c("lemma", "n_docs")
lemma_scores <- merge(agg_lemma, n_per_lemma, by = "lemma")
lemma_scores <- lemma_scores[lemma_scores$n_docs >= MIN_N, ]
lemma_scores <- lemma_scores[order(-lemma_scores$n_docs), ]

# -- Save -------------------------------------------------------------
saveRDS(freq_overall,
        here::here("data", "derived", "freq_overall.rds"))
saveRDS(freq_by_part,
        here::here("data", "derived", "freq_by_part.rds"))
saveRDS(lemma_scores,
        here::here("data", "derived", "lemma_scores.rds"))
saveRDS(anno_content,
        here::here("data", "derived", "anno_content.rds"))

message("Saved freq_overall (", nrow(freq_overall), " unique lemmas), ",
        "lemma_scores (", nrow(lemma_scores), " lemmas with n>=", MIN_N, ").")

if (interactive()) {
  cat("\nTop 30 lemmas:\n")
  print(utils::head(freq_overall, 30))
  cat("\nLemmas with most positive impact (relief):\n")
  print(utils::head(
    lemma_scores[order(-lemma_scores$impact), ], 15
  ))
  cat("\nLemmas with most negative impact (aggravation):\n")
  print(utils::head(
    lemma_scores[order(lemma_scores$impact), ], 15
  ))
}
