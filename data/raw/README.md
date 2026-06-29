# Raw data — what's here and what isn't

This directory contains only the raw inputs that can be shared publicly.
Participant-level response data is held privately by Zavod TINITUS.

## Files included in the repository

- `composition_coding.csv` — Binary coding of which sonic elements
appear in each part of each composition, derived from the
composer's post-hoc descriptions. Five columns:
  `session`, `part`, `element_type`, `element_id`, `element_label_sl`.
  Encoding: Windows-1250, `;` separator (Excel-on-Slovenian-Windows
  convention).
- `composition_descriptions.txt` — Full prose descriptions of each
  composition part, authored by Boštjan Simon. Source for the coding
  scheme in `composition_coding.csv`.

## Files NOT included

- `tinitus_2026.csv` — Questionnaire responses (n = 118), including
  free-text descriptions of participants' tinnitus experience.
- `signup_estimate.csv` — Sign-up sheet transcriptions with
participant names.

These files contain personal data of human research participants who
consented to anonymous processing of their responses but **not to
public release**. They are held by Zavod TINITUS.

## Access for replication or reuse

Researchers who wish to reproduce or extend this analysis can request
access to the raw data from Zavod TINITUS. Access is subject to:

1. A signed data-use agreement,
2. Ethical review by the requesting institution where applicable,
3. A commitment not to redistribute the data.

Contact: <sabrina.lever@asociacija.si>

## Reproducing the report without raw data

The report's headline figures, the per-element analysis, and the
session-level heatmaps can all be reproduced from the aggregated
outputs in `data/derived/` (when present), without access to the
raw response file. The pipeline scripts in `R/` show exactly what
is derived from what.
