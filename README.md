# Zvočni objem — analiza in poročilo

Reproducible analysis pipeline for the "Zvočni objem" project (Zavod TINITUS, 2026).

## Project structure

```
zvocni_objem/
├── README.md
├── data/
│   ├── raw/
│   │   ├── tinitus_2026.csv             Raw response data (per-session questionnaires, coded)
│   │   ├── signup_estimate.csv          OCR of sign-up sheets (for attendance estimation)
│   │   ├── composition_coding.csv       Binary coding of musical/masking elements per part (REVIEW)
│   │   └── composition_descriptions.txt Bostjan's prose descriptions of all 24 parts
│   └── derived/                         Outputs of cleaning + analysis scripts (generated)
├── R/
│   ├── 00_setup.R                       Libraries, helper functions, Slovenian labels
│   ├── 01_clean_data.R                  Raw CSV -> tidy data frames (wide + long)
│   ├── 02_plots.R                       Plotting functions (base graphics)
│   ├── 03_textanalysis.R                udpipe Slovenian lemmatization + frequency tables
│   ├── 04_signup_check.R                Unique-attendee count from sign-up sheets
│   └── 05_composition.R                 Element-level analysis + qualitative concordance
├── report/
│   └── porocilo.Rmd                     Main report (Slovenian, PDF output)
└── output/
    └── figures/                         Auto-saved figures (generated)
```

## How to run

1. Open R in the project root (or set working directory there).
2. `source("R/00_setup.R")` — installs missing packages, loads helpers.
3. `source("R/01_clean_data.R")` — creates `data/derived/person_session.rds` and `data/derived/part_level.rds`.
4. `source("R/04_signup_check.R")` — creates `signup_summary.rds`.
5. `source("R/05_composition.R")` — creates `element_summary.rds`, `composition_matrix.rds`, `concordance.rds`, `grand_means.rds`.
6. `source("R/03_textanalysis.R")` — downloads the Slovenian udpipe model on first run (~50 MB), saves token frequencies. Optional until you have Italian translations.
7. Knit `report/porocilo.Rmd` to PDF.

## Dependencies

R ≥ 4.5, packages: `here`, `udpipe`, `ggalluvial` (+ `ggplot2` as dependency),
`knitr`, `rmarkdown`. Pandoc (bundled with RStudio). No LaTeX required —
output is Microsoft Word (.docx).

A `report/reference.docx` file controls fonts, margins, heading styles, etc.
If you don't have one yet, create it once with:

```r
# Knit once without the reference (comment out reference_docx in YAML),
# then save the resulting porocilo.docx as report/reference.docx and
# style it in Word (Home -> Styles). Re-enable reference_docx and re-knit.
```

## Notes

- Code and comments are in English; report prose is in Slovenian.
- The sign-up estimate is a best-effort OCR transcription and contains uncertainty in spelling.
