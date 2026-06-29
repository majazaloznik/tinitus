# Zvočni objem — analiza in poročilo

*Reproducible analysis pipeline for the "Zvočni objem" project (Zavod TINITUS, 2026).*

Repozitorij vsebuje celotno analitično pot in poročilo pilotne študije
*Zvočni objem* (Zavod TINITUS, februar–april 2026, Nova Gorica), v kateri
smo z osmimi umetniškimi zvočnimi kopelmi avtorja Boštjana Simona
opazovali neposredne perceptualne učinke umetniško komponiranega zvoka
pri osebah s tinitusom (118 izpolnjenih vprašalnikov, ocenjenih ~64
različnih udeležencev). Vključene so R skripte za čiščenje in analizo
podatkov, kodirana zvočna shema kompozicij, opisi avtorja in končno
poročilo. Surovi podatki udeležencev v repozitoriju **niso** objavljeni
(glej `data/raw/README.md`); namen objave je transparentnost
analitičnega postopka in možnost ponovne uporabe metode, ne pa
posplošljiv dokaz učinkovitosti intervencije.


## Project structure

```
zvocni_objem/
├── README.md
├── data/
│   ├── raw/
│   │   ├── composition_coding.csv       Binary coding of musical/masking elements per part
│   │   ├── composition_descriptions.txt Boštjan's prose descriptions of all 24 parts
│   │   ├── tinitus_2026.csv             (not in repo — see data/raw/README.md)
│   │   └── signup_estimate.csv          (not in repo — see data/raw/README.md)
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

> **Note**: Steps 3–6 require the raw response data, which is not in
> this repository. See `data/raw/README.md` for access. Without it,
> you can still read the report and inspect the analysis code.

1. Open R in the project root (or set working directory there).
2. `source("R/00_setup.R")` — installs missing packages, loads helpers.
3. `source("R/01_clean_data.R")` — creates `data/derived/person_session.rds` and `data/derived/part_level.rds`.
4. `source("R/04_signup_check.R")` — creates `signup_summary.rds`.
5. `source("R/05_composition.R")` — creates `element_summary.rds`, `composition_matrix.rds`, `concordance.rds`, `grand_means.rds`.
6. `source("R/03_textanalysis.R")` — downloads the Slovenian udpipe model on first run (~50 MB), saves token frequencies. Optional until you have Italian translations.
7. Knit `report/porocilo.Rmd` to Word (.docx).

## Dependencies

R ≥ 4.5, packages: `here`, `udpipe`, `ggalluvial` (+ `ggplot2` as dependency),
`knitr`, `rmarkdown`. Pandoc (bundled with RStudio). No LaTeX required —
output is Microsoft Word (.docx).

## Licence

- **Code** (R scripts, Rmd source): [MIT](LICENSE)
- **Data and report** (CSV files, descriptions, rendered report,
  derived outputs): [CC BY 4.0](LICENSE-data.md)

Raw participant-level response data is not included in this
repository — see `data/raw/README.md`.

## Notes

- Code and comments are in English; report prose is in Slovenian.
- The sign-up estimate is a best-effort OCR transcription and contains uncertainty in spelling.
