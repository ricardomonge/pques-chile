# Cross-cultural Adaptation and Psychometric Validation of the Perceived Quality of University Experiences Scale (PQUES) among Business Students in Chile

Supplementary material and analysis code.

**Authors:** C. Frex; R. Monge; R. Fuentes; H. Fernández

**Contact:** rmonge@udla.cl

---

## Overview

This repository contains the code and data required to reproduce the analyses
reported in the manuscript. Two complementary analyses are provided:

- **Psychometric validation** (R): split-sample exploratory and confirmatory
  factor analysis (EFA/CFA), reliability, convergent and discriminant validity,
  exploratory graph analysis (EGA), and measurement invariance.
- **Content validity** (Python / Google Colab): Aiken's V coefficient and its
  95% confidence interval from expert-judge ratings.

## Repository structure

```
.
├── README.md
├── PQUES_psychometric_validation.R       # EFA/CFA, reliability, validity, EGA, invariance
├── PQUES_content_validity_AikenV.ipynb   # content validity (Aiken's V)
└── data/
    ├── data.csv                          # student survey dataset (PQUES)
    ├── demo_judges.csv                   # expert-judge profile
    └── data_Aiken.csv                # expert ratings (relevance / wording)
```

## Data

All datasets are placed in the `data/` folder.

**`data/data.csv`** — student responses used for the psychometric validation:
- Sociodemographic variables: `sex`, `edad`, `universidad`, `carrera`,
  `anio_ingreso`, `modalidad`, `trabaja`, `donde_trabaja`, `financiamiento`.
- Instrument-evaluation items: `aceptacion`, `comprension`, `satisfaccion`.
- 34 ordinal PQUES items organized into five subscales — `IS` (6 items),
  `CC` (7), `PA` (8), `CA` (8), and `DC` (5).

**`data/demo_judges.csv`** — sociodemographic profile of the expert judges:
`sexo`, `grado_academico`, `especializacion`, `experiencia_profesional`,
`anios_experiencia`, `exp_valid_inst`.

**`data/data_Aiken_NPS.csv`** — expert ratings in wide format: a `dimension`
column (`relevance` / `wording`) plus one column per item; cell values are
expert ratings on a 4-point scale.

## Requirements

**R (>= 4.3).** Install the required packages once:

```r
install.packages(c(
  "dplyr", "tidyverse", "readr", "purrr", "tibble",
  "gtsummary", "huxtable", "labelled", "matrixStats",
  "ggplot2", "gridExtra", "corrplot", "RColorBrewer", "likert", "scales",
  "psych", "lavaan", "semTools", "semPlot",
  "parameters", "performance", "nFactors", "FactoMineR", "factoextra",
  "QuantPsyc", "nortest", "MVN", "EGAnet", "see", "knitr"
))
```

**Python (Google Colab).** Packages: `pandas`, `numpy`, `scipy`, `matplotlib`,
`seaborn`, `tabulate` (all preinstalled in Colab).

## How to reproduce

1. Clone the repository and keep the data files inside `data/`.
2. **Psychometric validation (R):** open `PQUES_psychometric_validation.R` with
   the repository root as the working directory (so that `data/data.csv`
   resolves), then run the script. A fixed seed makes the EFA/CFA split
   reproducible; package versions are reported by `sessionInfo()` at the end.
3. **Content validity (Aiken's V):** open
   `PQUES_content_validity_AikenV.ipynb` in
   [Google Colab](https://colab.research.google.com/) and run the cells in
   order. The notebook reads the judge datasets from `data/`.

## Outputs

- R script: summary tables (participant characteristics, comparability,
  reliability, CR/AVE, Fornell–Larcker, HTMT, invariance) printed to the console.
- Aiken's V notebook: a results table per item with V and its 95% CI, and a
  forest-plot figure exported as `V_Aiken_final.pdf` and `V_Aiken_final.png`.

## License

Released under the [MIT License](https://opensource.org/licenses/MIT).

## Citation

Frex, C., Monge, R., Fuentes, R., & Fernández, H. (2026). *Cross-cultural
adaptation and psychometric validation of the Perceived Quality of University
Experiences Scale among business students in Chile.* [Journal / DOI]
