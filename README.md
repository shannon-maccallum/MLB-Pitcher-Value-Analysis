# MLB Pitcher Value Analysis

This project analyzes whether four-seam fastball traits help explain pitcher value in Major League Baseball from 2021 through 2025. The analysis compares active spin percentage, velocity, horizontal movement, and vertical movement against average Wins Above Replacement (WAR).

## Research Question

How do a pitcher's four-seam fastball active spin percentage, velocity, and movement relate to on-field value as measured by WAR?

## Key Findings

- The final model included 92 pitchers with matched Statcast and WAR data.
- Individual correlations between pitch traits and WAR were weak.
- The multiple regression model was not statistically significant overall.
- Fastball traits alone explained about 4.2% of the variation in average WAR.
- The results suggest that pitcher value depends on broader factors such as command, pitch mix, durability, role, sequencing, and run prevention context.

## Repository Structure

```text
.
├── analysis.R
├── data/
│   └── raw/
├── outputs/
│   ├── figures/
│   └── tables/
├── reports/
│   ├── pitcher-value-report.docx
│   └── pitcher-value-report.pdf
└── README.md
```

## Data

The `data/raw/` folder contains the source data used for the analysis:

- `WAR2021.csv` through `WAR2025.csv`
- `active-spin21.csv` through `active-spin25.csv`
- `pitch_movement21.csv` through `pitch_movement25.csv`
- Supplemental Acuna sprint speed files retained from the broader class project workspace

## Methods

The analysis:

1. Loads Statcast active spin and pitch movement data from 2021 through 2025.
2. Filters to pitchers with complete five-season records.
3. Calculates five-year averages for active spin percentage, velocity, horizontal movement, vertical movement, and WAR.
4. Creates high/low pitcher profiles based on median pitch traits.
5. Runs correlation tests and a multiple linear regression model.
6. Exports cleaned tables and figures to `outputs/`.

## Reproduce the Analysis

Install the required R packages:

```r
install.packages(c("dplyr", "ggplot2", "tidyr"))
```

Run the analysis from the repository root:

```bash
Rscript analysis.R
```

## Report

The polished work-sample report is available in `reports/pitcher-value-report.pdf`.
