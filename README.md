# OECD Well-being & Life Satisfaction Analysis

An R project exploring the relationship between **life satisfaction and multidimensional well-being** across countries in an OECD dataset. It prepares country-level indicators, constructs standardized dimension scores, visualizes associations, and compares Italy with the other countries in the analytical sample.

[Analysis script](dslab_oecd_wellbeing_reproducibility.R) · [Project report](DSLab_Project.pdf)

## Analytical approach

The script uses the **latest available observation for each country and measure within 2010–2025**. It:

1. Detects relevant columns and filters population totals where available.
2. Selects well-being measures and reshapes the data into country-level tables.
3. Reports missing values and descriptive statistics.
4. Calculates correlations with life satisfaction.
5. Standardizes indicators, reverses adverse measures, and averages them into dimension scores.
6. Compares Italy with the sample average excluding Italy.
7. Fits an exploratory linear regression using the three dimensions with the strongest absolute correlations, when enough complete observations are available.

Dimensions cover economic security, work quality, housing, health, social connections, governance, the environment, and safety.

## Run the analysis

Requirements: **R**, an internet connection for first-time package installation, and the supplied CSV.

~~~bash
git clone https://github.com/TonyDawra/oecd-wellbeing-analysis.git
cd oecd-wellbeing-analysis
Rscript dslab_oecd_wellbeing_reproducibility.R
~~~

Alternatively, open the script in RStudio and run it from the repository directory.

The script installs missing packages from this list: `tidyverse`, `janitor`, `skimr`, and `broom`. It expects `OECD_WELLBEING_DATA.csv` in the working directory and creates an `outputs/` folder.

## Outputs

| Output | Contents |
| --- | --- |
| Cleaned CSVs | Latest observations in long and wide formats |
| Quality summaries | Missing-value counts and descriptive statistics |
| Dimension tables | Standardized country scores and correlations |
| Regression CSVs | Analytical sample, coefficients, and model summary |
| PNG charts | Indicator correlations, dimension correlations, scatterplots, missingness, and Italy comparisons |

An existing `outputs.zip` is included for inspecting previously generated results.

## Repository contents

~~~text
├── dslab_oecd_wellbeing_reproducibility.R
├── OECD_WELLBEING_DATA.csv
├── DSLab_Project.pdf
├── outputs.zip
└── README.md
~~~

## Interpretation and limitations

The latest observation can come from different years for different countries or indicators, so this is not a synchronized annual panel. Missing indicators also affect which countries and measures enter each comparison.

Correlations and the exploratory regression describe associations. Dimension selection uses the same sample as the regression, so the model should not be presented as independently validated prediction or evidence of causation.

## Credits

Maintained by [Tony Dawra](https://github.com/TonyDawra). See the included report for project authorship and methodological details.
