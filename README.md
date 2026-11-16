# Nutrititional_Intelligence_Finland

[![Paper DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.cdnut.2026.109553-blue)](https://doi.org/10.1016/j.cdnut.2026.109553)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

R analyses for the paper “Nutritional intelligence? Hyper-palatable foods do
not dominate evaluations of familiar snacks in a Finnish sample”.

Published paper: [https://doi.org/10.1016/j.cdnut.2026.109553](https://doi.org/10.1016/j.cdnut.2026.109553)

## Repository contents

Analysis scripts:

- `01_Ttests_and_correlations.R` — HPF versus non-HPF t-tests, Spearman
  correlations, plots, and Bayesian analyses for energy density (Figure 1).
- `02_LMMs.R` — linear mixed-effects models for estimated energy density,
  expected satiation, liking, frequency of consumption, and healthiness.
  Participant-specific random intercepts account for repeated ratings.
- `03_Table_2_Analysis.R` — Shapiro-Wilk and Levene assumption checks, Welch
  t-tests, and Welch-style Cohen's d estimates for Table 2.

Data files:

- `data/0_Complete_pictureset_data.csv` — wide-format data.
- `data/0_Complete_pictureset_data_long.csv` — long-format data for the
  mixed-effects models.

## How to run

1. Install R 4.1 or newer.
2. Open the repository as the working project, or start R from the repository
   root.
3. Run any script on Mac or LINUX, for example:

   ```r
   source("01_Ttests_and_correlations.R")
   ```

**Microsoft Windows** users can open a script in RStudio or VS Code and run it block by block.
The numbered sections and `{}` blocks support selective execution and make it
possible to inspect each analysis stage separately.

The scripts use `here()` and repository-relative paths, so machine-specific
`setwd()` paths are not required. Each script checks for its required packages
and installs missing packages from CRAN before loading them. The mixed-model
script also runs visual diagnostics with `performance::check_model()` and
reports model fixed and random effects with 95% confidence intervals.

The scripts contain documented output sections. These outputs were captured
from the repository data and should be regenerated if the data, R version, or
package versions change.

To analyse your own data, replace the relevant file names in the `here("data",
"...")` import commands with your own CSV file names. Your data must contain
the columns required by the corresponding script and use compatible category
and palatability labels.
