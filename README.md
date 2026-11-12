# Nutrititional_Intelligence_Finland
Statistics analyses for the paper “Nutritional intelligence? Hyper-palatable foods do not dominate evaluations of familiar snacks in a Finnish sample”. ​ This repository contains R code and scripts to compute correlations, t-tests, linear mixed models, and ANOVAs used in the manuscript’s analyses.
The following scripts are uploaded:

- `01_Ttests and correlations.R` — HPF vs non-HPF t-tests and Spearman/Bayesian correlations for actual vs estimated energy density (Figure 1)
- `02_LMMs.R` — linear mixed models of ratings (EED, expected satiation, liking, frequency, healthiness) on palatability and category
- `03_Table 2 Analysis.R` — Welch t-tests and Welch-style Cohen's d for HPF vs non-HPF (Table 2)

The dataset used for analysis in csv long and wide format (with average values per food item) is provided under the names:
- 0_Complete_pictureset_data_long.csv
- 0_Complete_pictureset_data.csv



## How to run