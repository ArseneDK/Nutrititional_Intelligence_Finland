# ==============================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) and reviewed by Manon Chédeville (manon.chedeville@helsinki.fi)
# Purpose: Check assumptions, run frequentist and Bayesian tests, and visualise correlations.
# Script: Energy density vs estimated energy density (HPF vs non‑HPF)t-test and correlations
#
# Created: 18.06.2025
# Last updated: 07.09.2026
#
# ==============================================================================
# Run this script in a fresh R session (RStudio: Session > Restart R).

# Divider convention:
#   #:::::  Major numbered sections, such as 1. SETUP or 2. ASSUMPTIONS.
#   #=====  Numbered subsections, such as 2.1 AED or 2.2 EED.
#   #-----  Output separators: printed results within a subsection.
# Plain comments describe individual commands or interpretation.

#:::::::::::::::::::::::::::::::::::: 1. SETUP :::::::::::::::::::::::::::::::::
#===== 1.1 PACKAGE SETUP =======================================================


# Check, install, and load the packages required by this script.
# Only packages missing from the current R library are installed.
# The CRAN mirror is specified to avoid an interactive selection prompt.
{
required_packages <- c(
  "tidyverse",
  "coda",
  "Matrix",
  "ggpubr",
  "BayesFactor",
  "knitr",
  "DFBA",
  "here",
  "gridExtra",
  "car",
  "effsize"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

invisible(
  lapply(required_packages, library, character.only = TRUE)
)
}

#===== 1.2 DATA IMPORT AND CLEANING ============================================
# Import and prepare the analysis data.
# The path is built from the repository root so the script is portable.
# Categorical variables are converted to factors for modelling.
{
  # Import picture‑set data
  # Drop first column (index column created when exporting from R/Excel)
  food_data <- read.csv(here("data", "0_Complete_pictureset_data.csv"))[, -1]

  # Convert categorical variables to factors for modelling
  # Set explicit factor order for energy levels 1&2 (used in plotting / modelling)
  food_data <- food_data |>
    mutate(
      Category       = factor(Category),
      Palatability   = factor(Palatability),
      NOVA_Group     = factor(NOVA_Group),
      NOVA_Group2    = factor(NOVA_Group2),
      Energy_levels  = factor(Energy_levels, levels = c("HED", "MED", "LED")),
      Energy_levels2 = factor(
        Energy_levels2,
        levels = c("VHED", "HED", "MED", "LED", "VLED")
      )
    )
}

#:::::::::::::::::::::::::::: 2. ASSUMPTION CHECKS :::::::::::::::::::::::::::::

# Shapiro-Wilk normality check with a plain-language verdict.
#   x     : numeric vector to test
#   label : text shown in the output (e.g. "AED (full sample)")
#   alpha : significance threshold (default .05)
check_normality <- function(x, label, alpha = 0.05) {
  sw <- shapiro.test(x)
  cat(sprintf("%s: W = %.4f, p = %.4g\n", label, sw$statistic, sw$p.value))
  if (sw$p.value > alpha) {
    cat("  -> no significant deviation from normality\n")
  } else {
    cat("  -> significant deviation from normality\n")
  }

  # returns the full test object silently, in case you want to store it
  invisible(sw)
}



###Assumptions check for full sample
##Actual Energy density (AED)

{
#==================== 2.1 NORMALITY OF AED =====================================
#
check_normality(food_data$Energy_density, "AED (full sample)")
#-------------------------------------------------------------------------------
#OUTPUT:
#AED (full sample): W = 0.9892, p = 0.802
#   -> no significant deviation from normality
#-------------------------------------------------------------------------------



#==================== 2.2 HOMOGENEITY OF AED ===================================
# Check homogeneity of variance for AED by palatability
leveneTest(Energy_density ~ Palatability, data = food_data)
#------------------------------OUTPUT-------------------------------------------
# 
# Levene's Test for Homogeneity of Variance (center = median)
#      Df F value  Pr(>F)
# group  1  2.8513 0.09575 .
#      70
#-------------------------------------------------------------------------------
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#-------------------------------------------------------------------------------



#==================== 2.3 T-TEST ON AED ========================================
# Run t-test on AED, assuming equal variances based on Levene's test.
t.test(Energy_density ~ Palatability, data = food_data, var.equal = TRUE)
#-----------------------------OUTPUT--------------------------------------------
#
#        Two Sample t-test
#
#data:  Energy_density by Palatability
#t = -0.033026, df = 70, p-value = 0.9737
#alternative hypothesis: true difference in means
#       between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
# -0.6224608  0.6021816
#sample estimates:
#    mean in group HPF mean in group Not HPF
#             3.561429              3.571568
#-------------------------------------------------------------------------------

#==================== 2.4 ESTIMATED ENERGY DENSITY =============================
#
check_normality(food_data$Estimated_ED, label = "EED (full sample)")
#-----------------------------OUTPUT--------------------------------------------
# 
# EED (full sample): W = 0.9092, p = 7.301e-05
#  -> significant deviation from normality
#-------------------------------------------------------------------------------


#==================== 2.5 NORMALITY OF EED =====================================
#
shapiro.test(food_data$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
#         Shapiro-Wilk normality test
#
# data:  food_data$Estimated_ED
# W = 0.90921, p-value = 7.301e-05
#  -> data is NOT normally distributed
#-------------------------------------------------------------------------------




#==================== 2.6 WELCH T-TEST FOR EED =================================
# Use Welch t‑test for EED (non‑normal distribution)
t.test(Estimated_ED ~ Palatability, data = food_data)
#-----------------------------OUTPUT--------------------------------------------
#
#        Welch Two Sample t-test
# data:  Estimated_ED by Palatability
# t = 1.6467, df = 64.866, p-value = 0.1045
# alternative hypothesis: true difference in means
#       between group HPF and group Not HPF is not equal to 0
# 95 percent confidence interval:
# -0.7746267  8.0519728
# sample estimates:
#     mean in group HPF mean in group Not HPF
#              63.95121              60.31254
#-------------------------------------------------------------------------------
}

#:::::::::::::::::::::::::::: 3. SUBGROUP ANALYSIS :::::::::::::::::::::::::::::
### Subsetting and correlation choices

{
# Split data by palatability category
filtered_data_HPF  <- food_data |> filter(Palatability == "HPF")
filtered_data_NHPF <- food_data |> filter(Palatability == "Not HPF")

#==================== 3.1 SUBGROUP NORMALITY CHECKS ============================
#
shapiro.test(filtered_data_HPF$Energy_density)
#-----------------------------OUTPUT--------------------------------------------
# 
#        Shapiro-Wilk normality test
#
# data:  filtered_data_HPF$Energy_density
# W = 0.93903, p-value = 0.1044
#  -> no significant deviation from normality
#-------------------------------------------------------------------------------


shapiro.test(filtered_data_NHPF$Energy_density)
#-----------------------------OUTPUT--------------------------------------------
# 
#        Shapiro-Wilk normality test
#
# data:  filtered_data_NHPF$Energy_density
# W = 0.94627, p-value = 0.04002
#  -> data is NOT normally distributed
#-------------------------------------------------------------------------------


shapiro.test(filtered_data_HPF$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
# 
#        Shapiro-Wilk normality test
#
# data:  filtered_data_HPF$Estimated_ED
# W =  0.8931, p-value = 0.007917
#  -> data is NOT normally distributed
#-------------------------------------------------------------------------------

shapiro.test(filtered_data_NHPF$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
# 
#        Shapiro-Wilk normality test
#
# data:  filtered_data_NHPF$Estimated_ED
# W = 0.91112, p-value = 0.002434
#  -> data is NOT normally distributed
#-------------------------------------------------------------------------------


# Because most variable/group combinations are non-normal, use Spearman
# correlations to assess monotonic associations without assuming normality.
}

#:::::::::::::::::::::::::::: 4. VISUALISATIONS ::::::::::::::::::::::::::::::::
##Plots
#Scatter plots AED versus EED

{
#==================== 4.1 HPF SCATTER PLOT =====================================
p1 <- ggscatter(filtered_data_HPF, x = "Energy_density", y = "Estimated_ED", 
                add = "reg.line",
                ggtheme = theme_minimal(),
                conf.int = TRUE, 
                cor.coef = TRUE,
                cor.method = "spearman",
                ggp = NULL,             # default
                show.legend.text = NA,  # default
                cor.coef.size = 5,
                cor.coef.coord = c(2, 35), # place correlation label in empty region
                color = "#ffc107ff",   #  HPF color
                palette = NULL,          #default
                shape = 19,
                size = 4.2,           # slightly larger to emphasize HPF
                xlab = "Energy density (kcal/gram)",
                ylab = "EED (% of scale)") +
  ggtitle(paste0("HPF (n = ", nrow(filtered_data_HPF), ")")) +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Increase title size
    axis.title.x = element_text(size = 15, margin = margin(t = 10, b = 10), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(l = 10, r = 10), color = "black"),
    axis.text.x = element_text(size = 12, face = "bold"), # Increase size of x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"), # Increase size of y-axis tick labels
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(linewidth = 1.5)
  )

# Display the HPF scatter plot.
p1
#...............................................................................
#==================== 4.2 NON-HPF SCATTER PLOT =================================
p2 <- ggscatter(filtered_data_NHPF, x = "Energy_density", y = "Estimated_ED", 
                add = "reg.line",
                ggtheme = theme_minimal(),
                conf.int = TRUE, 
                cor.coef = TRUE,
                cor.method = "spearman",
                ggp = NULL,
                show.legend.text = NA,
                cor.coef.size = 5,
                cor.coef.coord = c(2, 35),
                color = "#4a8ce1ff",   # blue for non‑HPF
                palette = NULL,
                shape = 19,
                size = 4,
                xlab = "Energy density (kcal/gram)",
                ylab = "EED (% of scale)") +
  ggtitle(paste0("Non-HPF (n = ", nrow(filtered_data_NHPF), ")")) +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Increase title size
    axis.title.x = element_text(size = 15, margin = margin(t = 10, b = 10), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(l = 10, r = 10), color = "black"),
    axis.text.x = element_text(size = 12, face = "bold"), # Increase size of x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"), # Increase size of y-axis tick labels
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(size = 1.5))

# Display the non-HPF scatter plot.
p2
#...........................................................................
#==================== 4.3 ARRANGE SCATTER PLOTS ================================
# Arrange the HPF and non-HPF plots vertically in two rows.
grid.arrange(p1, p2, nrow = 2)
# Arrange HPF vs non‑HPF clearly (e.g. side‑by‑side)
grid.arrange(p1, p2, nrow = 1)
}
#:::::::::::::::::::: 5. CORRELATIONS AND BAYESIAN ANALYSES ::::::::::::::::::::
#===== 5.1 SPEARMAN CORRELATIONS ===============================================

{

#==================== 5.1.1 FULL SAMPLE: EED VS ED =============================
cor.test(food_data$Estimated_ED, food_data$Energy_density,
         method = "spearman")
#-----------------------------OUTPUT--------------------------------------------
# Spearman's rank correlation rho
#
# data:  food_data$Estimated_ED and food_data$Energy_density
# S = 38622, p-value = 0.001026
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho
# 0.3790259
#-------------------------------------------------------------------------------


#==================== 5.1.2 HPF: EED VS ED =====================================
cor.test(filtered_data_HPF$Estimated_ED, filtered_data_HPF$Energy_density,
         method = "spearman")
#-----------------------------OUTPUT--------------------------------------------
# Spearman's rank correlation rho
#
# data:  filtered_data_HPF$Estimated_ED and filtered_data_HPF$Energy_density
# S = 1255.7, p-value = 0.0001489
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho
# 0.6563569
#-------------------------------------------------------------------------------


#==================== 5.1.3 NON-HPF: EED VS ED =================================
cor.test(filtered_data_NHPF$Estimated_ED, filtered_data_NHPF$Energy_density,
         method = "spearman")
#-----------------------------OUTPUT--------------------------------------------
# Spearman's rank correlation rho
#
# data:  filtered_data_NHPF$Estimated_ED and filtered_data_NHPF$Energy_density
# S = 12862, p-value = 0.5456
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.09360023
#-------------------------------------------------------------------------------

}

#===== 5.2 BAYESIAN T-TESTS ====================================================
# Compare actual and estimated energy density between HPF and non-HPF groups.

{
# Pass numeric vectors explicitly because the formula interface can return
# a list error with some BayesFactor and R version combinations.
#==================== 5.2.1 AED BY PALATABILITY ================================
# Bayesian t-test for actual energy density (AED).
ttestBF(
  x = food_data$Energy_density[food_data$Palatability == "HPF"],
  y = food_data$Energy_density[food_data$Palatability == "Not HPF"]
)
#-----------------------------OUTPUT--------------------------------------------
# Bayes factor analysis
# [1] Alt., r=0.707 : 0.2484097 ±0.01%
#
# Against denominator:
#   Null, mu1-mu2 = 0
#
# Bayes factor type: BFindepSample, JZS
#-------------------------------------------------------------------------------


#==================== 5.2.2 EED BY PALATABILITY ================================
# Bayesian t-test for estimated energy density (EED).
ttestBF(
  x = food_data$Estimated_ED[food_data$Palatability == "HPF"],
  y = food_data$Estimated_ED[food_data$Palatability == "Not HPF"]
)
#-----------------------------OUTPUT--------------------------------------------
# Bayes factor analysis
# [1] Alt., r=0.707 : 0.7169468 ±0.01%
#
# Against denominator:
#   Null, mu1-mu2 = 0
#
# Bayes factor type: BFindepSample, JZS
#-------------------------------------------------------------------------------
}
#...............................................................................

#===== 5.3 BAYESIAN CORRELATIONS ===============================================
# Estimate exploratory Bayes factors for the associations between AED and EED.
# correlationBF is a Pearson-type Bayes factor and complements the Spearman
# correlations reported in subsection 5.1.

{
#==================== 5.3.1 FULL SAMPLE: AED VS EED ============================
correlationBF(y = food_data$Energy_density,
              x = food_data$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
# Bayes factor analysis
# [1] Alt., r=0.333 : 32.28543 ±0%
#
# Against denominator:
#   Null, rho = 0
#
# Bayes factor type: BFcorrelation, Jeffreys-beta*
#-----------------------------------------------------------------------


#==================== 5.3.2 HPF: AED VS EED ===================================
correlationBF(y = filtered_data_HPF$Energy_density,
              x = filtered_data_HPF$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
# Bayes factor analysis
# [1] Alt., r=0.333 : 371.2954 ±0%
#
# Against denominator:
#   Null, rho = 0
#
# Bayes factor type: BFcorrelation, Jeffreys-beta*
#-----------------------------------------------------------------------


#==================== 5.3.3 NON-HPF: AED VS EED ================================
correlationBF(y = filtered_data_NHPF$Energy_density,
              x = filtered_data_NHPF$Estimated_ED)
#-----------------------------OUTPUT--------------------------------------------
# Bayes factor analysis
# [1] Alt., r=0.333 : 0.7666349 ±0%
#
# Against denominator:
#   Null, rho = 0
#
# Bayes factor type: BFcorrelation, Jeffreys-beta*
#-----------------------------------------------------------------------
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::: THE END ::::::::::::::::::::::::::::::::::