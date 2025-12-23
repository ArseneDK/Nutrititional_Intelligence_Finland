# ==============================================================================================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) and reviewed by Manon Chédeville (manon.chedeville@helsinki.fi)
# Purpose: Check assumptions, run frequentist and Bayesian tests, and visualise correlations.
# Script: Energy density vs estimated energy density (HPF vs non‑HPF)t-test and correlations
#
# created: 18.06.2025
# last updated: 07.10.2025 
#
# ============================================================================================================================================== 
#Clear environment
rm(list=ls())

###Package and setup section

# Install packages once (comment out in normal use)
# install.packages(c(
#   "tidyverse", "ggpubr", "BayesFactor", "dplyr", "knitr",
#   "effsize", "DFBA", "coda", "Matrix", "gridExtra", "car"
# ))

# Load libraries used in this script
{
library(tidyverse)   # data wrangling, piping, plotting
library(coda)        # MCMC diagnostics (used by BayesFactor / DFBA)
library(Matrix)      # matrix operations (dependency)
library(ggpubr)      # ggscatter and publication‑ready plots
library(BayesFactor) # Bayesian t‑tests and correlations
library(knitr)       # reporting
library(DFBA)        # additional Bayesian analyses
library(dplyr)       # data manipulation (mostly via tidyverse)
library(gridExtra)   # arrange multiple plots
library(car)         # leveneTest for homogeneity of variance
library(effsize)     # effect size calculations
}
# Set working directory (use the one appropriate for your system)
# Windows path (lab server)
setwd("P:/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis")
# Mac path (mounted volume)
# setwd("/Volumes/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Data import and cleaning

# Import picture‑set data
food_data <- read.csv("0_Complete_pictureset_data.csv")

# Drop first column (index column created when exporting from R/Excel)
food_data <- food_data[, -1]

# Convert categorical variables to factors for modelling
{
food_data$Category       <- factor(food_data$Category)
food_data$Palatability   <- factor(food_data$Palatability)
food_data$NOVA_Group     <- factor(food_data$NOVA_Group)
food_data$Energy_levels  <- factor(food_data$Energy_levels)
food_data$Energy_levels2 <- factor(food_data$Energy_levels2)
food_data$NOVA_Group2    <- factor(food_data$NOVA_Group2)
}
# Set explicit factor order for energy levels (used in plotting / modelling)
{
food_data$Energy_levels  <- factor(food_data$Energy_levels,
                                   levels = c("HED", "MED", "LED"))
food_data$Energy_levels2 <- factor(food_data$Energy_levels2,
                                   levels = c("VHED", "HED", "MED", "LED", "VLED"))
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###Assumptions check for full sample
##Actual Energy density (AED)

# Check normality of AED
shapiro.test(food_data$Energy_density)
#W = 0.98925, p-value = 0.802 data is normally distributed

# Check homogeneity of variance for AED by palatability
leveneTest(Energy_density ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#                Df F value  Pr(>F)  
#Energy density  1  2.8513 0.09575 . Equal Variance (trend)
#      70                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Run t‑test on AED (assume equal variances if Levene p > .05)
t.test(Energy_density ~ Palatability, data = food_data, var.equal = TRUE)
#t = -0.033026, df = 70, p-value = 0.9737
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -0.6224608  0.6021816
#sample estimates:
#  mean in group HPF mean in group Not HPF 
#3.561429              3.571568 

##Estimated enrgy density (EED)

# Check normality of EED
shapiro.test(food_data$Estimated_ED)
#W = 0.90921, p-value = 7.301e-05 data is NOT normally distributed

# Check homogeneity of variance for EED
leveneTest(Estimated_ED ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1  0.6512 0.4224 Equal variance
#      70 

# Use Welch t‑test for EED (non‑normal distribution)
t.test(Estimated_ED ~ Palatability, data = food_data)
#t = 1.6467, df = 64.866, p-value = 0.1045
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
# -0.7746267  8.0519728
#sample estimates:
#    mean in group HPF mean in group Not HPF 
#             63.95121              60.31254
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###Subsetting and correlation choices

# Split data by palatability category
filtered_data_HPF  <- food_data %>% filter(Palatability == "HPF")
filtered_data_NHPF <- food_data %>% filter(Palatability == "Not HPF")

# Normality checks for AED and EED in each subgroup
shapiro.test(filtered_data_HPF$Energy_density)
#W = 0.93903, p-value = 0.1044 data is normally distributed

shapiro.test(filtered_data_NHPF$Energy_density)
#W = 0.94627, p-value = 0.04002 data is NOT normally distributed
shapiro.test(filtered_data_HPF$Estimated_ED)
#W =  0.8931, p-value = 0.007917 data is NOT normally distributed

shapiro.test(filtered_data_NHPF$Estimated_ED)
#W = 0.91112, p-value = 0.002434 is NOT normally distributed

# Normality is violated in most combinations, so use Spearman correlations
# (monotonic relationship, robust to non‑normality).
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##Plots
#Scatter plots AED versus EED

#HPF correlation for AED vs EED
p1 <- ggscatter(filtered_data_HPF, x = "Energy_density", y = "Estimated_ED", 
                add = "reg.line",
                ggtheme = theme_minimal(),
                conf.int = TRUE, 
                cor.coef = TRUE,
                cor.method = "spearman",
                ggp = NULL,
                show.legend.text = NA,
                cor.coef.size = 5,
                cor.coef.coord = c(2, 35), # place correlation label in empty region
                color = "#ffc107ff",   #  HPF color
                palette = NULL,
                shape = 19,
                size = 4.2,           # slightly larger to emphasize HPF
                xlab = "Energy density (kcal/gram)",
                ylab = "EED (% of scale)") +
  ggtitle("HPF (28)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Increase title size
    axis.title.x = element_text(size = 15, margin = margin(t = 10, b = 10), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(l = 10, r = 10), color = "black"),
    axis.text.x = element_text(size = 12, face = "bold"), # Increase size of x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"), # Increase size of y-axis tick labels
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(size = 1.5)
  )

p1
#...............................................................................
#AED vs EED
#NHPF
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
  ggtitle("Non HPF (44)") +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Increase title size
    axis.title.x = element_text(size = 15, margin = margin(t = 10, b = 10), color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(l = 10, r = 10), color = "black"),
    axis.text.x = element_text(size = 12, face = "bold"), # Increase size of x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"), # Increase size of y-axis tick labels
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(size = 1.5))

p2
#...............................................................................
# Arrange the plots in a 2x2 grid
grid.arrange(p1, p2, nrow = 2)
# Arrange HPF vs non‑HPF clearly (e.g. side‑by‑side)
grid.arrange(p1, p2, nrow = 1)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Corrlelation statistics for full sample
# Spearman correlations


# Full sample: EED vs ED
cor.test(food_data$Estimated_ED, food_data$Energy_density,
         method = "spearman")
#S = 38622, p-value = 0.001026
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.3790259 

# HPF only: EED vs ED
cor.test(filtered_data_HPF$Estimated_ED, filtered_data_HPF$Energy_density,
         method = "spearman")
#S = 1255.7, p-value = 0.0001489
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.6563569 

# Non‑HPF only: EED vs ED
cor.test(filtered_data_NHPF$Estimated_ED, filtered_data_NHPF$Energy_density,
         method = "spearman")
#S = 12862, p-value = 0.5456
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.09360023 

# Bayesian t‑tests (group differences)

# Bayesian t‑test: ED by palatability
ttestBF(Energy_density ~ Palatability, data = food_data)
#[1] Alt., r=0.707 : 0.2484097 ±0.01%

# Bayesian t‑test: EED by palatability
ttestBF(Estimated_ED ~ Palatability, data = food_data)
#[1] Alt., r=0.707 : 0.7169468 ±0.01%
#...............................................................................

#---------------------------#
# Bayesian correlations (exploratory)
#---------------------------#

# Full sample (not reported in manuscript; exploratory)
correlationBF(y = food_data$Energy_density,
              x = food_data$Estimated_ED)
#[1] Alt., r=0.333 : 32.28543 ±0%

# HPF: ED vs EED
correlationBF(y = filtered_data_HPF$Energy_density,
              x = filtered_data_HPF$Estimated_ED)
#  [1]Alt., r=0.333 : 371.2954 ±0% 

# Non‑HPF: ED vs EED
correlationBF(y = filtered_data_NHPF$Energy_density,
              x = filtered_data_NHPF$Estimated_ED)
#[1] Alt., r=0.333 : 0.7666349 ±0%
#...............................................................................
#::::::::::::::::::::THE END::::::::::::::::::::::::::::::::::::::::::::::::::::





