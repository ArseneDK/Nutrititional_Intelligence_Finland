# ==============================================================================================================================================
# 
# Nutritional intelligence in FinnFoodPics
# created by Arsene Kanyamibwa (arsene.kanyamibwa@helsinki.fi) and reviewed by Manon Chédeville (manon.chedeville@helsinki.fi)
#
# created: 18.06.2025
# last updated: 07.10.2025 
#
# ============================================================================================================================================== 
#Clear environment
rm(list=ls())

#load packages and open libraries
#install packages (if needed)

install.packages("tidyverse")
install.packages("ggpubr")
install.packages("BayesFactor")
install.packages("dplyr")
install.packages("knitr")
install.packages("effsize")
install.packages("DFBA")
install.packages("coda")
install.packages("Matrix")
install.packages("gridExtra")
install.packages("car")

#Load the necessary libraries 
{
  library(tidyverse)
  library(coda)
  library(Matrix)
  library(ggpubr)
  library(BayesFactor)
  library(knitr)
  library(DFBA)
  library(dplyr)
  library(gridExtra)
  library(car)
  library(effsize)
}  

#load excel file 
#set path
setwd("P:/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis")
#set path for Mac
setwd('/Volumes/h345/obrain_labspace/Projects/PhD_projects/MARVEL/02_MARVEL I_Macronutrient study/03_Experiment/Aim 1/FoodRating task/04_Analysis')

food_data <- read.csv("0_Complete_pictureset_data.csv")

#remove column X
food_data <- food_data[, -1]

#change class for the categories 
{
  food_data$Category <- as.factor(food_data$Category)
  food_data$Palatability <- as.factor(food_data$Palatability)
  food_data$NOVA_Group <- as.factor(food_data$NOVA_Group)
  food_data$Energy_levels <- as.factor(food_data$Energy_levels)
  food_data$Energy_levels2 <- as.factor(food_data$Energy_levels2)
  food_data$NOVA_Group2 <- as.factor(food_data$NOVA_Group2)
}

{
  #display factor levels for category
  levels(food_data$Energy_levels)
  #reorder factor category 
  food_data$Energy_levels <- factor(food_data$Energy_levels, levels = c("HED","MED","LED"))
  #display factor levels for category
  levels(food_data$Energy_levels2)
  #reorder factor category 
  food_data$Energy_levels2 <- factor(food_data$Energy_levels2, levels = c("VHED","HED","MED","LED","VLED"))
}
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
###Assumptions check for the whole sample
##Actual Energy density

#Normality test 
shapiro.test(food_data$Energy_density)
#W = 0.98925, p-value = 0.802 data is normally distributed

# Perform Levene's test
leveneTest(Energy_density ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#                Df F value  Pr(>F)  
#Energy density  1  2.8513 0.09575 . Equal Variance (trend)
#      70                  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Student's t-test (assuming equal variances)
t.test(Energy_density ~ Palatability, data = food_data, var.equal = TRUE)
#t = -0.033026, df = 70, p-value = 0.9737
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
#  -0.6224608  0.6021816
#sample estimates:
#  mean in group HPF mean in group Not HPF 
#3.561429              3.571568 

##Estimated energy density 

#Normality test
shapiro.test(food_data$Estimated_ED)
#W = 0.90921, p-value = 7.301e-05 data is NOT normally distributed

# Perform Levene's test
leveneTest(Estimated_ED ~ Palatability, data = food_data)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1  0.6512 0.4224 Equal variance
#      70 

# Welch's test
t.test(Estimated_ED ~ Palatability, data = food_data)
#t = 1.6467, df = 64.866, p-value = 0.1045
#alternative hypothesis: true difference in means between group HPF and group Not HPF is not equal to 0
#95 percent confidence interval:
# -0.7746267  8.0519728
#sample estimates:
#    mean in group HPF mean in group Not HPF 
#             63.95121              60.31254

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Filter the data for Category "HPF"
filtered_data_HPF <- food_data %>% filter(Palatability == "HPF")
# Filter the data for Category "NHPF"
filtered_data_NHPF <- food_data %>% filter(Palatability == "Not HPF")

###Assumptions check for the HPF
#Normality test
shapiro.test(filtered_data_HPF$Energy_density)
#W = 0.93903, p-value = 0.1044 data is normally distributed
shapiro.test(filtered_data_NHPF$Energy_density)
#W = 0.94627, p-value = 0.04002 data is NOT normally distributed

shapiro.test(filtered_data_HPF$Estimated_ED)
#W =  0.8931, p-value = 0.007917 data is NOT normally distributed

#Assumptions check for the  not HPF
#Normality test
shapiro.test(filtered_data_NHPF$Estimated_ED)
#W = 0.91112, p-value = 0.002434 is NOT normally distributed

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#ED vs EED
#normality failed for 3 out of 4 tests hence I will use Spearman 

#HPF
p1 <- ggscatter(filtered_data_HPF, x = "Energy_density", y = "Estimated_ED", 
                add = "reg.line",
                ggtheme = theme_minimal(),
                conf.int = TRUE, 
                cor.coef = TRUE,
                cor.method = "spearman",
                ggp = NULL,
                show.legend.text = NA,
                cor.coef.size = 5,
                cor.coef.coord = c(2, 35),
                color = "#ffc107ff",   # more vivid HPF color
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
#ED vs EED
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
                color = "#4a8ce1ff",   
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
#Frequentist statistic for whole sample
cor.test(food_data$Estimated_ED, food_data$Energy_density, method=c("spearman"))
#S = 38622, p-value = 0.001026
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.3790259 

#Frequentist statistic for HPF
cor.test(filtered_data_HPF$Estimated_ED, filtered_data_HPF$Energy_density, method=c("spearman"))
#S = 1255.7, p-value = 0.0001489
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.6563569 

#Frequentist statistic for Non HPF
cor.test(filtered_data_NHPF$Estimated_ED, filtered_data_NHPF$Energy_density, method=c("spearman"))
#S = 12862, p-value = 0.5456
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.09360023 

#Bayesian t-tests to complement the frequentist analysis
ttestBF(formula = Energy_density ~ Palatability, data = food_data)
#[1] Alt., r=0.707 : 0.2484097 ±0.01%

ttestBF(formula = Estimated_ED ~ Palatability, data = food_data)
#[1] Alt., r=0.707 : 0.7169468 ±0.01%
#...............................................................................
#Correlations in Bayesian statistics

#Bayesian statistic for the whole sample (NOT Reported in the paper)
correlationBF(y= food_data$Energy_density,
              x= food_data$Estimated_ED)
#[1] Alt., r=0.333 : 32.28543 ±0%

#Bayesian statistic for HPF ED vs HPF EED
correlationBF(y= filtered_data_HPF$Energy_density,
              x= filtered_data_HPF$Estimated_ED)
#Bayes factor analysis
#--------------
#  [1]Alt., r=0.333 : 371.2954 ±0% 

#Bayesian statistic for NHPF ED vs NHPF EED
correlationBF(y= filtered_data_NHPF$Energy_density,
              x= filtered_data_NHPF$Estimated_ED)
#[1] Alt., r=0.333 : 0.7666349 ±0%
#...............................................................................
#::::::::::::::::::::THE END::::::::::::::::::::::::::::::::::::::::::::::::::::





