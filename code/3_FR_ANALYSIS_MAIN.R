## ---------------------------
##
## Script name: Analyzing FR Election and Religion Data
##
## Purpose of script: Conduct exploratory and regression analysis on a dataset
## constructed of religious buildings, political outcomes, and demographic variables.
##
## Author: Overos et al. 2023
##
## Date Created: 7/31/2021
##
## Email: jcsauer@terpmail.umd.edu
##
## Script template: https://timfarewell.co.uk/my-r-script-header-template/
## ---------------------------
##
## Notes: Code follows tidyverse R Style Guide. Autoformatted using
## the `styler` package and addin.
##
## ---------------------------

### Ensure workspace is clean ----
rm(list = ls())

### Load necessary packages ----
library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(modelsummary)
library(dad)

### Set WD ----

# Get the current user of the script
currentuser <- Sys.info()['effective_user']

# Run loop to set appropriate working directory based on teammate's computer.

# Jeff laptop
if (currentuser == 'jeffe') {
  setwd("C:/Users/jeffe/Dropbox/FR Catholic Paper/")
  # Jeff desktop
} else if (currentuser == 'USER') {
  setwd("D:/Dropbox/FR Catholic Paper/")
  # Henry
} else if (currentuser == 'ohenr') {
  setwd("C:/Users/ohenr/Dropbox/10-19 Research Projects/13 French Catholic Paper/FR Catholic Paper")
} else {
  print("USER NOT DETECTED. Ensure that you have downloaded the replication materials.")
}

### Load analysis datasets ----

# Conduct a few final variable adjustments before running regressions

# Read in commune level dataset
FRDataSpatial <- st_read("./data/final/FRCommunes_analysis.gpkg")

# Drop geometry column - read and view data much faster as geom column is a
# large list for each observation
FRData <- st_drop_geometry(FRDataSpatial)

# Drop Wikipedia column
FRData <- subset(FRData, select = -c(wikipedia))

# Reduce to only observations where there is data for all communes
FRData <- FRData[complete.cases(FRData),]

# Divide population by 100,000 following Fitzgerald
FRData$INSEE_TotPop <- FRData$INSEE_TotPop / 100000

# Convert outcome to 0-100 rather than 0-1
FRData$Votes_Expressed_CommPct_T1_LEPEN <-
  (FRData$Votes_Expressed_CommPct_T1_LEPEN) * 100
FRData$Votes_Expressed_CommPct_T1_MACRON <-
  (FRData$Votes_Expressed_CommPct_T1_MACRON) * 100
FRData$Votes_Expressed_CommPct_T1_FILLON <-
  (FRData$Votes_Expressed_CommPct_T1_FILLON) * 100
FRData$Votes_Expressed_CommPct_T2_LEPEN <-
  (FRData$Votes_Expressed_CommPct_T2_LEPEN) * 100
FRData$Votes_Expressed_CommPct_T2_MACRON <-
  (FRData$Votes_Expressed_CommPct_T2_MACRON) * 100

# Drop observations that exceed the expected maximum of 100% (n=67 observations)
FRData <- FRData %>%
  dplyr::filter(!if_any(dplyr::starts_with("INSEE_"), ~ . > 100))

### Descriptive statistics ---- 

# Table 1: Descriptive Statistics of 2017 French Election Data

stargazer::stargazer(FRData)
datasummary_skim(FRData)

### Regression models ---- 

## Wave 1

LEPEN_Model1_T1 <- 'Votes_Expressed_CommPct_T1_LEPEN ~ CommCountCatholic'
LEPEN_Model2_T1 <- 'Votes_Expressed_CommPct_T1_LEPEN ~ CommCountCatholic + CommCountMosques'
LEPEN_Model3_T1 <- 'Votes_Expressed_CommPct_T1_LEPEN ~ CommCountCatholic + CommCountMosques + \
                INSEE_TotPop + INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
                INSEE_HighEduPct + INSEE_EmpAgriPct + INSEE_EmpArtPct +
                INSEE_EmpHighSkillPct + INSEE_EmpInterSkillPct + \
                INSEE_EmpWhtCollarPct + INSEE_EmpManualPct + INSEE_RootednessPct'

MACRON_Model1_T1 <- 'Votes_Expressed_CommPct_T1_MACRON ~ CommCountCatholic'
MACRON_Model2_T1 <- 'Votes_Expressed_CommPct_T1_MACRON ~ CommCountCatholic + CommCountMosques'
MACRON_Model3_T1 <- 'Votes_Expressed_CommPct_T1_MACRON ~ CommCountCatholic + CommCountMosques + \
                INSEE_TotPop + INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
                INSEE_HighEduPct + INSEE_EmpAgriPct + INSEE_EmpArtPct +
                INSEE_EmpHighSkillPct + INSEE_EmpInterSkillPct + \
                INSEE_EmpWhtCollarPct + INSEE_EmpManualPct + INSEE_RootednessPct'

FILLON_Model1_T1 <- 'Votes_Expressed_CommPct_T1_FILLON ~ CommCountCatholic'
FILLON_Model2_T1 <- 'Votes_Expressed_CommPct_T1_FILLON ~ CommCountCatholic + CommCountMosques'
FILLON_Model3_T1 <- 'Votes_Expressed_CommPct_T1_FILLON ~ CommCountCatholic + CommCountMosques + \
                INSEE_TotPop + INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
                INSEE_HighEduPct + INSEE_EmpAgriPct + INSEE_EmpArtPct +
                INSEE_EmpHighSkillPct + INSEE_EmpInterSkillPct + \
                INSEE_EmpWhtCollarPct + INSEE_EmpManualPct + INSEE_RootednessPct'

models <-
  list(
    #"Le Pen Wave 1 - M1" = lm(LEPEN_Model1, data = FRData),
    #"Macron Wave 1 - M1" = lm(MACRON_Model1, data = FRData),
    #"Fillon Wave 1 - M1" = lm(FILLON_Model1, data = FRData),
    #"Le Pen Wave 1 - M2" = lm(LEPEN_Model2, data = FRData),
    #"Macron Wave 1 - M2" = lm(MACRON_Model2, data = FRData),
    #"Fillon Wave 1 - M2" = lm(FILLON_Model2, data = FRData),
    "Le Pen Wave 1 - M3" = lm(LEPEN_Model3_T1, data = FRData),
    "Macron Wave 1 - M3" = lm(MACRON_Model3_T1, data = FRData),
    "Fillon Wave 1 - M3" = lm(FILLON_Model3_T1, data = FRData)
  )

# Table 2: OLS regression results on the relationship between election outcomes and presence of religious
# buildings for Wave 1 (preliminary) of the 2017 French Presidential election, adjusted for commune-level
# controls

modelsummary(models,
             #stars = T, # deprecated after version 0.8.0 of the modelsummary() package
             title = "Wave 1 Results",
             output = "latex"
)

modelsummary(models,
             stars = T,
             title = "Wave 1 Results"
)

## Wave 2

LEPEN_Model1_T2 <- 'Votes_Expressed_CommPct_T2_LEPEN ~ CommCountCatholic'
LEPEN_Model2_T2 <- 'Votes_Expressed_CommPct_T2_LEPEN ~ CommCountCatholic + CommCountMosques'
LEPEN_Model3_T2 <- 'Votes_Expressed_CommPct_T2_LEPEN ~ CommCountCatholic + CommCountMosques + \
                INSEE_TotPop + INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
                INSEE_HighEduPct + INSEE_EmpAgriPct + INSEE_EmpArtPct +
                INSEE_EmpHighSkillPct + INSEE_EmpInterSkillPct + \
                INSEE_EmpWhtCollarPct + INSEE_EmpManualPct + INSEE_RootednessPct'

MACRON_Model1_T2 <- 'Votes_Expressed_CommPct_T2_MACRON ~ CommCountCatholic'
MACRON_Model2_T2 <- 'Votes_Expressed_CommPct_T2_MACRON ~ CommCountCatholic + CommCountMosques'
MACRON_Model3_T2 <- 'Votes_Expressed_CommPct_T2_MACRON ~ CommCountCatholic + CommCountMosques + \
                INSEE_TotPop + INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
                INSEE_HighEduPct + INSEE_EmpAgriPct + INSEE_EmpArtPct +
                INSEE_EmpHighSkillPct + INSEE_EmpInterSkillPct + \
                INSEE_EmpWhtCollarPct + INSEE_EmpManualPct + INSEE_RootednessPct'

# Build models table

models <-
  list(
    #"Le Pen Wave 2 - M1" = lm(LEPEN_Model1, data = FRData),
    #"Macron Wave 2 - M1" = lm(MACRON_Model1, data = FRData),
    #"Le Pen Wave 2 - M2" = lm(LEPEN_Model2, data = FRData),
    #"Macron Wave 2 - M2" = lm(MACRON_Model2, data = FRData),
    "Le Pen Wave 2 - M3" = lm(LEPEN_Model3_T2, data = FRData),
    "Macron Wave 2 - M3" = lm(MACRON_Model3_T2, data = FRData)
  )

# Table 3: OLS regression results on the relationship between election outcomes and presence of religious
# buildings for Wave 2 (final) of the 2017 French Presidential election, adjusted for commune-level
# controls

modelsummary(models,
             stars = T,
             title = "Wave 2 Results",
             output = "latex")


modelsummary(models,
             stars = T,
             title = "Wave 2 Results")

# Figure 2: Coefficient plot for Wave 2 OLS models estimating candidate vote share. The Intercept and
# Total Population (per 100,000) coefficients were omitted from the plot as their large values obscured
# variation between the remaining coefficients.
DotPlot <-
  modelplot(
    models,
    coef_omit = "INSEE_TotPop|(Intercept)",
    coef_rename = c(
      "INSEE_RootednessPct" = "Rootedness (%)",
      "INSEE_EmpManualPct" = "Manual Employment (%)",
      "INSEE_EmpWhtCollarPct" = "White Collar Employment (%)",
      "INSEE_EmpInterSkillPct" = "Intermediate-skill Employment (%)",
      "INSEE_EmpHighSkillPct" = "High-skill Employment (%)",
      "INSEE_EmpArtPct" = "Artisanal Employment (%)",
      "INSEE_EmpAgriPct" = "Agricultural Employment (%)",
      "INSEE_HighEduPct" = "High Education (%)",
      "INSEE_LowEduPct" = "Low Education (%)",
      "INSEE_UnempPct" = "Unemployment (%)",
      "INSEE_ImmPct" = "Immigration (%)",
      "CommCountMosques" = "Commune Mosques",
      "CommCountCatholic" = "Commune Catholic Buildings"
  )
) +
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12)
  )

DotPlot

# Save the dot plot
#ggsave(
#  DotPlot,
#  filename = "./writing/figures/dotplot_wave2_regression.jpeg",
#  width = 8,
#  height = 6,
#  dpi = 320
#)

# Render the doplot with only Le Pen
modelstemp <- models
# Remove the Macron results
modelstemp$`Macron Wave 2 - M3` <- NULL 

DotPlot <-
  modelplot(
    modelstemp,
    coef_omit = "INSEE_TotPop|(Intercept)",
    coef_rename = c(
      "INSEE_RootednessPct" = "Rootedness (%)",
      "INSEE_EmpManualPct" = "Manual Employment (%)",
      "INSEE_EmpWhtCollarPct" = "White Collar Employment (%)",
      "INSEE_EmpInterSkillPct" = "Intermediate-skill Employment (%)",
      "INSEE_EmpHighSkillPct" = "High-skill Employment (%)",
      "INSEE_EmpArtPct" = "Artisanal Employment (%)",
      "INSEE_EmpAgriPct" = "Agricultural Employment (%)",
      "INSEE_HighEduPct" = "High Education (%)",
      "INSEE_LowEduPct" = "Low Education (%)",
      "INSEE_UnempPct" = "Unemployment (%)",
      "INSEE_ImmPct" = "Immigration (%)",
      "CommCountMosques" = "Commune Mosques",
      "CommCountCatholic" = "Commune Catholic Buildings"
    )
  ) +
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12)
  )

DotPlot

# Save the dot plot
#ggsave(
#  DotPlot,
#  filename = "./writing/figures/dotplot_wave2_regression_lepen.jpeg",
#  width = 8,
#  height = 6,
#  dpi = 320
#)

### Sensitivity analysis ----

# Limiting to Gironde

FRDataGironde <- FRData[FRData$DeptCode == 33, ]

models_gironde <-
  list(
    "Le Pen Wave 1 - M3" = lm(LEPEN_Model3_T1,  data = FRDataGironde),
    "Macron Wave 1 - M3" = lm(MACRON_Model3_T1, data = FRDataGironde),
    "Fillon Wave 1 - M3" = lm(FILLON_Model3_T1, data = FRDataGironde),
    "Le Pen Wave 2 - M3" = lm(LEPEN_Model3_T2,  data = FRDataGironde),
    "Macron Wave 2 - M3" = lm(MACRON_Model3_T2, data = FRDataGironde)
  )

# Table 4: Sensitivity analysis of OLS regression for the association of religious buildings on election share
# outcomes for the French Presidential election. Models are compared between all communes of France
# and all communes of Gironde.

# TeX summary
modelsummary(models_gironde, stars = T, output = "latex")
# View in R
modelsummary(models_gironde, stars = T)

### Other graphs ----

# Figure 3: Average estimated value for commune-level Le Pen vote share in Wave 2 of the 2017 French
# presidential election, by the number of mosques in a commune. Estimates and confidence intervals were
# gathered from fully adjusted regression models. Points with wide or missing confidence intervals indicate
# a limited number of communes with that number of mosques.

# vote for le pen coefficient by number of mosques

# Nationwide

FRData$ModelLePenFitted <- models$`Le Pen Wave 2 - M3`$fitted.values

test <- FRData %>% group_by(CommCountMosques) %>% 
  group_by(INSEE_TotPop) %>% AVG = mean(ModelLePenFitted, na.rm = T)

NationalBoxPlot <- FRData %>%
  group_by(CommCountMosques) %>%
  summarise(
    AvgEffect = mean(ModelLePenFitted, na.rm = T),
    UL = quantile(ModelLePenFitted, .95),
    LL = quantile(ModelLePenFitted, .05),
    SD = sd(ModelLePenFitted)
  )

BoxplotTop <-
  ggplot(NationalBoxPlot, aes(CommCountMosques, AvgEffect)) +
  geom_point() +
  geom_errorbar(aes(ymin = AvgEffect - SD, ymax = AvgEffect + SD)) +
  scale_x_continuous(breaks = seq(0, 28, 4)) +
  scale_y_continuous(breaks = seq(-50, 50, 10)) +
  ggtitle("") +
  xlab("Number of Mosques") +
  ylab("Average fitted value for Le Pen vote share") +
  theme_pubr() +
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12)
  )

BoxplotTop

#ggsave(
#  "./writing/figures/EffectBoxPlot_National.jpeg",
#  width = 12, 
#  height = 6, 
#  dpi = 320
#  )

### Response to reviewers: supplementary analysis ----

# Looking at the relationship between pct immigrant and 
# voter turnout for fn in SUZ

# Define a list of communtes that belong to ZUS (special zone)
# List made available by the french government from:
# https://sig.ville.gouv.fr/atlas/ZUS
ZUS <- readxl::read_xls("./data/raw/ZUS/ZUS_FR_SGCIV_20100701.xls", 
                        skip = 7)

colnames(ZUS)

# From: https://search.r-project.org/CRAN/refmans/dad/html/departments.html
data("departments")

# Merge the datasets. Note that the ZUS
# dataset has some french characters in the colname,
# so the accented e may not load correctly in by.x below.
# you may need to copy-paste the colname after calling colnames(ZUS)
ZUS <- merge(ZUS, 
             departments, 
             by.x = "Département(s)",
             by.y = "named")

# Covert coded (code department) to numeric to match 
# FRData DeptCode
ZUS$DeptCode <- as.numeric(levels(ZUS$coded))[ZUS$coded]

# Try to merge to FRData
FRDataZUS <- merge(ZUS, FRData, 
                   by.x = c("DeptCode", "Commune(s)"), 
                   by.y = c("DeptCode", "nom"))


# Figure 4: Scatterplot of commune-level proportion of votes expressed for LePen in Wave 2 of
# the 2017 French presidential election and commune-level proportion of immigrant residents.
# Each point represents one sensitive urban zones (zone urbaine sensible, ZUS). n=480 ZUS are
# shown in total, representing 67% of the n=715 ZUS located in mainland France. Not all ZUS
# could be included in the analysis due to a lack of comprehensive crosswalk available between
# ZUS and Communes.
ggscatter(FRDataZUS, 
          x = "INSEE_ImmPct",
          y = "Votes_Expressed_CommPct_T2_LEPEN", 
          # rug = TRUE
          shape = 21, size = 3, color = 'blue',  # Points color, shape and size
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = 35, label.sep = "\n")) + 
  labs(y = "Commune % Votes Expressed for LePen (T2)",
       x = "Commune % Immigrant Residents")

#ggsave(
#  "./writing/figures/ZUS_scatter.jpeg",
#  width = 8,
#  height = 6,
#  dpi = 320
#)

# Descriptive statistics

# ZUS - votes lepen
summary(FRDataZUS$Votes_Expressed_CommPct_T2_LEPEN)
Rmisc::CI(FRDataZUS$Votes_Expressed_CommPct_T2_LEPEN, ci=0.95)

# Nationwide - votes lepen
summary(FRData$Votes_Expressed_CommPct_T2_LEPEN)
Rmisc::CI(FRData$Votes_Expressed_CommPct_T2_LEPEN, ci=0.95)

# ZUS - immigration
summary(FRDataZUS$INSEE_ImmPct)
Rmisc::CI(FRDataZUS$INSEE_ImmPct, ci=0.95)

# Nationwide - immigration
summary(FRData$INSEE_ImmPct)
Rmisc::CI(FRData$INSEE_ImmPct, ci=0.95)

# ZUS - unemployment
summary(FRDataZUS$INSEE_UnempPct)
Rmisc::CI(FRDataZUS$INSEE_UnempPct, ci=0.95)

# Nationwide - unemployment
summary(FRData$INSEE_UnempPct)
Rmisc::CI(FRData$INSEE_UnempPct, ci=0.95)

### Fit regression models for ZUS

models_zus <-
  list(
    "Le Pen Wave 1 - M3" = lm(LEPEN_Model3_T1,  data = FRDataZUS),
    "Macron Wave 1 - M3" = lm(MACRON_Model3_T1, data = FRDataZUS),
    "Fillon Wave 1 - M3" = lm(FILLON_Model3_T1, data = FRDataZUS),
    "Le Pen Wave 2 - M3" = lm(LEPEN_Model3_T2,  data = FRDataZUS),
    "Macron Wave 2 - M3" = lm(MACRON_Model3_T2, data = FRDataZUS)
  )

# Table 5: Sensitivity analysis of OLS regression for the association of religious buildings on
# election share outcomes for the French Presidential election. Models are compared between
# all communes of France and n=480 sensitive urban zone (SUZ) communes across France.

# TeX summary
modelsummary(models_zus, stars = T, output = "latex")
# View in R
modelsummary(models_zus, stars = T)
