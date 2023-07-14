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
} else if (currentuser == 'Henry') {
  setwd("C:/Users/Henry/Dropbox/10-19 Research Projects/13 French Catholic Paper/FR Catholic Paper")
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

plot(FRData$CommCountMosques, FRData$Votes_Expressed_CommPct_T2_LEPEN)
plot(FRData$CommCountMosques, FRData$Votes_Expressed_CommPct_T2_MACRON)

# Drop observations that exceed the expected maximum of 100% (n=67 observations)
FRData <- FRData %>%
  dplyr::filter(!if_any(dplyr::starts_with("INSEE_"), ~ . > 100))

### Descriptive statistics ---- 
stargazer::stargazer(FRData)
datasummary_skim(FRData)

### Create a per capita variable ----

hist(FRData$CommCountCatholic)
hist(FRData$CommCountMosques)

# Replace existing CommCountCatholic and CommCountMosques, INSEE_TotPop
# is already population per 100,000, so this becomes 
# commune count of religious buildings per 100,000
FRData$CommCountCatholic <- FRData$CommCountCatholic/FRData$INSEE_TotPop
FRData$CommCountMosques <- FRData$CommCountMosques/FRData$INSEE_TotPop

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


modelsummary(models,
             stars = T, # deprecated after version 0.8.0 of the modelsummary() package
             title = "Wave 1 Results",
             output = "writing/SupplementaryWave1.tex"
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
LEPEN_Model4_T2 <- 'Votes_Expressed_CommPct_T2_LEPEN ~ CommCountCatholic + CommCountMosques + \
                INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
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
MACRON_Model4_T2 <- 'Votes_Expressed_CommPct_T2_MACRON ~ CommCountCatholic + CommCountMosques + \
                INSEE_ImmPct + INSEE_UnempPct + INSEE_LowEduPct + \
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
    "Macron Wave 2 - M3" = lm(MACRON_Model3_T2, data = FRData),
  )

modelsummary(models,
             stars = T,
             title = "Wave 2 Results")

# Create a figure plot for the coefficients
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
      "CommCountMosques" = "Commune Mosques (per 100,000)",
      "CommCountCatholic" = "Commune Catholic Buildings (per 100,000)"
  )
) +
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12)
  )
DotPlot

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
      "CommCountMosques" = "Commune Mosques (per 100,000)",
      "CommCountCatholic" = "Commune Catholic Buildings (per 100,000)"
    )
  ) +
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12)
  )

DotPlot

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

# TeX summary
modelsummary(models_gironde, stars = T, output = "latex")
# View in R
modelsummary(models_gironde, stars = T)