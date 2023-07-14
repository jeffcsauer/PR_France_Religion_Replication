## ---------------------------
##
## Script name: Processing Insee data
##
## Purpose of script: Load and combine various department-level datasets. 
## Ultimately produces the final dataset used the analysis script.
##
## Author: Overos et al. 2023
##
## Date Created: 7/18/2021
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

library(readxl)
library(dplyr)

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
} else if (currentuser == 'hoveros') {
  setwd("/Henry/To/Fill/In")
} else {
  print("USER NOT DETECTED. Ensure that you have downloaded the replication materials.")
}

### Start processing data ----

## Population

POP2017 <-
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_POP1A_2017/BTX_TD_POP1A_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

# Get simple rowsum
POP2017 <- POP2017 %>%
  mutate(INSEE_TotPop = rowSums(select(., starts_with("SEX")))) %>%
  select(CODGEO, LIBGEO, INSEE_TotPop)

## Immigration

IMM2017 <-
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_IMG1A_2017/BTX_TD_IMG1A_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

# Get sum of immigrants, and sum of all, calculate pct

IMM2017 <- IMM2017 %>%
  mutate(
    INSEE_ImmNum = rowSums(select(., contains("IMMI1"))),
    INSEE_ImmDenom = rowSums(select(., contains("AGE"))),
    INSEE_ImmPct = (INSEE_ImmNum / INSEE_ImmDenom) * 100
  ) %>%
  select(CODGEO, LIBGEO, INSEE_ImmNum, INSEE_ImmDenom, INSEE_ImmPct)

## Foreign Population

NAT2017 <-
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_NAT1_2017/BTX_TD_NAT1_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

# Get sum of etrangers, and sum of all, calculate pct

NAT2017 <- NAT2017 %>%
  mutate(
    INSEE_NatNum = rowSums(select(., contains("INATC2"))),
    INSEE_NatDenom = rowSums(select(., contains("AGE"))),
    INSEE_NatPct = (INSEE_NatNum / INSEE_NatDenom) * 100
  ) %>%
  select(CODGEO, LIBGEO, INSEE_NatNum, INSEE_NatDenom, INSEE_NatPct)

## Unemployed

UNEMP2017 <-
  read_xlsx(
    './data/raw/FR_Census/base-ccx-emploi-pop-active-2017/base-cc-emploi-pop-active-2017.xlsx',
    sheet = 'COM_2017',
    skip = 5
  )

# Numerator = P17_CHOM1564: Ch?meurs 15-64 ans en 2017 (princ)
# Denominator = P17_POP1564: Pop 15-64 ans en 2017 (princ)

UNEMP2017 <- UNEMP2017 %>%
  select(CODGEO, LIBGEO, P17_CHOM1564, P17_POP1564) %>%
  mutate(INSEE_UnempPct = (P17_CHOM1564 / P17_POP1564) * 100) %>%
  rename(INSEE_UnempNum = P17_CHOM1564,
         INSEE_UnempDenom = P17_POP1564)

## Education

EDU2017 <-
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_FOR2_2017/BTX_TD_FOR2_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

# Low Education
# Contains DIPL_19A - A : Aucun dipl?me ou certificat d'?tudes primaires
# ENG: No primary school diploma or certificate

LOWEDU2017 <- EDU2017 %>%
  mutate(
    INSEE_LowEduNum = rowSums(select(., contains("DIPL_19A"))),
    INSEE_LowEduDenom = rowSums(select(., contains("AGE"))),
    INSEE_LowEduPct = (INSEE_LowEduNum / INSEE_LowEduDenom) * 100
  ) %>% 
  select(CODGEO, LIBGEO, INSEE_LowEduNum, INSEE_LowEduDenom, INSEE_LowEduPct)

# High Education

HIGHEDU2017 <- EDU2017 %>%
  mutate(
    INSEE_HighEduNum = rowSums(select(., contains(c("DIPL_19D", "DIPL_19E", "DIPL_19F", "DIPL_19G")))),
    INSEE_HighEduDenom = rowSums(select(., contains("AGE"))),
    INSEE_HighEduPct = (INSEE_HighEduNum / INSEE_HighEduDenom) * 100
  ) %>% 
  select(CODGEO, LIBGEO, INSEE_HighEduNum, INSEE_HighEduDenom, INSEE_HighEduPct)

## Types of workers

EMP2017 <-
  read_xlsx(
    './data/raw/FR_Census/base-ic-activite-residents-2017/base-ic-activite-residents-2017.xlsx',
    sheet = 'IRIS',
    skip = 5
  )

EMPProc2017 <- EMP2017 %>%
  rename(
    INSEE_EmpDenom = C17_ACTOCC1564, # All workers
    INSEE_EmpAgriNum = C17_ACTOCC1564_CS1, # Agricultural
    INSEE_EmpArtNum = C17_ACTOCC1564_CS2, # Artisans
    INSEE_EmpHighSkillNum = C17_ACT1564_CS3, # high skill
    INSEE_EmpInterSkillNum = C17_ACT1564_CS4, # intermediate skill
    INSEE_EmpWhtCollarNum = C17_ACT1564_CS5, # white collar
    INSEE_EmpManualNum = C17_ACT1564_CS6, # manual labor
    CODGEO = COM,
    LIBGEO = LIBCOM
  ) %>%
  group_by(CODGEO, LIBGEO) %>% 
  summarize(
    INSEE_EmpDenom = sum(INSEE_EmpDenom, na.rm = T),
    INSEE_EmpAgriNum = sum(INSEE_EmpAgriNum, na.rm = T),
    INSEE_EmpArtNum = sum(INSEE_EmpArtNum, na.rm = T),
    INSEE_EmpHighSkillNum = sum(INSEE_EmpHighSkillNum, na.rm = T),
    INSEE_EmpInterSkillNum = sum(INSEE_EmpInterSkillNum, na.rm = T),
    INSEE_EmpWhtCollarNum =sum( INSEE_EmpWhtCollarNum, na.rm = T),
    INSEE_EmpManualNum = sum(INSEE_EmpManualNum, na.rm = T)
  ) %>% 
  mutate(
    INSEE_EmpAgriPct = (INSEE_EmpAgriNum / INSEE_EmpDenom) * 100,
    INSEE_EmpArtPct = (INSEE_EmpArtNum / INSEE_EmpDenom) * 100,
    INSEE_EmpHighSkillPct = (INSEE_EmpHighSkillNum / INSEE_EmpDenom) * 100,
    INSEE_EmpInterSkillPct = (INSEE_EmpInterSkillNum / INSEE_EmpDenom) * 100,
    INSEE_EmpWhtCollarPct = (INSEE_EmpWhtCollarNum / INSEE_EmpDenom) * 100,
    INSEE_EmpManualPct = (INSEE_EmpManualNum / INSEE_EmpDenom) * 100
  ) %>% 
  select(CODGEO, LIBGEO,
         INSEE_EmpAgriPct, INSEE_EmpArtPct, INSEE_EmpHighSkillPct,
         INSEE_EmpInterSkillPct, INSEE_EmpWhtCollarPct, INSEE_EmpManualPct)

# Drop a few random NaN - lose about 60 obs from rest of dataset, very small pct

EMPProc2017 <- EMPProc2017[complete.cases(EMPProc2017),]

# Rootedness

# Combination of three numerators

# Numerator 1:

ROOT2017Num1 <- 
  read_xlsx(
    './data/raw/FR_Census/base-ic-activite-residents-2017/base-ic-activite-residents-2017.xlsx',
    sheet = 'IRIS',
    skip = 5
  )

ROOT2017Num1Proc <- ROOT2017Num1 %>%
  rename(CODGEO = COM,
         LIBGEO = LIBCOM) %>%
  group_by(CODGEO, LIBGEO) %>%
  summarize(
    P17_ACTOCC15P_ILT1_sum = sum(P17_ACTOCC15P_ILT1, na.rm = T),
    C17_ACT1564_sum = sum(C17_ACT1564, na.rm = T)
  ) %>%
  mutate(INSEE_RootNum1Pct = (P17_ACTOCC15P_ILT1_sum / C17_ACT1564_sum) * 100) %>%
  select(CODGEO, LIBGEO, INSEE_RootNum1Pct)

# remove a few bad values

ROOT2017Num1Proc <- ROOT2017Num1Proc[complete.cases(ROOT2017Num1Proc),]
ROOT2017Num1Proc <- ROOT2017Num1Proc[!is.infinite(ROOT2017Num1Proc$INSEE_RootNum1Pct),]

# Numerator 2:

PRINC2 <- 
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_PRINC2_2017/BTX_TD_PRINC2_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

PRINC2 <- PRINC2 %>% 
  mutate(INSEE_STOC_10 = rowSums(select(., contains("STOCD10"))),
         INSEE_STOC_21 = rowSums(select(., contains("STOCD21"))),
         INSEE_STOC_22 = rowSums(select(., contains("STOCD22"))),
         INSEE_STOC_23 = rowSums(select(., contains("STOCD23"))),
         INSEE_STOC_30 = rowSums(select(., contains("STOCD30"))),
  ) %>% 
  mutate(
    INSEE_RootNum2Pct = ((INSEE_STOC_10) / (INSEE_STOC_10 + INSEE_STOC_21 + INSEE_STOC_22 + INSEE_STOC_23 + INSEE_STOC_30))*100
  ) %>% 
  select(
    CODGEO, LIBGEO, INSEE_RootNum2Pct
  )

# Numerator 3:

PRINC5 <- 
  read_xlsx(
    './data/raw/FR_Census/BTX_TD_PRINC5_2017/BTX_TD_PRINC5_2017.xlsx',
    sheet = 'COM',
    skip = 10
  )

PRINC5 <- PRINC5 %>% 
  mutate(INSEE_AMER2_0 = rowSums(select(., contains("ANEMR20"))),
         INSEE_AMER2_1 = rowSums(select(., contains("ANEMR21"))),
         INSEE_AMER2_2 = rowSums(select(., contains("ANEMR22"))),
         INSEE_AMER2_3 = rowSums(select(., contains("ANEMR23"))),
         INSEE_AMER2_4 = rowSums(select(., contains("ANEMR24"))),
         INSEE_AMER2_5 = rowSums(select(., contains("ANEMR25")))
  ) %>% 
  mutate(
    INSEE_RootNum3Pct = ((INSEE_AMER2_3 + INSEE_AMER2_4 + INSEE_AMER2_5) / (INSEE_AMER2_0 + INSEE_AMER2_1 + INSEE_AMER2_2 + INSEE_AMER2_3 + INSEE_AMER2_4 + INSEE_AMER2_5))*100
  ) %>% 
  select(
    CODGEO, LIBGEO, INSEE_RootNum3Pct
  )

# Join together 

ROOT2017 <- plyr::join_all(list(ROOT2017Num1Proc,PRINC2,PRINC5), by = c('CODGEO', 'LIBGEO'), type = 'left')

# Remove values above 100
ROOT2017 <- ROOT2017 %>% 
  filter(INSEE_RootNum1Pct <= 100 & INSEE_RootNum2Pct <= 100 & INSEE_RootNum3Pct <= 100)

# Calculate final rootedness index
ROOT2017 <- ROOT2017 %>% 
  mutate(INSEE_RootednessPct = (INSEE_RootNum1Pct + INSEE_RootNum2Pct + INSEE_RootNum3Pct)/3)

#### Join all datasets together ----

# Join all datasets

INSEE <- plyr::join_all(
  list(
    EMPProc2017,
    HIGHEDU2017,
    IMM2017,
    LOWEDU2017,
    NAT2017,
    POP2017,
    ROOT2017,
    UNEMP2017
  ),
  by = c('CODGEO', 'LIBGEO'),
  type = 'left'
)

# Filter to necessary variables
INSEEFinal <- INSEE %>%
  select(CODGEO, LIBGEO, INSEE_TotPop, contains("PCT"))

# Export
write.csv(INSEEFinal,
          "D:/Dropbox/FR Catholic Paper/data/final/INSEE_variables.csv")

### Join dataset to existing geopackage file to create final dataset ----

# Load commune dataset
FRCommunesElecRelig <-
  readOGR("./data/final/FRCommunes_outcomes_exposures.gpkg")

# Transofrm some variables in the INSEE final dataset to enable merging between
# the two sources

# Extract first two characters for DeptCode
INSEEFinal$DeptCode <- as.numeric(substr(INSEEFinal$CODGEO, 1, 2))

# Extract last three characters for CommuneCode
INSEEFinal$CommuneCode <-
  as.numeric(substr(INSEEFinal$CODGEO, 3, 5))

# Merge INSEE variables to commune dataset
FRFinal <-
  merge(FRCommunesElecRelig,
        INSEEFinal,
        by = c("DeptCode", "CommuneCode"))

# Convert to simple features object
FRFinal <- st_as_sf(FRFinal)

# Write final dataset - note this may be relatively large (~0.3GB)
# so this operation may take a few seconds.
st_write(FRFinal,
         "./data/final/FRCommunes_analysis.gpkg",
         delete_layer = TRUE)