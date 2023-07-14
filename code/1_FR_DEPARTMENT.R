## ---------------------------
##
## Script name: Creating department-level dataset
##
## Purpose of script: Load and combine various department-level datasets
##
## Author: Overos et al. 2023
##
## Date Created: 6/20/2021
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
library(Guerry)
library(sf)
library(tmap)
library(ggmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

### Set working environment  ----

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
  setwd("/Henry/")
} else {
  print("USER NOT DETECTED. Ensure that you have downloaded the replication materials.")
}

### Create baseline dataset ----

# First load in a generic shapefile from Database of Global Administrative Areas
# describing French departments. We opt for a generic shapefile from GADM
# because the Guerry 1833 data provided by Friendly et al. 2020 (below)
# lacks projection information. 
FRDepartments <- readRDS("./data/raw/FR_Boundaries/gadm36_FRA_2_sf.rds")

# Also load in commune-level data
FRCommunes <- st_read("./data/raw/FR_Boundaries/communes-20170112.shp")

# Split varname insee
# first two chars: dept code
# last three chars: commune code
FRCommunes <- FRCommunes %>% 
  separate(insee, c("DeptCode", "CommuneCode"), sep = 2)

# Cast to numeric
FRCommunes$DeptCode <- as.numeric(FRCommunes$DeptCode)
FRCommunes$CommuneCode <- as.numeric(FRCommunes$CommuneCode)

# Drop rows if dept code is NA
FRCommunes <- FRCommunes[!is.na(FRCommunes$DeptCode), ]

# Now load in Guerry 1833 data provided by Friendly et al. 2020. Note that we use
# the version of the dataset that excludes the island of Corsica.
GuerryDS <- as.data.frame(gfrance85)

# View variable definitions in an internet browser
#browseURL("https://www.datavis.ca/gallery/guerry/guerrydat.html")

# Join to the GADM boundary file.
FRDepartments <- merge(FRDepartments, 
                       GuerryDS, 
                       by.x = "CC_2", 
                       by.y = "CODE_DEPT", 
                       all.x = T)

# Keep only those variables that are necessary
FRDepartments <- FRDepartments %>% 
  select(CC_2, Crime_pers:Pop1831) %>% 
  rename(DeptCode = CC_2)

# Convert Dept Code to numeric
FRDepartments$DeptCode <- as.numeric(FRDepartments$DeptCode)

# Drop departments for corsica (dept codes 2A and 2B)
FRDepartments <- FRDepartments[!is.na(FRDepartments$DeptCode), ]

### Geocode auxiliary datasets ----

# Much of this  of this section is commented out as it 
# only needs to be run once after the initial scrape. 
# If you are interested in recreating the results, continue
# to line 196.

register_google("INSERT-YOUR-GOOGLE-API-CODE-HERE")

# Mosques

#Mosques <-
#  read.csv("./data/raw/Mosques/trouvetamosquee_scrape.csv")

# Convert nonoutf8 characters
#Mosques$MosqueAddress <-
#  stringi::stri_enc_toutf8(Mosques$MosqueAddress, validate = T)

# Look through each row of dataset, geocoding if location is not NA
# Commented out to avoid re-geocoding, simply load the saved .csv below.
#for (i in 1:nrow(Mosques)) {
#  lonlat <- ggmap::geocode(as.character(Mosques$MosqueAddress[i]))
#  Mosques$lon[i] <- lonlat$lon
#  Mosques$lat[i] <- lonlat$lat
#}

# Drop the few (n=4) NAs
#Mosques <- Mosques[!is.na(Mosques$lon),]

# Drop a few mosques outside France (Brussels and Luxembourg )
#Mosques <- Mosques[Mosques$MosqueAddress!="32 Dernier Sol - 2543 LUXEMBOURG",]
#Mosques <- Mosques[Mosques$MosqueAddress!="Parc du cinquantenaire - 1000 ETTERBEEK",]

# Write
#write.csv(Mosques, "./data/final/trouvetamosquee_geocoded.csv")

# Catholic Institutions/Shrines

#Catholic <-
#  read.csv("./data/raw/CatholicDirectory/CatholicDirectoryScrape.csv")

# Convert nonoutf8 characters
#Catholic$Address <-
#  stringi::stri_enc_toutf8(Catholic$Address, validate = T)

# Look through each row of dataset, geocoding if location is not NA
# Commented out to avoid re-geocoding, simply load the saved .csv below.
#for (i in 1:nrow(Catholic)) {
#  lonlat <- ggmap::geocode(as.character(Catholic$Address[i]))
#  Catholic$lon[i] <- lonlat$lon
#  Catholic$lat[i] <- lonlat$lat
#}
#
## Clean up the text a bit more and then re-geocode with added 'France' string
#Catholic$AddressTrimmed <- gsub("(View ListingClick).*","",Catholic$Address)
#Catholic$AddressTrimmed <- gsub("(View Listing ).*","",Catholic$AddressTrimmed)
#Catholic$AddressTrimmed <- gsub("(Show Phone ).*","",Catholic$AddressTrimmed)
#
## Run loop again, but this time only get the location if the lat or lon
## is an extreme value, or NA
#for (i in 1:nrow(Catholic)) {
#  if (Catholic$lon[i] < -5.5591 ||
#      Catholic$lon[i] > 9.6624999 ||
#      Catholic$lat[i] < 41.31433 ||
#      Catholic$lat[i] > 51.1241999 || 
#      is.na(Catholic$lon[i]) ||
#      is.na(Catholic$lat[i])) {
#    lonlat <-
#      ggmap::geocode(paste0(as.character(Catholic$AddressTrimmed[i]), "France"))
#    Catholic$lon[i] <- lonlat$lon
#    Catholic$lat[i] <- lonlat$lat
#  }
#}
#
## Drop a few observations that are far outside the geographical limits of Francw
#Catholic <- Catholic %>% 
#  filter(lon >= -5.5591,
#         lon <= 9.6624999,
#         lat >= 41.31433,
#         lat <= 51.124199)
#
## Write
#write.csv(Catholic, "./data/final/CatholicDirectoryScrape_geocoded.csv")

### Counts of auxiliary datasets by departments ----

# Re-load geocoded datasets to continue with analysis
Mosques <- read.csv("./data/final/trouvetamosquee_geocoded.csv")
Catholic <- read.csv("./data/final/CatholicDirectoryScrape_geocoded.csv")

# Convert dataframe to a simple features (sf) object
Mosques <- st_as_sf(Mosques,
                    coords = c("lon", "lat"), 
                    crs = 4326)

Catholic <- st_as_sf(Catholic,
                     coords = c("lon", "lat"), 
                     crs = 4326)

# Before getting counts by department, ensure that they have the 
# same projection
st_crs(FRDepartments) == st_crs(Mosques)
st_crs(FRDepartments) == st_crs(Catholic)
st_crs(FRCommunes) == st_crs(Catholic)
st_crs(FRCommunes) == st_crs(Catholic)

# FIGURE 1: Point locations of Catholic and 
# Islamic buildings across France
basicdot <- tm_shape(FRDepartments) +
  tm_borders() +
  tm_shape(Catholic) +
  tm_dots(col = "#d95f02", size = 0.01) +
  tm_shape(Mosques) +
  tm_dots(col = "#1b9e77", size = 0.01) +
  tm_add_legend(
    "symbol",
    labels = c("Catholic Buildings", "Islamic Buildings"),
    col = c("#d95f02", "#1b9e77"),
    size = .3,
    lwd = 0
  ) + 
  tm_layout(frame = FALSE)

basicdot

#tmap_save(basicdot,
#          "./writing/figures/dotmap_cathisl_buildings.jpeg", 
#          dpi = 500)

# Get counts of Mosques and Catholic buildings by department
FRDepartments$DeptCountMosques <- lengths(st_intersects(FRDepartments, Mosques))
FRDepartments$DeptCountCatholic <- lengths(st_intersects(FRDepartments, Catholic))

# Get counts of Mosques and Catholic buildings by communes
FRCommunes$CommCountMosques <- lengths(st_intersects(FRCommunes, Mosques))
FRCommunes$CommCountCatholic <- lengths(st_intersects(FRCommunes, Catholic))

### Load and process French voting datasets ----

# Tour 1 Candidates - for reference
#Tour1Candidates <-
#  c(
#    "ASSELINEAU",
#    "DUPONT-AIGNAN",
#    "FILLON",
#    "HAMON",
#    "LASSALLE",
#    "LE PEN",
#    "MACRON",
#    "M?LENCHON",
#    "POUTOU",
#    "ARTHAUD",
#    "CHEMINADE"
#  )

# Source of election datasets
# Wave 1 (Tour 1): https://www.data.gouv.fr/en/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-communes/
# Wave 2 (Tour 2): https://www.data.gouv.fr/en/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-2nd-tour/

### Process Wave 1 dataset first

FRPresTour1 <-
  readxl::read_xls(
    "./data/raw/FR_Elections/Presidentielle_2017_Resultats_Communes_Tour_1_c.xls",
    skip = 3
  )

# Set column names
Tour1ColNames <- c(
  "DeptCode",
  "DeptName",
  "CommuneCode",
  "CommuneName",
  "Votes_Registered",
  "Votes_Abstention",
  "Votes_Abstentions_pct",
  "Votes_Cast",
  "Votes_Turnout_pct",
  "Votes_Blank",
  "Votes_Blank_Registered_pct",
  "Votes_Blank_Cast_pct",
  "Votes_Nulls",
  "Votes_Null_Registered_pct",
  "Votes_Null_Cast_pct",
  "Votes_Expressed_T1",
  "Votes_Expressed_Registered_pct",
  "Votes_Expressed_Cast_pct",
  "CandidateID1",
  "Cand_1_Sex",
  "Cand_1_Surname",
  "Cand_1_Firstname",
  "Cand_1_Votes",
  "Cand_1_Votes_Registered_pct",
  "Cand_1_Votes_Expressed_pct",
  "CandidateID2",
  "Cand_2_Sex",
  "Cand_2_Surname",
  "Cand_2_Firstname",
  "Cand_2_Votes",
  "Cand_2_Votes_Registered_pct",
  "Cand_2_Votes_Expressed_pct",
  "CandidateID3",
  "Cand_3_Sex",
  "Cand_3_Surname",
  "Cand_3_Firstname",
  "Cand_3_Votes",
  "Cand_3_Votes_Registered_pct",
  "Cand_3_Votes_Expressed_pct",
  "CandidateID4",
  "Cand_4_Sex",
  "Cand_4_Surname",
  "Cand_4_Firstname",
  "Cand_4_Votes",
  "Cand_4_Votes_Registered_pct",
  "Cand_4_Votes_Expressed_pct",
  "CandidateID5",
  "Cand_5_Sex",
  "Cand_5_Surname",
  "Cand_5_Firstname",
  "Cand_5_Votes",
  "Cand_5_Votes_Registered_pct",
  "Cand_5_Votes_Expressed_pct",
  "CandidateID6v",
  "Cand_6_Sex",
  "Cand_6_Surname",
  "Cand_6_Firstname",
  "Cand_6_Votes",
  "Cand_6_Votes_Registered_pct",
  "Cand_6_Votes_Expressed_pct",
  "CandidateID7",
  "Cand_7_Sex",
  "Cand_7_Surname",
  "Cand_7_Firstname",
  "Cand_7_Votes",
  "Cand_7_Votes_Registered_pct",
  "Cand_7_Votes_Expressed_pct",
  "CandidateID8",
  "Cand_8_Sex",
  "Cand_8_Surname",
  "Cand_8_Firstname",
  "Cand_8_Votes",
  "Cand_8_Votes_Registered_pct",
  "Cand_8_Votes_Expressed_pct",
  "CandidateID9",
  "Cand_9_Sex",
  "Cand_9_Surname",
  "Cand_9_Firstname",
  "Cand_9_Votes",
  "Cand_9_Votes_Registered_pct",
  "Cand_9_Votes_Expressed_pct",
  "CandidateID10",
  "Cand_10_Sex",
  "Cand_10_Surname",
  "Cand_10_Firstname",
  "Cand_10_Votes",
  "Cand_10_Votes_Registered_pct",
  "Cand_10_Votes_Expressed_pct",
  "CandidateID11",
  "Cand_11_Sex",
  "Cand_11_Surname",
  "Cand_11_Firstname",
  "Cand_11_Votes",
  "Cand_11_Votes_Registered_pct",
  "Cand_11_Votes_Expressed_pct"
)

colnames(FRPresTour1) <- Tour1ColNames

# Votes_Expressed is the sum of votes for each candidate and the blank votes

# First calculate total Votes_Expressed per department
FRPresTour1 <- FRPresTour1 %>% 
  group_by(DeptCode) %>% 
  mutate(Votes_Expressed_DeptTotal = sum(Votes_Expressed_T1)) 

# We see this matches *Rappel des r?sultats du d?partement au 1er tour - Exprim?s
# from https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/001/index.html

# Now we need to calculate total Votes_expressed per candidate per department

# Given the data structure we need to do some transformations that are a bit
# cumbersome. We first generate a an ungrouped VOTES_LEPEN and VOTES_MACRON variables.
# However, because they may be in any of the 10 variable groups, we have to identify take the 
# sum from the correct column.
FRPresTour1 <- FRPresTour1 %>% 
  mutate(VOTES_LEPEN_T1 = ifelse(Cand_1_Surname  == "LE PEN", Cand_1_Votes,
                                 ifelse(Cand_2_Surname  == "LE PEN", Cand_2_Votes,
                                        ifelse(Cand_3_Surname  == "LE PEN", Cand_3_Votes, 
                                               ifelse(Cand_4_Surname  == "LE PEN", Cand_4_Votes,
                                                      ifelse(Cand_5_Surname  == "LE PEN", Cand_5_Votes,
                                                             ifelse(Cand_6_Surname  == "LE PEN", Cand_6_Votes, 
                                                                    ifelse(Cand_7_Surname  == "LE PEN", Cand_7_Votes,
                                                                           ifelse(Cand_8_Surname  == "LE PEN", Cand_8_Votes,
                                                                                  ifelse(Cand_9_Surname  == "LE PEN", Cand_9_Votes,
                                                                                         ifelse(Cand_10_Surname == "LE PEN", Cand_10_Votes,
                                                                                                ifelse(Cand_11_Surname == "LE PEN", Cand_11_Votes,
                                                                                                       NA))))))))))),
         VOTES_MACRON_T1 = ifelse(Cand_1_Surname  == "MACRON", Cand_1_Votes,
                                  ifelse(Cand_2_Surname  == "MACRON", Cand_2_Votes,
                                         ifelse(Cand_3_Surname  == "MACRON", Cand_3_Votes, 
                                                ifelse(Cand_4_Surname  == "MACRON", Cand_4_Votes,
                                                       ifelse(Cand_5_Surname  == "MACRON", Cand_5_Votes,
                                                              ifelse(Cand_6_Surname  == "MACRON", Cand_6_Votes, 
                                                                     ifelse(Cand_7_Surname  == "MACRON", Cand_7_Votes,
                                                                            ifelse(Cand_8_Surname  == "MACRON", Cand_8_Votes,
                                                                                   ifelse(Cand_9_Surname  == "MACRON", Cand_9_Votes,
                                                                                          ifelse(Cand_10_Surname == "MACRON", Cand_10_Votes,
                                                                                                 ifelse(Cand_11_Surname == "MACRON", Cand_11_Votes,
                                                                                                        NA))))))))))),
         VOTES_FILLON_T1 = ifelse(Cand_1_Surname  == "FILLON", Cand_1_Votes,
                                  ifelse(Cand_2_Surname  == "FILLON", Cand_2_Votes,
                                         ifelse(Cand_3_Surname  == "FILLON", Cand_3_Votes, 
                                                ifelse(Cand_4_Surname  == "FILLON", Cand_4_Votes,
                                                       ifelse(Cand_5_Surname  == "FILLON", Cand_5_Votes,
                                                              ifelse(Cand_6_Surname  == "FILLON", Cand_6_Votes, 
                                                                     ifelse(Cand_7_Surname  == "FILLON", Cand_7_Votes,
                                                                            ifelse(Cand_8_Surname  == "FILLON", Cand_8_Votes,
                                                                                   ifelse(Cand_9_Surname  == "FILLON", Cand_9_Votes,
                                                                                          ifelse(Cand_10_Surname == "FILLON", Cand_10_Votes,
                                                                                                 ifelse(Cand_11_Surname == "FILLON", Cand_11_Votes,
                                                                                                        NA))))))))))))

  ###
  # Select important columns and merge to subset to commune data
  FRCommunes <- merge(FRCommunes, FRPresTour1 %>% select(DeptCode, 
                                                         CommuneCode, 
                                                         Votes_Expressed_T1, 
                                                         VOTES_LEPEN_T1, 
                                                         VOTES_MACRON_T1,
                                                         VOTES_FILLON_T1), 
                      by = c("DeptCode", "CommuneCode"))

# Back to departments
# Then we can simply do a group by and summarize on the VOTES_LEPEN and VOTES_MACRON

# Reduce to a dataset at the Department level, creating variable labels with
# T1 for Tour 1
FRPresTour1 <- FRPresTour1 %>% 
  group_by(DeptCode) %>% 
  summarize(DeptName = first(DeptName),
            Votes_Expressed_DeptTotal_T1 = first(Votes_Expressed_DeptTotal),
            Votes_Expressed_DeptTotal_T1_LEPEN = sum(VOTES_LEPEN_T1),
            Votes_Expressed_DeptTotal_T1_MACRON = sum(VOTES_MACRON_T1),
            Votes_Expressed_DeptTotal_T1_FILLON = sum(VOTES_FILLON_T1)) %>% 
  mutate(Votes_Expressed_DeptPct_T1_LEPEN = Votes_Expressed_DeptTotal_T1_LEPEN/Votes_Expressed_DeptTotal_T1,
         Votes_Expressed_DeptPct_T1_MACRON = Votes_Expressed_DeptTotal_T1_MACRON/Votes_Expressed_DeptTotal_T1,
         Votes_Expressed_DeptPct_T1_FILLON = Votes_Expressed_DeptTotal_T1_FILLON/Votes_Expressed_DeptTotal_T1)

# We see these match *Rappel des r?sultats du d?partement au 1er tour - LE PEN and MACRON
# Ain: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/001/index.html
# Aisne: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/032/002/index.html
# Allier: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/003/index.html
# Alpes-de-Haute-Provence: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/093/004/index.html

### Process Wave 2 dataset second

FRPresTour2 <-
  readxl::read_xls(
    "./data/raw/FR_Elections/Presidentielle_2017_Resultats_Communes_Tour_2_c.xls",
    skip = 3
  )

Tour2ColNames <- c(
  "DeptCode",
  "DeptName",
  "CommuneCode",
  "CommuneName",
  "Votes_Registered",
  "Votes_Abstention",
  "Votes_Abstentions_pct",
  "Votes_Cast",
  "Votes_Turnout_pct",
  "Votes_Blank",
  "Votes_Blank_Registered_pct",
  "Votes_Blank_Cast_pct",
  "Votes_Nulls",
  "Votes_Null_Registered_pct",
  "Votes_Null_Cast_pct",
  "Votes_Expressed_T2",
  "Votes_Expressed_Registered_pct",
  "Votes_Expressed_Cast_pct",
  "CandidateID1",
  "Cand_1_Sex",
  "Cand_1_Surname",
  "Cand_1_Firstname",
  "Cand_1_Votes",
  "Cand_1_Votes_Registered_pct",
  "Cand_1_Votes_Expressed_pct",
  "CandidateID2",
  "Cand_2_Sex",
  "Cand_2_Surname",
  "Cand_2_Firstname",
  "Cand_2_Votes",
  "Cand_2_Votes_Registered_pct",
  "Cand_2_Votes_Expressed_pct"
)

colnames(FRPresTour2) <- Tour2ColNames

# Votes_Expressed is the sum of votes for each candidate and the blank votes

# First calculate total Votes_Expressed per commune
FRPresTour2 <- FRPresTour2 %>% 
  group_by(DeptCode) %>% 
  mutate(Votes_Expressed_DeptTotal = sum(Votes_Expressed_T2)) 

# We see this matches *R?sultats du d?partement au 2d tour  - Exprim?s
# from https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/001/index.html

# Now we need to calculate total Votes_expressed per candidate per commune

# Use the same approach as before
FRPresTour2 <- FRPresTour2 %>% 
  mutate(VOTES_LEPEN_T2 = ifelse(Cand_1_Surname  == "LE PEN", Cand_1_Votes,
                                 ifelse(Cand_2_Surname  == "LE PEN", Cand_2_Votes, NA)),
         VOTES_MACRON_T2 = ifelse(Cand_1_Surname == "MACRON", Cand_1_Votes,
                                  ifelse(Cand_2_Surname == "MACRON", Cand_2_Votes, NA)))
  
  ###
  # Select important columns and merge to subset to commune data
  FRCommunes <- merge(FRCommunes, FRPresTour2 %>% select(DeptCode, 
                                                         CommuneCode, 
                                                         Votes_Expressed_T2, 
                                                         VOTES_LEPEN_T2, 
                                                         VOTES_MACRON_T2), 
                      by = c("DeptCode", "CommuneCode"))

# Back to departments...
# Then we can simply do a group by and summarize on the VOTES_LEPEN and VOTES_MACRON

# Reduce to a dataset at the Department level, creating variable labels with
# T1 for Tour 1
FRPresTour2 <- FRPresTour2 %>% 
  group_by(DeptCode) %>% 
  summarize(DeptName = first(DeptName),
            Votes_Expressed_DeptTotal_T2 = first(Votes_Expressed_DeptTotal),
            Votes_Expressed_DeptTotal_T2_LEPEN = sum(VOTES_LEPEN_T2),
            Votes_Expressed_DeptTotal_T2_MACRON = sum(VOTES_MACRON_T2)) %>% 
  mutate(Votes_Expressed_DeptPct_T2_LEPEN = Votes_Expressed_DeptTotal_T2_LEPEN/Votes_Expressed_DeptTotal_T2,
         Votes_Expressed_DeptPct_T2_MACRON = Votes_Expressed_DeptTotal_T2_MACRON/Votes_Expressed_DeptTotal_T2)

# We see these match *R?sultats du d?partement au 2d tour  - LE PEN and MACRON
# Ain: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/001/index.html
# Aisne: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/032/002/index.html
# Allier: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/084/003/index.html
# Alpes-de-Haute-Provence: https://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__presidentielle-2017/(path)/presidentielle-2017/093/004/index.html

# Carry out a few final transformations on the commune level data
FRCommunes <- FRCommunes %>% 
  mutate(
    Votes_Expressed_CommPct_T1_LEPEN = VOTES_LEPEN_T1/Votes_Expressed_T1,
    Votes_Expressed_CommPct_T1_MACRON = VOTES_MACRON_T1/Votes_Expressed_T1,
    Votes_Expressed_CommPct_T1_FILLON = VOTES_FILLON_T1/Votes_Expressed_T1,
    Votes_Expressed_CommPct_T2_LEPEN = VOTES_LEPEN_T2/Votes_Expressed_T2,
    Votes_Expressed_CommPct_T2_MACRON = VOTES_MACRON_T2/Votes_Expressed_T2
  )

# Merge to ongoing department-level datasets

# Merge data
FRDepartments <-
  merge(FRDepartments, 
        FRPresTour1, 
        by = "DeptCode", 
        all.x = T)

FRDepartments <-
  merge(FRDepartments, 
        FRPresTour2, 
        by = "DeptCode", 
        all.x = T)

### Save dataset as it stands now ----

# Already saved - second file is 0.2GB so can take some time to upload - caution

## Save as geopackage to avoid truncation of variable names

# Departments
#st_write(FRDepartments,
#         "./data/final/FRDepartments_outcomes_exposures.gpkg",
#         delete_layer = TRUE)

# Communes
#st_write(FRCommunes,
#         "./data/final/FRCommunes_outcomes_exposures.gpkg",
#         delete_layer = TRUE)
