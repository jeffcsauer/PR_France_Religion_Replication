# Replication materials for *Dieu dans la commune Religion and Voting in the 2017 French Election* by Overos and Sauer, 2023

This repository contains all materials needed to replicate the findings presented in 'Dieu dans la commune Religion and Voting in the 2017 French Election' published in the peer-reviewed journal *Politics and Religion* in 2023.

# Abstract

European political parties, particularly radical right parties (RRPs), increasingly use religious symbols during elections. Despite the prevalence of these symbols, evidence on the association between religion and far-right vote share is mixed. We compare two leading arguments explaining the relationship between religion and RRPs. We hypothesize that the number of religious buildings, identifiable as Islamic or Catholic, will be associated with higher RRP support. We test this as a most likely crucial case using results from the French 2017 presidential election. Controlling for other demographic factors, more Catholic buildings in a commune are associated with a decrease in votes for the Front Nationale (FN). An increase in the number of mosques in non-urban communes is associated with increased support for FN. We argue these findings are evidence that RRPs use religious symbolism to draw on nativist or anti-Islamic support rather than traditional religious support.

# Analysis and file explanation

In order to facilitate clear replication of results we provide a general overview of the analysis, a list of all datasets required to replicate the analysis, an indication as to where each figure in the paper originates, and a few considerations for full replication (e.g., those who wish to rescrape websites).

To begin, the high-level overview of the analysis is as follows:

1. Execution of a webscraping script on different websites to obtain text addresses of religious buildings across France
2. Geocoding of the addresses and aggregating counts of the buildings to different levels of French geography (e.g., Department, Commune)
3. Processing of French Government Election results for 2017
4. Processing of French Census (INSEE) information for 2017 and the surrounding years
5. Combining the geocoded addresses, election results, and census information into a single analysis ready dataset
5. Executing the main analysis on the combined analysis ready dataset
6. Executing supplementary analysis on subsets of the combined analysis ready dataset

These steps were executed sequentially. We now proceed with a description of the replication materials folder structure before moving on to more detailed explanations of each file.

```
replication_materials
│   README.md
│
└───code
│   │   1_FR_DEPARTMENT.R
│   │   2_FR_INSEE.R
|   |   3_FR_ANALYSIS_MAIN.R
|   |   4_FR_ANALYSIS_PERCAPITA_SUPP.R
|   |   Scraping_Catholic_Directory.ipynb
|   |   Scraping_Trouvetamosquee.ipynb
│   
└───data
│   └───final                                          
|       |   trouvetamosquee_geocoded.csv
|       |   CatholicDirectoryScrape_geocoded.csv
|       |   FRCommunes_analysis.gpkg
|       |   FRCommunes_outcomes_exposures.gpkg
|       |   FRDepartments_outcomes_exposures.gpkg
│   └───raw                                            
|       └───FR_Boundaries
│       │   ...
|       └───FR_Census
│       │   ...
|       └───FR_Elections
│       │   ...
|       └───ZUS
│       │   ...
|       └───CatholicDirectory
│       │   ...
|       └───Mosques
│       │   ...
```

Below we provide more detailed explanations of each file presented in the replication materials. **Note: if the user simply wishes to recreate the results presented in the paper, they need only execute `1_FR_DEPARTMENT.R` (for Figure 1) and `3_FR_ANALYSIS_MAIN.R` (for all other Figures, Tables, and results).** We provide *all* replication materials to facilitate as much transparency as possible.

In addition, please note that each `R` script begins with a few lines of code that are meant to **set the correct working directory**. Users should download the replication materials folder, unzip to the desired location, and then set that location as the working directory of the `R` script before proceeding. 

CODE 

- `1_FR_DEPARTMENT.R`: A data processing script of the paper. This script intakes the French Department and Commune boundaries, geocodes the religious buildings datasets, processes the French voting datasets, and combines all of these sources into an intermediate dataset. Also produces Figure 1. 
- `2_FR_INSEE.R`: A data processing script of the paper. This script intakes French INSEE (Census) data to calculate the variables elaborated upon in the paper (e.g., sociodemographic measures). This produces a another intermediate dataset that, when combined with the results of the previous script, produces the main analysis dataset. Does not produce any Figure or Table in the paper.
- `3_FR_ANALYSIS_MAIN.R`: The **main** analysis script of the paper.  Conducts main regression analysis and supplementary analyses found in the paper and those requested by the reviewers. Produces Table 1, Table 2, Table 3, Table 4, Table 5, Figure 2, Figure 3, and Figure 4.
- `4_FR_ANALYSIS_PERCAPITA_SUPP.R`: Extra analysis script where we considered the variables on a per capita basis. In the paper we provide an argument as to why this is undesirable. No figures or major data produced from this script.
- `Scraping_Catholic_Directory.ipynb`: interactive webscraping notebook for https://www.catholicdirectory.com/. Produces a dataset that is fed into `1_FR_DEPARTMENT.R`. Note that this website was last scraped in November of 2021, so rescraping may produce different results. We provide the results of the scrape in the replication materials.  
- `Scraping_Trouvetamosquee.ipynb`: interactive webscraping notebook for https://www.trouvetamosquee.fr/. Produces a dataset that is fed into `1_FR_DEPARTMENT.R`. Note that this website was last scraped in November of 2021, so rescraping may produce different results. We provide the results of the scrape in the replication materials.

DATASETS

We provide both the 'final' (e.g., cleaned and ready for analysis) as well as raw (e.g., untouched, directly from source) datasets used in the analysis. We provide a more detailed description of the 'final' datasets as these are the minimum files needed to replicate the analysis. A thorough description of all the data sources - and their nuances - can be found in the paper and analysis scripts.

_Note: some of the below files can be quite large. Some have been compressed and split to facilitate uploading to Github. These files are indicated with an asterisk (*). A link to the full, zipped files is also available [here](https://drive.google.com/file/d/1LM1Fg-78ZHJzzkfEd73ZIfj5EX75PBCN/view?usp=drive_web)._

DATASETS - Final

- `CatholicDirectoryScrape_geocoded.csv`: geocoded locations of Catholic buildings. Produced from `1_FR_DEPARTMENT.R` and used for Department- and Commune-level counts of Catholic buildings. 
- `trouvetamosquee_geocoded.csv`: geocoded locations of Islamic buildings (primarily mosques). Produced from `1_FR_DEPARTMENT.R` and used for Department- and Commune-level counts of Islamic buildings. 
- `FRCommunes_outcomes_exposures.gpkg`*: an intermediary dataset that is a byproduct of data processing. Produced from `1_FR_DEPARTMENT.R`. Fed into `2_FR_INSEE.R` to ultimately result in `FRCommunes_analysis.gpkg`, the main dataset used in the analysis.
- `FRCommunes_analysis.gpkg`*: the **main** dataset used to produce the results presented in the paper. Contains Department- and Commune-level information on French voting outcomes in 2017, sociodemographic measures, and the number of religious buildings. Variable names are descriptive and efforts have been made to remove all unnecessary variables.

DATASETS - Raw

The files presented in the `raw` folder may be numerous as these are the raw, unedited files acquired from various governmental and official sources.While each file is not described, we provide a broad description of the data's provenance and explanation as to how it is used. 

- ZUS: an excel file describing nomenclature and identification information for the sensitivie urban zones. List made available by the french government from: https://sig.ville.gouv.fr/atlas/ZUS
- FR_Boundaries: generic shapefiles representing French Department and Commune boundaries. We consider both the sources of the Database of Global Administrative Areas (GADM) and Friendly et al. 2020 for the widely known Guerry 1833 data.   
- FR_Census*: various excel files corresponding to French INSEE (Census) information obtained from: https://www.insee.fr/en/accueil. Note that each sub-folder (e.g., BTX_TD_FOR2_2017) corresponds to a table and year. Only the _emploi_ file is zipped.
- FR_Elections: various excel files corresponding to French Government election results obtained for Wave 1 from https://www.data.gouv.fr/en/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-communes/ and Wave 2 from https://www.data.gouv.fr/en/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-2nd-tour/.
- CatholicDirectory: contains the raw output of the webscraping effort. Contains names and addresses of Catholic bulidings, but no geographic (e.g,. latitute longitude point coordinates) that can be associated with Department or Commune-level information.
- Mosques: contains the raw output of the webscraping effort. Contains names and addresses of Islamic bulidings, but no geographic (e.g,. latitute longitude point coordinates) that can be associated with Department or Commune-level information.

# Author team and contact information

For any questions on the replication, please contact both authors.

1. Henry Overos (hoveros@terpmail.umd.edu)
2. Jeffery Sauer (jcsauer@terpmail.umd.edu)

*Last updated: 13 July 2023*

Please note that the replication materials will also be available in full on the author's [Github](https://github.com/jeffcsauer/PR_France_Religion_Replication). 
