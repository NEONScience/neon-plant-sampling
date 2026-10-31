### VST QC using neonPlants estimateWoodProd outputs ####
library(tidyverse)
library(neonUtilities)
library(broom)
devtools::load_all() #--> setwd() to neonPlants

source("vst_neonPlantsQC/predictStemDiameter.R")

##  Process steps in predictStemDiameter function:
#--> Get 'flagged' table from estimateWoodProd; has all records for an individualID if any record is incrementFlag == TRUE
#--> Filter "flagged" individualIDs further to those that are "live" the whole time, or that experience mortality.
#--> Identify anomalous stemDiameter values relative to lead/lag values
#--> Model 'predictedStemDiam'



### Productivity QC testing: Run all data through estimateWoodProd function ####
#--> Note: Phase 1 -- output from "duplicates" table is saved for resolution. As many as possible to be corrected by 2026-11-20.
#--> Note: Phase 2 -- output in "flagged" table is corrected when possible using predictStemDiameter function.



### D01 prod output checks ####
vstD01 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BART", "HARV"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

#   BART site
theSite <- "BART"
tempPP <- vstD01$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD01$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD01$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D01{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   HARV site
theSite <- "HARV"
tempPP <- vstD01$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD01$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD01$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D01{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D02 prod output checks ####
vstD02 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BLAN", "SCBI", "SERC"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

#   BLAN site
theSite <- "BLAN"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   SCBI site
theSite <- "SCBI"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   SERC site
theSite <- "SERC"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D03 prod output checks ####
vstD03 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("DSNY", "JERC", "OSBS"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

#   DSNY site
theSite <- "DSNY"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   JERC site








### D04 output checks
vstD04 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GUAN", "LAJA"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

#   Wood mass check
massD04 <- neonPlants::estimateWoodMass(inputDataList = vstD04)
indiv <- massD04$vst_AGB_indiv
plot <- massD04$vst_AGB_plot
site <- massD04$vst_AGB_site
dupes <- massD04$vst_AGB_duplicates
downed <- massD04$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D04.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 1135 records, output accurate 
#--> Indiv: 793 records with "noAllometry" and almost all are cactus, yucca, small-palm.
#--> Plot-level records look plausible
#--> Site-level records look plausible
#--> Downed records look plausible



##  D05 wood mass output check
vstD05 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("STEI", "TREE", "UNDE"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD05 <- neonPlants::estimateWoodMass(inputDataList = vstD05)
indiv <- massD05$vst_AGB_indiv
plot <- massD05$vst_AGB_plot
site <- massD05$vst_AGB_site
dupes <- massD05$vst_AGB_duplicates
downed <- massD05$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D05.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 402 records, output accurate
#--> Indiv: 12,553 records with "noAllometry"; 12,506 are ferns; output in this table looks plausible
#--> Downed: Output accurate
#--> Plot-level: Output plausible
#--> Site-level: Output plausible


##  D06 wood mass output check
vstD06 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("KONA", "KONZ", "UKFS"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD06 <- neonPlants::estimateWoodMass(inputDataList = vstD06)
indiv <- massD06$vst_AGB_indiv
plot <- massD06$vst_AGB_plot
site <- massD06$vst_AGB_site
dupes <- massD06$vst_AGB_duplicates
downed <- massD06$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D06.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 161 records, output accurate
#--> Indiv: 949 records with "noAllometry"; 456 are Maclura pomifera (Osage Orange), 267 are Morus rubra (Red Mulberry). Output looks plausible but some problems in the data, e.g. for NEON.PLA.D06.KONZ.01250, NEON.PLA.D06.KONZ.01252
#--> Downed: Output accurate
#--> Plot-level: KONZ_046 has implausibly high biomass in 2016 --> driven by NEON.PLA.D06.KONZ.01138 sapling with reported basalStemDiameter = 20 cm, height 1.6 m; had basalStemDiametet = 1.6 cm and height 0.7 m in 2015, reported "dead" both years.
#--> Site-level: Big jump in biomass from 2021 to 2022 at KONZ, odd. Seems driven by KONZ_061; data issues in 'indiv' table (see above), but output logical given data issues.


##  D07 wood mass output check
vstD07 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GRSM", "MLBS", "ORNL"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD07 <- neonPlants::estimateWoodMass(inputDataList = vstD07)
indiv <- massD07$vst_AGB_indiv
plot <- massD07$vst_AGB_plot
site <- massD07$vst_AGB_site
dupes <- massD07$vst_AGB_duplicates
downed <- massD07$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D07.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 202 records, output accurate
#--> Indiv: --> 5894 records with "noAllometry"; 2027 are THNO (fern), 1978 are OSCI (fern); in total 4740 are ferns
#--> Downed: Looks reasonable
#--> Plot-level: Some plots show significant drops in biomass - GRSM_051, GRSM_062, beginning in 2019 (fire?); very high biomass for GRSM_011 and GRSM_012 --> looks legit, very big trees. 
#--> Site-level: Plausible output


##  D08 wood mass output check
vstD08 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("DELA", "LENO", "TALL"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD08 <- neonPlants::estimateWoodMass(inputDataList = vstD08)
indiv <- massD08$vst_AGB_indiv
plot <- massD08$vst_AGB_plot
site <- massD08$vst_AGB_site
dupes <- massD08$vst_AGB_duplicates
downed <- massD08$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D08.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 199 records, output appears accurate
#--> Indiv: 2332 records with "noAllometry"; 470 are Halesia diptera (Two-wing snowdrop tree), 406 are Triadica sebifera (Chinese Tallow), no ferns
#--> Downed: Looks reasonable
#--> Plot-level: Some odd changes in biomass, DELA_053 in 2021, LENO_011 in 2021; nothing systematically wrong with output
#--> Site-level: SD is pretty high for 2021, but to be expected given plot-level observations. Plausible output.


##  D10 wood mass output check
vstD10 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CPER", "RMNP", "STER"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD10 <- neonPlants::estimateWoodMass(inputDataList = vstD10)
indiv <- massD10$vst_AGB_indiv
plot <- massD10$vst_AGB_plot
site <- massD10$vst_AGB_site
dupes <- massD10$vst_AGB_duplicates
downed <- massD10$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D10.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 26 records, output appears accurate
#--> Indiv: 424 records with "noAllometry", all are cactus at CPER; output looks plausible
#--> Downed: Looks reasonable
#--> Plot-level: Some odd changes in biomass, RMNP_003 in 2025, RMNP_043 in 2024; nothing systematically wrong with output
#--> Site-level: Plausible output


##  D11 wood mass output check
vstD11 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CLBJ", "OAES"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD11 <- neonPlants::estimateWoodMass(inputDataList = vstD11)
indiv <- massD11$vst_AGB_indiv
plot <- massD11$vst_AGB_plot
site <- massD11$vst_AGB_site
dupes <- massD11$vst_AGB_duplicates
downed <- massD11$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D11.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 63 records, output accurate
#--> Indiv: 532 records with "noAllometry", 305 are Opuntia and Yucca; output looks plausible
#--> Downed: Looks reasonable
#--> Plot-level: CLBJ_006 transitions to 0 biomass in 2025, CLBJ_009 significantly reduced biomass from 2017 onward, CLBJ_016, 022, 024 same as 009; nothing systematically wrong with output
#--> Site-level: Plausible output


##  D12 wood mass output check
vstD12 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = "YELL",
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD12 <- neonPlants::estimateWoodMass(inputDataList = vstD12)
indiv <- massD12$vst_AGB_indiv
plot <- massD12$vst_AGB_plot
site <- massD12$vst_AGB_site
dupes <- massD12$vst_AGB_duplicates
downed <- massD12$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D12.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 72 records, output accurate
#--> Indiv: 46 records with "noAllometry", all are taxonID = 2PLANT, 15 of which are from 2024 --> how is this happening? Output looks plausible
#--> Downed: Looks reasonable
#--> Plot-level: Output looks plausible
#--> Site-level: Output looks plausible


##  D13 wood mass output check
vstD13 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("MOAB", "NIWO"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD13 <- neonPlants::estimateWoodMass(inputDataList = vstD13)
indiv <- massD13$vst_AGB_indiv
plot <- massD13$vst_AGB_plot
site <- massD13$vst_AGB_site
dupes <- massD13$vst_AGB_duplicates
downed <- massD13$vst_lost_downed

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D13.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 42 records, output accurate
#--> Indiv: 40 records with "noAllometry", 35 are Opuntia; output plausible
#--> Downed: Looks reasonable
#--> Plot-level: Some oddities, MOAB_001, 002, 004, 006, 009, 010, 012, 014, 015, 016, 017, 018 all go to 0 biomass in last event. Structurally, output looks fine.
#--> Site-level: Strange biomass so low in 2015. Structurally, output looks fine.


##  D14 wood mass output check
vstD14 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("JORN", "SRER"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD14 <- neonPlants::estimateWoodMass(inputDataList = vstD14)
indiv <- massD14$vst_AGB_indiv
plot <- massD14$vst_AGB_plot
site <- massD14$vst_AGB_site
dupes <- massD14$vst_AGB_duplicates
downed <- massD14$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D14.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 104 records, output accurate
#--> Indiv: 4982 records with "noAllometry"; nearly all are yucca or cactus; mass for some multi-stemmed ATCA2 seems awfully high, possible Conti allometry over-estimates due to high aggregate basalStemDiameter.
#--> Downed: Strange a number of yucca have no liveDeadStatus --> traces back to no plantStatus in nonWoody table
#--> Plot-level: Output plausible
#--> Site-level: Output plausible


##  D15 wood mass output check
vstD15 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ONAQ"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD15 <- neonPlants::estimateWoodMass(inputDataList = vstD15)
indiv <- massD15$vst_AGB_indiv
plot <- massD15$vst_AGB_plot
site <- massD15$vst_AGB_site
dupes <- massD15$vst_AGB_duplicates
downed <- massD15$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D15.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 14 records, output accurate
#--> Indiv: 222 records with "noAllometry"; all are Opuntia; output plausible
#--> Downed: Output plausible
#--> Plot-level: Output plausible
#--> Site-level: Very low plot numbers in 2014 and 2015, and biomass is 0 these two years which is unexpected.
# No AI or NW records on Portal for 2014 or 2015, but do mave PPPY for ONAQ_045 and 056 in 2014 and ONAQ_065 in 2015.
# Either need to find and publish AI and/or NW data from respective years or delete PPPY records.


##  D16 wood mass output check
vstD16 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ABBY", "WREF"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD16 <- neonPlants::estimateWoodMass(inputDataList = vstD16)
indiv <- massD16$vst_AGB_indiv
plot <- massD16$vst_AGB_plot
site <- massD16$vst_AGB_site
dupes <- massD16$vst_AGB_duplicates
downed <- massD16$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D16.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 120 records, output accurate
#--> Indiv: 1018 records with "noAllometry"; 159 are a single species of fern, 833 are Frangula purshiana (Cascara)
#--> Downed: Output plausible
#--> Plot-level: Some large swings in biomass for ABBY_007 (logging?), WREF_004, WREF_013; ABBY_067 very high in 2016 compared to other years (NEON.PLA.D16.ABBY.01003 with stemDiameter = 222 cm, diameter 111-138 in all subsequent years, unclear why it keeps "growing" if dead); ABBY_070 has hardly any biomass at all in 2016 but much higher in 2015 and all other years (only 29 individuals in 2016; 118 individuals in 2017, no changes in totalsampledareas). 
#--> Site-leveL: Output plausible given plot-level issues noted.


##  D17 wood mass output check
vstD17 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("SJER", "SOAP", "TEAK"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD17 <- neonPlants::estimateWoodMass(inputDataList = vstD17)
indiv <- massD17$vst_AGB_indiv
plot <- massD17$vst_AGB_plot
site <- massD17$vst_AGB_site
dupes <- massD17$vst_AGB_duplicates
downed <- massD17$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D17.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 100 records, output accurate
#--> Indiv: 220 records with "noAllometry", 56 are CEIN3 (Ceonothus) that are classified either as sap or smt (same taxonID gets a biomass when classified as a shrub); 46 are 2PLANT; a number of CELE2 with substantial mass (Ceonothus, 15-70 kg), no diameters, and large crowns. 
#--> Downed: Strange a lot of "lost" 2PLANT from 2016 at SJER (example: NEON.PLA.D17.SJER.00097 was a dead sap with no diameters in 2015 then "lost" 2PLANT in 2016, just delete both records?)
#--> Plot-level: Some large swings in biomass for SJER_009 in 2019; 060, 061 in 2025; TEAK_046 relatively low biomass in 2021, 2023.
#--> Site-level: Output plausible.


##  D19 wood mass output check
vstD19 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BONA", "DEJU", "HEAL"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD19 <- neonPlants::estimateWoodMass(inputDataList = vstD19)
indiv <- massD19$vst_AGB_indiv
plot <- massD19$vst_AGB_plot
site <- massD19$vst_AGB_site
dupes <- massD19$vst_AGB_duplicates
downed <- massD19$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D19.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 346 records, output accurate
#--> Indiv: 0 records with "noAllometry"; output plausible
#--> Downed: Output plausible
#--> Plot-level: BONA_021 large-ish loss of mass from 2017 to 2020, BONA_071 from 2016 to 2022; DEJU_053 oddly high in 2016; 2X fluctuations in HEAL_023, 059, 061, 068, 072 in 2016
#--> Site-level: Much higher AGB in 2021 than all other years by factor of 2-3X --> 3 plots with high biomass 014, 020, 025
# 014: Two Betula classified as single shrub getting high biomass estimate via Conti (~30 kg); a couple single-bole white spruce with 13-18 cm diameter.
# 020: A number of large Salix single shrubs that Conti estimates between 14-30 kg, could be overestimate.
# 025: A number of large single-bole trees
# Interestingly, all 3 of these plots last sampled in 2021, not in subsequent years.


##  D20 wood mass output check
vstD20 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("PUUM"),
                                       release = "LATEST",
                                       check.size = FALSE,
                                       token = Sys.getenv("NEON_TOKEN"))

massD20 <- neonPlants::estimateWoodMass(inputDataList = vstD20)
indiv <- massD20$vst_AGB_indiv
plot <- massD20$vst_AGB_plot
site <- massD20$vst_AGB_site
dupes <- massD20$vst_AGB_duplicates
downed <- massD20$vst_lost_downed

indivSummary <- indiv %>%
  dplyr::filter(source == "noAllometry") %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(count = n())

write.csv(dupes, file = "~/Desktop/vst_AGB_dupes_D20.csv", row.names = FALSE, fileEncoding = "UTF-8")

#--> Dupes: 24 records, output accurate
#--> Indiv: CIGL tree ferns from 2018 and 2019 have no stemLength --> "noAllometry" (385 records); 384 records are Dryopteris wallichiana (fern). Output plausible.
#--> Downed: Output plausible
#--> Plot-level: PUUM_010 large biomass reduction 2020->2025
#--> Site-level: Biomass lowest by a bit in 2022, not immediately clear why without looking at per plot stem counts through time; nothing systematically wrong with output.
