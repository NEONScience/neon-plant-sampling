### VST QC using neonPlants estimateWoodProd outputs ####
library(tidyverse)
library(neonUtilities)
library(broom)

setwd("~/Documents/gitRepositories/neonPlants")
devtools::load_all()
setwd("~/Documents/gitRepositories/neon-plant-sampling")

source("vst_QC_neonPlants/predictStemDiameter.R")

##  Process steps in predictStemDiameter function:
#--> Get 'flagged' table from estimateWoodProd; has all records for an individualID if any record is incrementFlag == TRUE
#--> Filter "flagged" individualIDs further to those that are "live" the whole time, or that experience mortality.
#--> Identify anomalous stemDiameter values relative to lead/lag values
#--> Model 'predictedStemDiam'



### Productivity QC testing: Run all data through estimateWoodProd function ####
#--> Note: Phase 1 -- output from "duplicates" table is saved for resolution. As many as possible to be corrected by 2026-11-20.
#--> Note: Phase 2 -- output in "flagged" table is corrected when possible using predictStemDiameter function. All "corrDXXSITE" files bundled as final step and written out as single file.
#--> Note: Phase 2 -- individualIDs with > 1 stemDiameter anomaly have only one addressed via this correction process.
#--> Note: Phase 2 -- A number of flagged diameter increments cannot be corrected due to n=2 observations per individualID. Need to re-run this code annually



### D01 prod output checks ######################################################################################
vstD01 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BART", "HARV"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  BART site
theSite <- "BART"
tempPP <- vstD01$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD01$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD01$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D01{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD01BART <- predictStemDiameter(flaggedDF)


##  HARV site
theSite <- "HARV"
tempPP <- vstD01$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD01$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD01$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D01{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD01HARV <- predictStemDiameter(flaggedDF)



### D02 prod output checks ######################################################################################
vstD02 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BLAN", "SCBI", "SERC"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  BLAN site
theSite <- "BLAN"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD02BLAN <- predictStemDiameter(flaggedDF)


##  SCBI site
theSite <- "SCBI"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Check flagged output for corrections
flaggedDF <- prodOut$vst_ANPP_flagged
corrD02SCBI <- predictStemDiameter(flaggedDF)


##  SERC site
theSite <- "SERC"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Check flagged output for corrections
flaggedDF <- prodOut$vst_ANPP_flagged
corrD02SERC <- predictStemDiameter(flaggedDF)



### D03 prod output checks ######################################################################################
vstD03 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("DSNY", "JERC", "OSBS"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  DSNY site
theSite <- "DSNY"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "distributed")

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")
#--> No tree duplicates

#   Check flagged output for corrections
flaggedDF <- prodOut$vst_ANPP_flagged
#--> Nothing flagged


##  JERC site
theSite <- "JERC"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Check flagged output for corrections
flaggedDF <- prodOut$vst_ANPP_flagged
corrD03JERC <- predictStemDiameter(flaggedDF)


##  OSBS site
theSite <- "OSBS"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Check flagged output for corrections
flaggedDF <- prodOut$vst_ANPP_flagged
corrD03OSBS <- predictStemDiameter(flaggedDF)



### D04 prod output checks ######################################################################################
vstD04 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GUAN", "LAJA"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  GUAN site
theSite <- "GUAN"
tempPP <- vstD04$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD04$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD04$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D04{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD04GUAN <- predictStemDiameter(flaggedDF)


##  LAJA site
theSite <- "LAJA"
tempPP <- vstD04$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD04$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD04$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "distributed")

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D04{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")
#--> No tree dupes

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD04LAJA <- predictStemDiameter(flaggedDF)



###  D05 prod output checks ######################################################################################
vstD05 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("STEI", "TREE", "UNDE"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  STEI site
theSite <- "STEI"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD05STEI <- predictStemDiameter(flaggedDF)


##  TREE site
theSite <- "TREE"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD05TREE <- predictStemDiameter(flaggedDF)


##  UNDE site
theSite <- "UNDE"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD05UNDE <- predictStemDiameter(flaggedDF)



### D06 prod output checks ######################################################################################
vstD06 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("KONA", "KONZ", "UKFS"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  KONA site
theSite <- "KONA"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error, no data


##  KONZ site
theSite <- "KONZ"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "towerAll")

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D06{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD06KONZ <- predictStemDiameter(flaggedDF)


##  UKFS site
theSite <- "UKFS"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D06{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD06UKFS <- predictStemDiameter(flaggedDF)



### D07 prod output checks ######################################################################################
vstD07 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GRSM", "MLBS", "ORNL"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  GRSM site
theSite <- "GRSM"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD07GRSM <- predictStemDiameter(flaggedDF)



##  MLBS site
theSite <- "MLBS"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD07MLBS <- predictStemDiameter(flaggedDF)


##  ORNL site
theSite <- "ORNL"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD07ORNL <- predictStemDiameter(flaggedDF)



### D08 prod output checks ######################################################################################
vstD08 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("DELA", "LENO", "TALL"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  DELA site
theSite <- "DELA"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD08DELA <- predictStemDiameter(flaggedDF)


##  LENO site
theSite <- "LENO"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD08LENO <- predictStemDiameter(flaggedDF)


##  TALL site
theSite <- "TALL"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD08TALL <- predictStemDiameter(flaggedDF)



### D10 prod output checks ######################################################################################
vstD10 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CPER", "RMNP", "STER"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  CPER site
theSite <- "CPER"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected NULL output with message "No tree biomass found"


##  RMNP site
theSite <- "RMNP"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D10{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD10RMNP <- predictStemDiameter(flaggedDF)


##  STER site
theSite <- "STER"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error: Table vst_perplotperyear has no data



### D11 prod output checks ######################################################################################
vstD11 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CLBJ", "OAES"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  CLBJ site
theSite <- "CLBJ"
tempPP <- vstD11$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD11$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD11$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D11{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD11CLBJ <- predictStemDiameter(flaggedDF)


##  OAES site
theSite <- "OAES"
tempPP <- vstD11$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD11$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD11$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error: Table 'vst_perplotperyear' has no data



### D12 prod output checks ######################################################################################
vstD12 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = "YELL",
                                       release = "LATEST",
                                       check.size = FALSE)

#   YELL site
theSite <- "YELL"
tempPP <- vstD12$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD12$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD12$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D12{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD12YELL <- predictStemDiameter(flaggedDF)



### D13 prod output checks ######################################################################################
vstD13 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("MOAB", "NIWO"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  MOAB site
theSite <- "MOAB"
tempPP <- vstD13$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD13$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD13$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D13{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
#--> No flagged records


##  NIWO site
theSite <- "NIWO"
tempPP <- vstD13$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD13$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD13$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D13{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD13NIWO <- predictStemDiameter(flaggedDF)



### D14 prod output checks ######################################################################################
vstD14 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("JORN", "SRER"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  JORN site
theSite <- "JORN"
tempPP <- vstD14$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD14$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD14$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> No tree biomass found


##  SRER site
theSite <- "SRER"
tempPP <- vstD14$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD14$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD14$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> No tree biomass found

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D14{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D15 prod output checks ######################################################################################
vstD15 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ONAQ"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  ONAQ site
theSite <- "ONAQ"
tempPP <- vstD15$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD15$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD15$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> No tree dupes

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD15ONAQ <- predictStemDiameter(flaggedDF)



### D16 prod output checks ######################################################################################
vstD16 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ABBY", "WREF"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  ABBY site
theSite <- "ABBY"
tempPP <- vstD16$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD16$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD16$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D16{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD16ABBY <- predictStemDiameter(flaggedDF)


##  WREF site
theSite <- "WREF"
tempPP <- vstD16$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD16$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD16$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D16{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD16WREF <- predictStemDiameter(flaggedDF)



### D17 prod output checks ######################################################################################
vstD17 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("SJER", "SOAP", "TEAK"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  SJER site
theSite <- "SJER"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD17SJER <- predictStemDiameter(flaggedDF)


##  SOAP site
theSite <- "SOAP"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD17SOAP <- predictStemDiameter(flaggedDF)


##  TEAK site
theSite <- "TEAK"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "towerAll")
#--> Not enough data for Distributed plots

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD17TEAK <- predictStemDiameter(flaggedDF)



### D19 prod output checks ######################################################################################
vstD19 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BONA", "DEJU", "HEAL"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  BONA site
theSite <- "BONA"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD19BONA <- predictStemDiameter(flaggedDF)


##  DEJU site
theSite <- "DEJU"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD19DEJU <- predictStemDiameter(flaggedDF)


##  HEAL site
theSite <- "HEAL"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
#--> Zero flagged records



### D20 prod output checks ######################################################################################
vstD20 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("PUUM"),
                                       release = "LATEST",
                                       check.size = FALSE)

##  PUUM site
theSite <- "PUUM"
tempPP <- vstD20$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD20$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD20$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected tree fern message generated

#   Write out dupes identified by estimateWoodProd
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D20{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Identify and correct flagged stemDiameters from estimateWoodProd
flaggedDF <- prodOut$vst_ANPP_flagged
corrD20PUUM <- predictStemDiameter(flaggedDF)



### Collate and write output ###################################################################################
##  Create list of data frames beginning with "corr"
corr_dfs <- mget(ls(pattern = "^corr", envir = .GlobalEnv),
                 envir = .GlobalEnv)

corrDiamDF <- dplyr::bind_rows(corr_dfs)

write.csv(corrDiamDF, file = "vst_QC_correctedStemDiam.csv", row.names = FALSE, fileEncoding = "UTF-8")



