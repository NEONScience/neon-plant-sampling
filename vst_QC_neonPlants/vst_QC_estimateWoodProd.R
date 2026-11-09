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
#--> Note: Phase 2 -- output in "flagged" table is corrected when possible using predictStemDiameter function.



### D01 prod output checks ####
vstD01 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BART", "HARV"),
                                       release = "LATEST",
                                       check.size = FALSE)

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
                                       check.size = FALSE)

##  BLAN site
theSite <- "BLAN"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")


##  SCBI site
theSite <- "SCBI"
tempPP <- vstD02$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD02$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD02$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

#   Cache duplicate outputs
write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D02{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   Check flagged output for corrections
predictOut <- predictStemDiameter(flaggedData = prodOut$vst_ANPP_flagged)

test <- flaggedDF %>% dplyr::filter(!individualID %in% predictOut$individualID)


##  SERC site
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
                                       check.size = FALSE)

#   DSNY site
theSite <- "DSNY"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "distributed")

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#--> No tree duplicates

#   JERC site
theSite <- "JERC"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   OSBS site
theSite <- "OSBS"
tempPP <- vstD03$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD03$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD03$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D03{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D04 prod output checks ####
vstD04 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GUAN", "LAJA"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   GUAN site
theSite <- "GUAN"
tempPP <- vstD04$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD04$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD04$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D04{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   LAJA site
theSite <- "LAJA"
tempPP <- vstD04$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD04$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD04$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "distributed")

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D04{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")
#--> No tree dupes



###  D05 prod output checks ####
vstD05 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("STEI", "TREE", "UNDE"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   STEI site
theSite <- "STEI"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   TREE site
theSite <- "TREE"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   UNDE site
theSite <- "UNDE"
tempPP <- vstD05$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD05$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD05$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D05{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D06 prod output checks ####
vstD06 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("KONA", "KONZ", "UKFS"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   KONA site
theSite <- "KONA"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error, no data

#   KONZ site
theSite <- "KONZ"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "towerAll")

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D06{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   UKFS site
theSite <- "UKFS"
tempPP <- vstD06$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD06$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD06$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D06{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D07 prod output checks ####
vstD07 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("GRSM", "MLBS", "ORNL"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   GRSM site
theSite <- "GRSM"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")


#   MLBS site
theSite <- "MLBS"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   ORNL site
theSite <- "ORNL"
tempPP <- vstD07$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD07$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD07$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D07{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D08 prod output checks ####
vstD08 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("DELA", "LENO", "TALL"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   DELA site
theSite <- "DELA"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")


#   LENO site
theSite <- "LENO"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   TALL site
theSite <- "TALL"
tempPP <- vstD08$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD08$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD08$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D08{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D10 prod output checks ####
vstD10 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CPER", "RMNP", "STER"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   CPER site
theSite <- "CPER"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected NULL output with message "No tree biomass found"

#   RMNP site
theSite <- "RMNP"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D10{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   STER site
theSite <- "STER"
tempPP <- vstD10$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD10$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD10$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error: Table vst_perplotperyear has no data



### D11 prod output checks ####
vstD11 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("CLBJ", "OAES"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   CLBJ site
theSite <- "CLBJ"
tempPP <- vstD11$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD11$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD11$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D11{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   OAES site
theSite <- "OAES"
tempPP <- vstD11$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD11$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD11$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected error: Table 'vst_perplotperyear' has no data



### D12 prod output checks ####
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

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D12{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D13 prod output checks ####
vstD13 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("MOAB", "NIWO"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   MOAB site
theSite <- "MOAB"
tempPP <- vstD13$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD13$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD13$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D13{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   NIWO site
theSite <- "NIWO"
tempPP <- vstD13$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD13$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD13$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D13{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D14 prod output checks ####
vstD14 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("JORN", "SRER"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   JORN site
theSite <- "JORN"
tempPP <- vstD14$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD14$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD14$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> No tree biomass found

#   SRER site
theSite <- "SRER"
tempPP <- vstD14$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD14$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD14$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D14{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D15 prod output checks ####
vstD15 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ONAQ"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   ONAQ site
theSite <- "ONAQ"
tempPP <- vstD15$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD15$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD15$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> No tree dupes



### D16 prod output checks ####
vstD16 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("ABBY", "WREF"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   ABBY site
theSite <- "ABBY"
tempPP <- vstD16$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD16$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD16$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D16{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   WREF site
theSite <- "WREF"
tempPP <- vstD16$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD16$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD16$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D16{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D17 prod output checks ####
vstD17 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("SJER", "SOAP", "TEAK"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   SJER site
theSite <- "SJER"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   SOAP site
theSite <- "SOAP"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   TEAK site
theSite <- "TEAK"
tempPP <- vstD17$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD17$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD17$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput,
                                        plotSubset = "towerAll")
#--> Not enough data for Distributed plots

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D17{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D19 prod output checks ####
vstD19 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("BONA", "DEJU", "HEAL"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   BONA site
theSite <- "BONA"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   DEJU site
theSite <- "DEJU"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

#   HEAL site
theSite <- "HEAL"
tempPP <- vstD19$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD19$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD19$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D19{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")



### D20 prod output checks ####
vstD20 <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                       site = c("PUUM"),
                                       release = "LATEST",
                                       check.size = FALSE)

#   PUUM site
theSite <- "PUUM"
tempPP <- vstD20$vst_perplotperyear %>% dplyr::filter(siteID == theSite)
tempMap <- vstD20$vst_mappingandtagging %>% dplyr::filter(siteID == theSite)
tempAI <- vstD20$vst_apparentindividual %>% dplyr::filter(siteID == theSite)
tempInput <- list(vst_perplotperyear = tempPP, vst_mappingandtagging = tempMap, vst_apparentindividual = tempAI)

prodOut <- neonPlants::estimateWoodProd(inputDataList = tempInput)
#--> Expected tree fern message generated

write.csv(prodOut$vst_ANPP_duplicates, file = glue::glue("~/Desktop/vst_ANPP_treedupes_D20{theSite}.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

