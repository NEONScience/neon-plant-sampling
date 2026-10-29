### VST implausible stemDiameter increment check ####
library(tidyverse)
library(neonUtilities)
library(broom)
devtools::load_all() #--> setwd() to neonPlants

source("vst_implausibleIncrement/predictStemDiameter.R")

##  Process steps in predictStemDiameter function:
#--> Get 'flagged' table from estimateWoodProd; has all records for an individualID if any record is incrementFlag == TRUE
#--> Filter "flagged" individualIDs further to those that are "live" the whole time, or that experience mortality.
#--> Identify anomalous stemDiameter values relative to lead/lag values
#--> Model 'predictedStemDiam'



### Get D16 ABBY test dataset
vst <- neonUtilities::loadByProduct(dpID = "DP1.10098.001",
                                    site = "ABBY",
                                    release = "LATEST",
                                    check.size = FALSE)


##  Generate wood prod outputs
prodOut <- neonPlants::estimateWoodProd(inputDataList = vst)
tempFlagged <- prodOut$vst_ANPP_flagged


##  Process ABBY flagged data
corrStemDiam <- predictStemDiameter(flaggedData = tempFlagged)
#--> Seems to work as expected



