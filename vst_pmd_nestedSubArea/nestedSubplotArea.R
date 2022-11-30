### Purpose: Identify most recent nestedSubplotArea values by growthForm for all NEON plots ####
### Author: Courtney Meier - cmeier@BattelleEcology.org

library(dplyr)
library(neonUtilities)
source("~/Documents/workDocuments/gitRepositories/os-data-quality-review/vst/get.fulcrum.data.R")


### 2022-11-29: Identify past values for JERC_057 
pmdDF <- neonUtilities::loadByProduct(
  dpID = "DP1.10098.001",
  site = "JERC",
  tabl = "vst_perplotperyear",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

pmdDF <- pmdDF$vst_perplotperyear

nested_057 <- pmdDF %>%
  dplyr::filter(plotID == "JERC_057") %>%
  dplyr::select(plotID, date, eventID, nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana)



### Retrieve all NEON Portal vst_perplotperyear data for all sites
pmdDF <- neonUtilities::loadByProduct(
  dpID = "DP1.10098.001",
  site = "all",
  package = "basic",
  release = "current",
  tabl = "vst_perplotperyear",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

pmdDF <- pmdDF$vst_perplotperyear

nstPmdDF <- neonUtilities::loadByProduct(
  dpID = "DP1.10045.001",
  site = "all",
  package = "basic",
  release = "current",
  tabl = "vst_perplotperyear",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

nstPmdDF <- nstPmdDF$vst_perplotperyear

saveRDS(pmdDF, "vst_woody_pmdDF_20210819.RDS")
saveRDS(nstPmdDF, "vst_nonWoody_pmdDF_20210819.RDS")


### Determine most recently recorded value of nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana, and nestedSubplotAreaOther
### Note: Check values of nestedSubplotAreaOther=='noneSelected' to make sure most recent value for Tower plots comes from
### a bout where ALL Tower plots were measured.

#   Create reference table of record count by siteID and eventID and plotType --> useful to know when all Tower plots last measured
eventSummary <- pmdDF %>%
  dplyr::group_by(domainID, siteID, eventID, plotType) %>%
  dplyr::summarise(
    count = length(unique(plotID))
  )

saveRDS(eventSummary, file = "eventSummary.RDS")

nstSummary <- nstPmdDF %>%
  dplyr::group_by(domainID, siteID, eventID, plotType) %>%
  dplyr::summarise(
    count = length(unique(plotID))
  )

saveRDS(nstSummary, file = "nstSummary.RDS")


##  Woody data: Group pmdDF by domainID, siteID, and plotID and find most recent measurement date
#   Remove Sampling Impractical == anything other than 'OK'
filPmdDF <- pmdDF %>%
  dplyr::filter(!samplingImpractical %in% c("logistical", "extreme weather")) %>%
  dplyr::group_by(domainID, siteID, plotID) %>%
  dplyr::select(
    domainID,
    siteID,
    plotID,
    plotType,
    date,
    eventID,
    eventType,
    dataCollected,
    nestedSubplotAreaShrubSapling,
    nestedSubplotAreaLiana
  )


##  Non-woody data: Group pmdDF by domainID, siteID, and plotID and find most recent measurement date
tempDF <- nstPmdDF %>%
  dplyr::group_by(domainID, siteID, plotID) %>%
  dplyr::select(
    domainID,
    siteID,
    plotID,
    date,
    eventID,
    nestedSubplotAreaOther
  )


##  Join woody and non-woody data
allPmdDF <- filPmdDF %>%
  dplyr::full_join(tempDF, by = c("domainID", "siteID", "plotID", "date", "eventID")) %>%
  dplyr::ungroup() %>%
  dplyr::distinct()



### Bring in Fulcrum data that may not have transitioned to Portal (180 d load delay for PMD)
#   Define Fulcrum PMD query
pmdFormID <- "de2da940-190b-415b-96dc-e67852bb96d3"
pmdColumns <- "domainid, siteid, plotid, plottype, date, eventid, eventtype, sampling_impractical, datacollected, nestedsubplotareashrubsapling, nestedsubplotarealiana, nestedsubplotareaother"
pmdQuery <- URLencode(glue::glue('SELECT {pmdColumns} FROM "{pmdFormID}" ORDER BY _record_id', .sep = " "))
pmdFulcrum <- get.fulcrum.data(apiToken = Sys.getenv('FULCRUM_PAT'), sql = pmdQuery)

pmdFulcrum <- pmdFulcrum %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::rename(
    domainID = domainid,
    siteID = siteid,
    plotID = plotid,
    plotType = plottype,
    eventID = eventid,
    eventType = eventtype,
    dataCollected = datacollected,
    nestedSubplotAreaShrubSapling = nestedsubplotareashrubsapling,
    nestedSubplotAreaLiana = nestedsubplotarealiana,
    nestedSubplotAreaOther = nestedsubplotareaother
  ) %>%
  dplyr::filter(!sampling_impractical %in% c("Logistical", "Extreme weather")) %>%
  dplyr::select(-sampling_impractical) %>%
  dplyr::distinct()


##  Bind Portal data and Fulcrum data to one dataframe
#   Remove Portal records that are still in Fulcrum
fulcrumPlots <- unique(pmdFulcrum$plotID)
fulcrumEvents <- unique(pmdFulcrum$eventID)
allPmdDF <- allPmdDF %>%
  dplyr::filter(!plotID %in% fulcrumPlots & !eventID %in% fulcrumEvents)

#   Bind Fulcrum and Portal data together, select latest record for plotID
allPmdDF <- allPmdDF %>%
  dplyr::bind_rows(pmdFulcrum) %>%
  dplyr::group_by(plotID) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(domainID, siteID, plotID)

saveRDS(allPmdDF, file = "vst_pmdFulcrumAndPortal_20210819.RDS")
allPmdDF <- readRDS("vst_pmdFulcrumAndPortal_20210819.RDS")


### Identify records with same plotID and eventID; check to see if dates are same for dupes
plotEventDupes <- allPmdDF %>%
  dplyr::group_by(plotID, eventID) %>%
  dplyr::summarise(
    count = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(count > 1)

dupeAllPmdDF <- allPmdDF %>%
  dplyr::filter(plotID %in% plotEventDupes$plotID) %>%
  dplyr::filter(eventID %in% plotEventDupes$eventID)

saveRDS(dupeAllPmdDF, file = "dupePlotEventPmdDF.RDS")

#---> 'date' all identical for dupes but values in one of the nestedSubplotArea fields conflict


##  For plotID x eventID dupes, manually set conflicting nestedSubplotArea values to NULL in output dataset and delete dupe
#--> List of plotID x eventID to investigate comes from 'dupeAllPmdDF'
write.csv(allPmdDF, file = "vst_pmd_nestedSubplotArea.csv", row.names = FALSE)

#   Read back in cleaned dataset to inspect
allPmdDF <- read.csv("vst_pmd_nestedSubplotArea.csv", header = TRUE)



### Next step: Review values for towerSubset at each site and update 'noneSelected' with value from nstPmdDF when allTowerPlots were measured 

write.csv(nstPmdDF, file = "vst_nonWoody_pmdDF_20210819.csv", row.names = FALSE, fileEncoding = "UTF-8")





