### VST Mapping and Tagging clean-up 2021 ####
### Author: Courtney Meier (cmeier@BattelleEcology.org)
##  Goals:
#   Keep latest Fulcrum and L0 records when individualID is duplicated
#   Remove M&T Fulcrum records with vst_apparentindividual.tagStatus == 'tagRemoved|removed'


##  Required output:
#   List of Fulcrum `_record_id` values to be deleted from Fulcrum AND L0
#   List of Fulcrum `_record_id` values to be deleted from Fulcrum only


##  Process:
#   From Fulcrum data: Identify duplicates by individualID and keep most recent; use anti_join to identify fulcrumIDs for records that should be deleted from both Fulcrum and L0.
#   In L0: Download records with tagStatus == 'tagRemoved|removed' and generate list of unique individualIDs and M&T fulcrumIDs that should be deleted from Fulcrum ONLY.



### Load required libraries and functions
library(plyr)
library(dplyr)
library(glue)
library(httr)
library(jsonlite)
library(restR)
library(stringr)
source(file = "plant_tools/get_fulcrum_pageLim.R")



### Retrieve Fulcrum Mapping & Tagging data
# #   Define SQL query to retrieve all data
# mtQuery <- URLencode('SELECT _record_id, load_status, date, yearboutbegan, individualid, vstid, taxonid, domainid, siteid, plotid, subplotid, nestedsubplotid, pointid, stemazimuth, stemdistance, recordtype, cfconlytag FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae" ORDER BY _record_id')
# 
# #   Get Fulcrum data
# mtDF <- get_fulcrum_pageLim(apiToken = Sys.getenv('FULCRUM_PAT'), sql = mtQuery)
# 
# #   Save as RDS to avoid re-retrieval
# saveRDS(mtDF, file = "~/Box/qaqcNeonData/vst_tableCleanup/vst_fulcrum_mapTag_20210909.RDS")

#   Read in complete Fulcrum M&T data retrieved 2021-09-09
mtDF <- readRDS(file = "~/Box/qaqcNeonData/vst_tableCleanup/vst_fulcrum_mapTag_20210909.RDS")
mtDF$date <- as.Date(mtDF$date)


### Retrieve NEON L0 Apparent Individual records with tagStatus == 'tagRemoved|removed'
# aiDF <- restR::get.os.l0.by.query(
#   stack = 'prod',
#   tab = 'DP0.10098.001:vst_apparentindividual_in',
#   fieldDataSearchStrings = c("removed", "tagRemoved"),
#   fieldName = 'tagStatus',
#   format_for_L0_editor = TRUE
# )
# 
# #   Save as RDS to avoid re-retrieval
# saveRDS(aiDF, file = "~/Box/qaqcNeonData/vst_tableCleanup/vst_L0_AppIndiv_tagStatFil_20210909.RDS")

#   Read in AI L0 data filtered by tagStatus per restR call above
aiDF <- readRDS(file = "~/Box/qaqcNeonData/vst_tableCleanup/vst_L0_AppIndiv_tagStatFil_20210909.RDS")

#   Retain needed AI columns
aiDF <- aiDF %>%
  dplyr::select(
    fulcrumID,
    plotID,
    eventID,
    startDate,
    individualID,
    tempStemID,
    tagStatus,
    plantStatus,
    growthForm
  ) %>%
  dplyr::arrange(plotID, individualID)



### Identify M&T fulcrumIDs that should be deleted from L0
#   Identify records to keep
mtDF_fil <- mtDF %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(date == max(date))

#   Identify records to delete from L0
l0Delete <- mtDF %>%
  dplyr::anti_join(mtDF_fil)

write.csv(l0Delete, file = "vst_fulcrum_deletes/vst_MT_deleteL0_20210910.csv", row.names = FALSE)



### Identify M&T fulcrumIDs that should be deleted from Fulcrum only
#   Generate list of individualIDs in AI that have tagStatus=='tagRemoved|removed'
aiRemove <- aiDF %>%
  dplyr::distinct(individualID)

#   Generate DF of Fulcrum M&T records to keep
mtDF_fulcrum <- mtDF %>%
  dplyr::group_by(individualid) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!individualid %in% aiRemove$individualID)

#   Generate DF of Fulcrum M&T records to delete
fulcrumDelete <- mtDF %>%
  dplyr::anti_join(mtDF_fulcrum)
  
write.csv(fulcrumDelete, file = "vst_fulcrum_deletes/vst_MT_deleteFulcrum_20210910.csv", row.names = FALSE)


