### nestedSubplotAreaFerns in TOS Spatial Data Fulcrum app: Initially populated this field with value from
### nestedSubplotAreaOther; however, this creates problems for plots that do not have ferns but DO have
### 'other' growth forms like cacti (e.g., D14 SRER). 

### Goal: Identify all plots that have 'fern' growth form in them using vst_non-woody Portal data,
### then anti-join on the plotIDs from TOS Spatial Data to get those plotIDs that should have 
### nestedSubplotAreaFerns set to 'noneSelected'

#   Load libraries
library(neonUtilities)
library(dplyr)
library(httr)
library(jsonlite)
library(glue)



### Retrieve all plotIDs from TOS Spatial Data Fulcrum app with potentially incorrect nestedSubplotAreaFerns values
source(file = "plant_tools/get_fulcrum_pageLim.R")

#   Construct TOS Spatial Data query
spatialQuery <- URLencode(glue::glue('SELECT _record_id, domainid, siteid, plotid, plottype, subtype, nestedsubplotareaferns, nestedsubplotareaother FROM "eb10727b-bb4d-4778-9a74-8ee03465417e" WHERE', "(plottype='distributed' OR plottype='tower') AND subtype='basePlot' AND NOT nestedsubplotareaferns='noneSelected' ORDER BY _record_id", .sep = " "))

#   Retrieve TOS Spatial Data and arrange
spatialDF <- get_fulcrum_pageLim(apiToken = Sys.getenv('FULCRUM_PAT'), sql = spatialQuery)

spatialDF <- spatialDF %>%
  dplyr::arrange(domainid, siteid, plotid)

#   Identify all sites with potential problems
theSites <- unique(spatialDF$siteid)



### Retrieve NEON vst_non-woody data for all sites with potentially incorrect nestedSubplotAreaFerns
nwDF <- neonUtilities::loadByProduct(
  dpID = "DP1.10098.001",
  site = theSites,
  package = "basic",
  tabl = "vst_non-woody",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

nwDF <- nwDF$`vst_non-woody`



### Identify unique plotIDs with ferns and anti-join with spatial data to find plotIDs with no ferns but 
### that have nestedSubplotAreaFerns != 'noneSelected'
#   Identify plots with ferns
fernPlotsDF <- nwDF %>%
  dplyr::filter(growthForm == "fern") %>%
  dplyr::select(plotID) %>%
  dplyr::distinct()

#   Anti-join with spatial data and set nestedSubplotAreaFerns to 'noneSelected'
nestedUpdate <- spatialDF %>%
  dplyr::anti_join(fernPlotsDF, by = c("plotid" = "plotID")) %>%
  dplyr::mutate(nestedsubplotareaferns = 'noneSelected')

#   Write out results to .csv
write.csv(
  nestedUpdate %>% dplyr::select(`_record_id`, nestedsubplotareaferns),
  file = "vst_misc/vst_nestedSubFernsUpdate_20230216.csv",
  row.names = FALSE
)



### Make a summary of 'other' growth forms in plots that do not have ferns but that did have a nestedSubplotAreaOther value; expect to see something for at least one 'other' growth form. Want to update nestedSubplotAreaOther separately to 'noneSelected' for plotIDs that actually have no 'other' growth forms but currently do have a value for nestedSubplotAreaOther (e.g., BART_010?)

#   Identify plots with no ferns but that do have nestedSubplotAreaOther != 'noneSelected' in TOS Spatial Data
thePlots <- nestedUpdate$plotid

#   Count 'other' individuals by growthForm in plots with no ferns
noFernSummary <- nwDF %>%
  dplyr::filter(plotID %in% thePlots) %>%
  dplyr::group_by(domainID, siteID, plotID, growthForm) %>%
  dplyr::summarise(
    stemCount = n(),
    .groups = "drop"
  ) 

#   Identify plots with 'other' growth forms and no ferns
otherPlots <- unique(noFernSummary$plotID)

#   Identify plots with no ferns AND no 'other' growth forms
noFernOther <- dplyr::setdiff(thePlots, otherPlots)

#   For plots with no ferns and no 'other', set nestedSubplotAreaOther to 'noneSelected'
otherUpdate <- spatialDF %>%
  dplyr::filter(plotid %in% noFernOther) %>%
  dplyr::mutate(nestedsubplotareaother = 'noneSelected')

#   Write out results to .csv
write.csv(
  otherUpdate %>% dplyr::select(`_record_id`, nestedsubplotareaother),
  file = "vst_misc/vst_nestedSubOtherUpdate_20230216.csv",
  row.names = FALSE
)






#--> Strategy for L0 edits in vst_perplotperyear: Use fernPlotsDF list of plotIDs to identify all plotIDs that should have ferns in them. Download vst_perplotperyear for all time, then filter to only those plotIDs with ferns. For each plotID x eventID combination, populate nestedSubplotAreaFerns and totalSampledAreaFerns from nestedSubplotAreaOther and totalSampledAreaOther. This should account for the potential for nestedSubplotAreaOther to change through time if stem densities changed.



