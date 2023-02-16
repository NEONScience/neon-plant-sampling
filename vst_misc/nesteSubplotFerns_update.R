### nestedSubplotAreaFerns in TOS Spatial Data Fulcrum app: Initially populated this field with value from
### nestedSubplotAreaOther; however, this creates problems for plots that do not have ferns but DO have
### 'other' growth forms like cacti (e.g., D14 SRER). 

### Goal: Identify all plots that have 'fern' growth form in them using vst_apparentindividual Portal data,
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
spatialQuery <- URLencode(glue::glue('SELECT _record_id, domainid, siteid, plotid, plottype, subtype, nestedsubplotareaferns FROM "eb10727b-bb4d-4778-9a74-8ee03465417e" WHERE', "(plottype='distributed' OR plottype='tower') AND subtype='basePlot' AND NOT nestedsubplotareaferns='noneSelected' ORDER BY _record_id", .sep = " "))

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



